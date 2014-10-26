{-# LANGUAGE OverloadedStrings, DataKinds #-}

module LambdaCube.Font.Atlas
       ( createFontAtlas
       , defaultOptions
       , buildTextMesh
       , defaultTextStyle
       , renderCharacter
       , getTextureData
       , FontAtlas(..)
       , FontAtlasOptions(..)
       , FontRenderer(..)
       , TextStyle(..)
       ) where

import Control.Monad
import Data.Vect
import Data.List
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Trie as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import GHC.Int
import Graphics.Text.TrueType
import LambdaCube.GL
import LambdaCube.GL.Mesh

import LambdaCube.Font.Common

data ISize = ISize Int Int deriving Show

data IRect = IRect Int Int Int Int deriving Show

data AtlasTree = Tree IRect AtlasNode

data AtlasNode = Split AtlasTree AtlasTree | Taken | Free

-- | A dynamic font atlas.  Given a font and a rendering method, it manages the resources needed to bake letters on a texture on
-- demand and create the geometry to render text using this texture.
data FontAtlas =
    FontAtlas
    { atlasRenderer :: Renderer                         -- ^ The rendering pipeline used to bake glyphs.
    , atlasIndex :: IORef (IntMap CharacterDescriptor)  -- ^ An index of the characters already present in the atlas.
    , atlasGlyphs :: IORef (IntMap GlyphDescriptor)     -- ^ An index of the glyphs already rendered to the atlas.
    , atlasTree :: IORef AtlasTree                      -- ^ Data structure to track the free space on the texture.
    , atlasFont :: Font                                 -- ^ The font associated with this atlas.
    , atlasFontRenderer :: FontRenderer                 -- ^ The font renderer for the chosen rendering method.
    , atlasOptions :: FontAtlasOptions                  -- ^ Additional options specified during creation.
    }

-- | Options specified during atlas creation.
data FontAtlasOptions =
    FontAtlasOptions
    { atlasSize :: Int           -- ^ The resolution of the atlas texture.
    , atlasLetterScale :: Int    -- ^ The number of atlas pixels per em unit.
    , atlasLetterPadding :: Int  -- ^ The extra padding around each entry in the atlas.  Distance field renderers use this value
                                 -- to scale the blending width.  Extra padding might be needed when mipmapping is used.
    }

-- | A font renderer to be used for baking glyphs into an atlas.  Predefined renderers can be found in the
-- "LambdaCube.Font.SimpleDistanceField" and "LambdaCube.Font.CompositeDistanceField" modules.
data FontRenderer =
    FontRenderer
    { frPipeline :: FontAtlasOptions -> GPOutput SingleOutput  -- ^ The rendering pipeline whose top-level target will be the
                                                               -- atlas itself.  The output sampler must be called @"atlas"@.
    , frClearSurface :: Renderer -> IO ()                      -- ^ The action to clear the atlas after creating the pipeline.
    , frBakeGlyph :: FontAtlas -> OutlineCurves -> Vec2 -> Vec2 -> IO ()  -- ^ The action to bake a glyph to the atlas.
    }

-- | Options for preparing a text mesh.
data TextStyle =
    TextStyle
    { textLetterSpacing :: Float  -- ^ Extra displacement applied to the advance values, given in em units.  Negative values allowed.
    , textLineHeight :: Float     -- ^ The distance between the baselines of consecutive lines, given in em units.
    }

-- | Descriptor for a single character.
data CharacterDescriptor =
    CharacterDescriptor
    { cdGlyphs :: [(Mat3, Int)]  -- ^ The glyphs making up the character along with the necessary transformation.
    , cdAdvance :: Float         -- ^ Amount to advance the current point with in em units.
    }

-- | Descriptor for a single glyph.
data GlyphDescriptor =
    GlyphDescriptor
    { cdUVBounds :: Vec4    -- ^ Bounds on the atlas: xMin, yMin, xMax, yMax.
    , cdQuadBounds :: Vec4  -- ^ Quad bounds relative to the current point in em units.
    }

-- | Default options for the atlas.
defaultOptions :: FontAtlasOptions
defaultOptions = FontAtlasOptions
    { atlasSize = 1024
    , atlasLetterScale = 64
    , atlasLetterPadding = 2
    }

-- | Default text style.
defaultTextStyle :: TextStyle
defaultTextStyle = TextStyle
    { textLetterSpacing = 0
    , textLineHeight = 1.25
    }

-- | Create an atlas from a font, a rendering method, and some additional options.
createFontAtlas :: Font -> FontRenderer -> FontAtlasOptions -> IO FontAtlas
createFontAtlas font fontRenderer options = do
    let size = atlasSize options
        textureSize = V2 (fromIntegral size) (fromIntegral size)
    renderer <- compileRenderer (frPipeline fontRenderer options)
    setScreenSize renderer (fromIntegral size) (fromIntegral size)
    frClearSurface fontRenderer renderer
    let uniformMap = uniformSetter renderer

    atlasIndexRef <- newIORef IM.empty
    atlasGlyphsRef <- newIORef IM.empty
    atlasTreeRef <- newIORef (emptyAtlasTree size)
    return $ FontAtlas
        { atlasRenderer = renderer
        , atlasIndex = atlasIndexRef
        , atlasGlyphs = atlasGlyphsRef
        , atlasTree = atlasTreeRef
        , atlasFont = font
        , atlasFontRenderer = fontRenderer
        , atlasOptions = options
        }

-- | Get the reference to the texture the atlas is managing.  This reference can be used to provide input for another rendering
-- pipeline.
getTextureData :: FontAtlas -> TextureData
getTextureData atlas = textureData
  where
    Just textureData = T.lookup "atlas" (samplerOutput (atlasRenderer atlas))

-- | Render a character to the atlas.  Returns 'True' in case of success.  The main use of this function is to render the
-- necessary letters ahead of time so displaying text doesn't cause huge frame skips.
renderCharacter :: FontAtlas -> Char -> IO Bool
renderCharacter atlas @ FontAtlas { atlasFont = font } character = do
    index <- readIORef (atlasIndex atlas)
    if IM.member characterIndex index then return True else do
        glyphSuccesses <- V.forM glyphs $ \(_, glyphIndex, rawCurves) -> renderGlyph atlas glyphIndex rawCurves
        let success = V.and glyphSuccesses
        when success $ modifyIORef (atlasIndex atlas) (IM.insert characterIndex newDescriptor)
        return success
  where
    characterIndex = fromEnum character
    newDescriptor = CharacterDescriptor
        { cdGlyphs = map processTransforms (V.toList glyphs)
        , cdAdvance = advance / emSize
        }
    (advance, glyphs) = getCharacterGlyphsAndMetrics font character
    emSize = fromIntegral (unitsPerEm font)
    processTransforms (scales, glyphIndex, _) = (transform, glyphIndex)
      where
        transform = foldl' composeScales idmtx scales
        composeScales mtx (CompositeScaling a b c d e f) = mtx' .*. mtx
          where
            mtx' = Mat3 (Vec3 a' b' 0) (Vec3 c' d' 0) (Vec3 e' f' 1)
            a' = toFloat a
            b' = toFloat b
            c' = toFloat c
            d' = toFloat d
            e' = fromIntegral e * scaler a' c' / emSize
            f' = fromIntegral f * scaler c' d' / emSize
            toFloat x = fromIntegral x / 0x4000
            scaler v1 v2
                | abs (abs v1 - abs v2) <= (33 / 65536 :: Float) = 2 * vf
                | otherwise = vf
              where
                vf = max (abs v1) (abs v2)

renderGlyph :: FontAtlas -> Int -> [UV.Vector (Int16, Int16)] -> IO Bool
renderGlyph atlas @ FontAtlas { atlasFont = font, atlasOptions = options } glyphIndex rawCurves = do
    glyphs <- readIORef (atlasGlyphs atlas)
    if IM.member glyphIndex glyphs then return True else bakeGlyph
  where
    bakeGlyph = do
        let emSize = fromIntegral (unitsPerEm font)
            letterScale = fromIntegral (atlasLetterScale options)
            boundingBox [] = (0, 0, 0, 0)
            boundingBox vs = (min xs, min ys, max xs, max ys)
              where
                xs = map fst vs
                ys = map snd vs
                min = fromIntegral . minimum
                max = fromIntegral . maximum
            (xMin, yMin, xMax, yMax) = boundingBox (UV.toList =<< rawCurves)
            padding = atlasLetterPadding options
            glyphRectSize = ISize (toInt (xMax - xMin) + 1 + padding * 2) (toInt (yMax - yMin) + 1 + padding * 2)
              where
                toInt v = ceiling (v * letterScale / emSize)
        tree <- readIORef (atlasTree atlas)
        let (tree', newRect) = addRectangle tree glyphRectSize
        case newRect of
            Nothing -> return False
            Just (IRect cx cy cw ch) -> do
                let texelScale = 2 / fromIntegral (atlasSize options)
                    toUV coord = (fromIntegral coord - 0.5) * (texelScale / 2)
                    toClip coord = toUV coord * 2 - 1
                    curves = map (tessellateBezierCurves . map toVec2 . UV.toList) rawCurves
                    toVec2 (x, y) = Vec2 (fromIntegral x) (fromIntegral y)
                frBakeGlyph (atlasFontRenderer atlas) atlas curves (Vec2 xMin yMin) (Vec2 (toClip cx) (toClip cy))

                writeIORef (atlasTree atlas) tree'
                let newDescriptor = GlyphDescriptor
                        { cdUVBounds = Vec4 (toUV cx) (toUV cy) (toUV (cx + cw)) (toUV (cy + ch))
                        , cdQuadBounds = Vec4 quadX quadY (quadX + fromIntegral cw / letterScale) (quadY + fromIntegral ch / letterScale)
                        }
                    quadX = xMin / emSize - fromIntegral padding / letterScale
                    quadY = yMin / emSize - fromIntegral padding / letterScale
                modifyIORef (atlasGlyphs atlas) (IM.insert glyphIndex newDescriptor)

                return True

tessellateBezierCurves :: [Vec2] -> [Vec2]
tessellateBezierCurves vs = makeBezierSection =<< extractControlPoints vs
  where
    extractControlPoints (v1:v2:vs@(v3:_)) = (v1,v2,v3) : extractControlPoints vs
    extractControlPoints _ = []
    makeBezierSection (v1, v2, v3) = go 0 1 []
      where
        go ta tb rest
            | turnMeasure < 0.997 = go ta tm (go tm tb rest)
            | otherwise = pm : pb : rest
          where
            turnMeasure = normalize (pm &- pa) &. normalize (pb &- pm)
            tm = (ta + tb) * 0.5
            pa = evalBezier ta
            pb = evalBezier tb
            pm = evalBezier tm
        evalBezier t = ((t*t) *& v3) &+ ((2*t*t') *& v2) &+ ((t'*t') *& v1)
          where
            t' = 1-t

-- | Build a mesh from a string and render all the characters needed that aren't yet present on the atlas.  Every @'\n'@ character
-- starts a new line.
buildTextMesh :: FontAtlas -> TextStyle -> String -> IO Mesh
buildTextMesh atlas style string = do
    mapM_ (renderCharacter atlas) string
    index <- readIORef (atlasIndex atlas)
    glyphs <- readIORef (atlasGlyphs atlas)
    let letterSpacing = textLetterSpacing style
        lineHeight = textLineHeight style
        (rects, uvs) = go string (Vec2 0 0) [] []
          where
            go [] point rects uvs = (reverse rects, reverse uvs)
            go ('\n':chars) (Vec2 _ pointY) rects uvs = go chars (Vec2 0 pointY') rects uvs
              where
                pointY' = pointY - lineHeight
            go (char:chars) point@(Vec2 pointX pointY) rects uvs = case IM.lookup characterIndex index of
                Nothing -> go chars point rects uvs
                Just descriptor -> go chars (Vec2 pointX' pointY') (newRects ++ rects) (newUVs ++ uvs)
                  where
                    pointX' = pointX + cdAdvance descriptor + letterSpacing
                    pointY' = pointY
                    (newRects, newUVs) = unzip (map makeGlyphQuad (cdGlyphs descriptor))
                    makeGlyphQuad (transform, glyphIndex) = ((transform, rect), uv)
                      where
                        glyph = glyphs IM.! glyphIndex
                        rect = cdQuadBounds glyph &+ Vec4 pointX pointY pointX pointY
                        uv = cdUVBounds glyph
              where
                characterIndex = fromEnum char
        textMesh = Mesh
            { mAttributes   = T.fromList [("position", makeQuads rects), ("uv", makeQuads (addIds uvs))]
            , mPrimitive    = P_Triangles
            , mGPUData      = Nothing
            }
        addIds = map ((,) (idmtx :: Mat3))
        makeQuads = A_V2F . SV.fromList . concatMap makeQuad
        makeQuad (mat, Vec4 x1 y1 x2 y2) = map transform [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          where
            transform (V2 x y) = V2 x' y'
              where
                Vec3 x' y' _ = Vec3 x y 1 .* mat
    return textMesh

emptyAtlasTree :: Int -> AtlasTree
emptyAtlasTree size = Tree (IRect 0 0 size size) Free

addRectangle :: AtlasTree -> ISize -> (AtlasTree, Maybe IRect)
addRectangle tree@(Tree tRect@(IRect tx ty tw th) node) rSize@(ISize w h) = case node of
    Split lTree rTree -> case lRect of
        Just r -> (Tree tRect (Split lTreeNew rTree), lRect)
        Nothing -> (Tree tRect (Split lTree rTreeNew), rRect)
      where
        (lTreeNew, lRect) = addRectangle lTree rSize
        (rTreeNew, rRect) = addRectangle rTree rSize
    Taken -> (tree, Nothing)
    Free
        | w == tw && h == th -> (Tree tRect Taken, Just tRect)
        | w <= tw && h <= th -> (Tree tRect (Split lTreeNew rTreeNew), rectNew)
        | otherwise -> (tree, Nothing)
      where
        dw = tw - w
        dh = th - h
        (lTreeNew, rectNew) = addRectangle (Tree lRect Free) rSize
        rTreeNew = Tree rRect Free
        (lRect, rRect) = if dw > dh
            then (IRect tx ty w th, IRect (tx + w) ty dw th)
            else (IRect tx ty tw h, IRect tx (ty + h) tw dh)
