{-# LANGUAGE ParallelListComp, OverloadedStrings, TypeOperators, DataKinds #-}

module LambdaCube.Font.SimpleDistanceField (fontRenderer) where

import Data.ByteString (ByteString)
import Data.Vect
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import Graphics.Text.TrueType
import LambdaCube.GL
import LambdaCube.GL.Mesh

import LambdaCube.Font.Atlas
import LambdaCube.Font.Common

type OutlineVertices = [([Vec2], [Vec2], [Vec2])]

type FillTriangles = [Vec2]

type OutlineTriangles = [(Vec2, Float)]

-- | A font renderer that uses a basic distance field to create curves that remain sharp even when magnified.  The type of the
-- exported atlas is @Float Red@.
fontRenderer :: FontRenderer
fontRenderer = FontRenderer pipeline clearSurface bakeGlyph

pipeline :: FontAtlasOptions -> GPOutput SingleOutput
pipeline options = makeSamplerOut [PrjFrameBuffer "" tix0 bakePipeline]
  where
    makeSamplerOut = SamplerOut "atlas" . Sampler LinearFilter Repeat . Texture (Texture2D (Float Red) n1) textureSize NoMip
    size = atlasSize options
    textureSize = V2 (fromIntegral size) (fromIntegral size)

bakeGlyph :: FontAtlas -> OutlineCurves -> Vec2 -> Vec2 -> IO ()
bakeGlyph atlas @ FontAtlas { atlasFont = font, atlasOptions = options } curves bakeOffset atlasOffset = do
    outlineOuterMesh <- compileMesh (makeOutlineMesh outlineOuterTriangles)
    outlineInnerMesh <- compileMesh (makeOutlineMesh outlineInnerTriangles)
    fillMesh <- compileMesh (makeFillMesh fillTriangles)

    outlineOuterObject <- addMesh renderer "outlineOuterStream" outlineOuterMesh []
    outlineInnerObject <- addMesh renderer "outlineInnerStream" outlineInnerMesh []
    fillObject <- addMesh renderer "fillStream" fillMesh []

    uniformM33F "charToAtlasMatrix" uniformMap charToAtlasMatrix

    render renderer

    removeObject renderer outlineOuterObject
    removeObject renderer outlineInnerObject
    removeObject renderer fillObject
  where
    outlineVertices = letterOutlineVertices outlineThickness curves
    fillTriangles = letterFillTriangles outlineVertices
    outlineOuterTriangles = letterOutlineOuterTriangles outlineVertices
    outlineInnerTriangles = letterOutlineInnerTriangles outlineVertices

    outlineThickness = padding * emSize / letterScale
    renderer = atlasRenderer atlas
    uniformMap = uniformSetter renderer
    padding = fromIntegral (atlasLetterPadding options)
    letterScale = fromIntegral (atlasLetterScale options)
    emSize = fromIntegral (unitsPerEm font)

    charToAtlasMatrix = V3 (V3 scale 0 0) (V3 0 scale 0) (V3 xt yt 1)
    texelScale = 2 / fromIntegral (atlasSize options)
    scale = texelScale * letterScale / emSize
    padOffset = padding * texelScale
    Vec2 xt yt = Vec2 padOffset padOffset &- bakeOffset &* scale &+ atlasOffset

clearSurface :: Renderer -> IO ()
clearSurface renderer = do
    uniformM33F "charToAtlasMatrix" uniforms (V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1))
    clearMeshInner <- compileMesh blackQuad
    clearMeshOuter <- compileMesh blackQuad
    clearObjectInner <- addMesh renderer "outlineInnerStream" clearMeshInner []
    clearObjectOuter <- addMesh renderer "outlineOuterStream" clearMeshOuter []
    render renderer
    removeObject renderer clearObjectInner
    removeObject renderer clearObjectOuter
  where
    uniforms = uniformSetter renderer
    blackQuad = Mesh
        { mAttributes   = T.fromList
                          [ ("position", A_V2F (SV.fromList [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 1 1, V2 (-1) 1, V2 (-1) (-1)]))
                          , ("distance", A_Float (SV.fromList [0, 0, 0, 0, 0, 0]))
                          ]
        , mPrimitive    = P_Triangles
        , mGPUData      = Nothing
        }

bakePipeline :: Exp Obj (FrameBuffer 1 Float)
bakePipeline = (outlineInner . outlineOuter . fill) emptyBuffer
  where
    outlineOuter = Accumulate (outlineFragmentCtx Max) PassAll outlineFragmentShader (outlineFragmentStream "outlineOuterStream")
    outlineInner = Accumulate (outlineFragmentCtx Min) PassAll outlineFragmentShader (outlineFragmentStream "outlineInnerStream")
    fill = Accumulate fillFragmentCtx PassAll fillFragmentShader fillFragmentStream
    emptyBuffer = FrameBuffer (UnclearedImage n1 :. ZT)
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

    outlineVertices slotName = Fetch slotName Triangles (IV2F "position", IFloat "distance")
    outlineFragmentStream = Rasterize rasterCtx . Transform outlineVertexShader . outlineVertices
    outlineFragmentCtx op = AccumulationContext Nothing (ColorOp (outlineBlending op) True :. ZT)
    outlineBlending op = Blend (op, op) ((One, One), (One, One)) zero'

    outlineVertexShader attr = VertexOut (transformVertex pos) (floatV 1) ZT (Smooth dist :. ZT)
      where
        (pos, dist) = untup2 attr

    outlineFragmentShader distance = FragmentOut (distance :. ZT)

    fillVertices = Fetch "fillStream" Triangles (IV2F "position")
    fillFragmentCtx = AccumulationContext Nothing (ColorOp fillBlending True :. ZT)
    fillBlending = Blend (FuncSubtract, FuncSubtract) ((One, One), (One, One)) zero'
    fillFragmentStream = Rasterize rasterCtx fillPrimitiveStream
    fillPrimitiveStream = Transform fillVertexShader fillVertices

    fillVertexShader pos = VertexOut (transformVertex pos) (floatV 1) ZT ZT

    fillFragmentShader _ = FragmentOut (floatF 1 :. ZT)

    transformVertex vertex = v3v4 (charToAtlas @*. v2v3 vertex)
      where
        charToAtlas = Uni (IM33F "charToAtlasMatrix") :: Exp V M33F

letterOutlineVertices :: Float -> OutlineCurves -> OutlineVertices
letterOutlineVertices outlineThickness = map (offsetEdges . removeCollapsedEdges . duplicateLast)
  where
    miterLimit = 4

    duplicateLast xs = last xs : xs

    removeCollapsedEdges (v1:v2:vs) =
        if lensqr (v1 &- v2) > outlineThickness * 0.001
        then v1 : removeCollapsedEdges (v2:vs)
        else removeCollapsedEdges (v1:vs)
    removeCollapsedEdges vs = vs

    offsetEdges vertices = (innerVertices, vertices, outerVertices)
      where
        outerVertices = [v &- o &* outlineThickness | v <- vertices | o <- cycle offsets]
        innerVertices = [v &+ o &* outlineThickness | v <- vertices | o <- cycle offsets]
        edges = [v2 &- v1 | v1 <- vertices | v2 <- tail vertices]
        edgeNormals = map (normalize . turn) edges
        vertexNormals = [normalize (n1 &+ n2) | n1 <- last edgeNormals : edgeNormals | n2 <- edgeNormals]
        offsets = [nv &* min miterLimit (recip (nv &. ne)) | ne <- edgeNormals | nv <- vertexNormals]

letterFillTriangles :: OutlineVertices -> FillTriangles
letterFillTriangles = concatMap makePoly
  where
    makePoly (_, v:vs, _) = concat (zipWith makeTri (init vs) (tail vs))
      where
        makeTri v1 v2 = [v, v1, v2]

letterOutlineOuterTriangles :: OutlineVertices -> OutlineTriangles
letterOutlineOuterTriangles = concatMap makeQuads
  where
    makeQuads (i1:i2:is, m1:m2:ms, o1:o2:os) = (m1, m) : (o1, o) : (o2, o) : (o2, o) : (m2, m) : (m1, m) : makeQuads (i2:is, m2:ms, o2:os)
    makeQuads _ = []
    m = 0.5
    o = 0

letterOutlineInnerTriangles :: OutlineVertices -> OutlineTriangles
letterOutlineInnerTriangles = concatMap makeQuads
  where
    makeQuads (i1:i2:is, m1:m2:ms, o1:o2:os) = (i1, i) : (m1, m) : (m2, m) : (m2, m) : (i2, i) : (i1, i) : makeQuads (i2:is, m2:ms, o2:os)
    makeQuads _ = []
    i = 1
    m = 0.5

makeFillMesh :: FillTriangles -> Mesh
makeFillMesh fillTriangles = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F $ SV.fromList [V2 x y | Vec2 x y <- fillTriangles])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }

makeOutlineMesh :: OutlineTriangles -> Mesh
makeOutlineMesh outlineTriangles = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F $ SV.fromList [V2 x y | (Vec2 x y, _) <- outlineTriangles])
        , ("distance", A_Float $ SV.fromList [d | (_, d) <- outlineTriangles])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
