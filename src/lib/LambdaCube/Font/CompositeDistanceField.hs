{-# LANGUAGE ParallelListComp, OverloadedStrings, TypeOperators, DataKinds #-}

module LambdaCube.Font.CompositeDistanceField
       ( fontRenderer
       , sampleDistance
       ) where

import Data.ByteString (ByteString)
import Data.List
import Data.Vect
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import Graphics.Text.TrueType
import LambdaCube.GL
import LambdaCube.GL.Mesh

import LambdaCube.Font.Atlas
import LambdaCube.Font.Common

type FillTriangles = [Vec2]

type OutlineTriangles = [[[(Vec2, Vec4)]]]

-- | A font renderer that uses a quad-channel distance field to create curves with sharp corners even when magnified.  The type of
-- the exported atlas is @Float RGBA@.
fontRenderer :: FontRenderer
fontRenderer = FontRenderer pipeline clearSurface bakeGlyph

-- | A fragment shader snippet to reconstruct the distance field.  Takes the texture slot name and the uv coordinates as
-- parameters.
sampleDistance :: ByteString -> Exp F V2F -> Exp F Float
sampleDistance slotName uv = max' (max' (d1 @* dw1) (d2 @* dw2)) (min' d1 d2)
  where
    --test = pack' (V4 ((dw1 @+ step d1) @* floatF 0.5) (step distance) ((dw2 @+ step d2) @* floatF 0.5) (floatF 1))
    dw1 = step' (floatF 0.5) w1
    dw2 = step' (floatF 0.5) w2
    V4 d1 d2 w1 w2 = unpack' (texture' (Sampler LinearFilter Repeat tex) uv)
    tex = TextureSlot slotName (Texture2D (Float RGBA) n1)

pipeline :: FontAtlasOptions -> GPOutput SingleOutput
pipeline options = makeSamplerOut [PrjFrameBuffer "" tix0 (bakePipeline (atlasLetterScale options))]
  where
    makeSamplerOut = SamplerOut "atlas" . Sampler LinearFilter Repeat . Texture (Texture2D (Float RGBA) n1) textureSize NoMip
    size = atlasSize options
    textureSize = V2 (fromIntegral size) (fromIntegral size)

bakeGlyph :: FontAtlas -> OutlineCurves -> Vec2 -> Vec2 -> IO ()
bakeGlyph atlas @ FontAtlas { atlasFont = font, atlasOptions = options } curves bakeOffset atlasOffset = do
    outlineMesh <- compileMesh (makeOutlineMesh outlineTriangles)
    fillMesh <- compileMesh (makeFillMesh fillTriangles)

    outlineObject <- addMesh renderer "outlineStream" outlineMesh []
    fillObject <- addMesh renderer "fillStream" fillMesh []

    uniformM33F "bakeMatrix" uniformMap bakeMatrix
    uniformM33F "blitMatrix" uniformMap blitMatrix

    render renderer

    removeObject renderer outlineObject
    removeObject renderer fillObject
  where
    renderer = atlasRenderer atlas
    uniformMap = uniformSetter renderer

    outlineTriangles = letterOutlineTriangles (outlineThickness * 0.75) curves
    fillTriangles = letterFillTriangles outlineTriangles

    outlineThickness = fromIntegral padding * emSize / letterScale
    padding = atlasLetterPadding options
    letterScale = fromIntegral (atlasLetterScale options)
    emSize = fromIntegral (unitsPerEm font)

    bakeMatrix = V3 (V3 bakeScale 0 0) (V3 0 bakeScale 0) (V3 (bakeX - 1) (bakeY - 1) 1)
    bakeScale = 2 / emSize
    bakePad = -outlineThickness
    Vec2 bakeX bakeY = (bakeOffset &+ Vec2 bakePad bakePad) &* (-bakeScale)

    blitMatrix = V3 (V3 blitScale 0 0) (V3 0 blitScale 0) (V3 blitX blitY 1)
    blitScale = 2 / fromIntegral (atlasSize options) * letterScale
    Vec2 blitX blitY = atlasOffset

clearSurface :: Renderer -> IO ()
clearSurface renderer = do
    quadMesh <- compileMesh quad
    clearObject <- addMesh renderer "clearStream" quadMesh []
    render renderer
    addMesh renderer "blitStream" quadMesh []
    removeObject renderer clearObject
  where
    uniforms = uniformSetter renderer
    quad = Mesh
        { mAttributes   = T.fromList [("position", A_V2F (SV.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 1, V2 0 1, V2 0 0]))]
        , mPrimitive    = P_Triangles
        , mGPUData      = Nothing
        }

bakePipeline :: Int -> Exp Obj (FrameBuffer 1 V4F)
bakePipeline letterScale = (blit . clear) emptyBuffer
  where
    letterTexture = Texture (Texture2D (Float RGBA) n1) letterTextureSize NoMip [PrjFrameBuffer "" tix0 letterBakePipeline]
    letterTextureSize = V2 bakeScale bakeScale
    letterBakePipeline = (outline . fill) clearBuffer
    bakeScale = fromIntegral letterScale * 2

    clear = Accumulate clearFragmentCtx PassAll clearFragmentShader clearFragmentStream
    blit = Accumulate blitFragmentCtx PassAll blitFragmentShader blitFragmentStream
    outline = Accumulate outlineFragmentCtx PassAll outlineFragmentShader outlineFragmentStream
    fill = Accumulate fillFragmentCtx PassAll fillFragmentShader fillFragmentStream
    emptyBuffer = FrameBuffer (UnclearedImage n1 :. ZT)
    clearBuffer = FrameBuffer (ColorImage n1 (V4 0 0 0 0) :. ZT)
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
    maxBlending = Blend (Max, Max) ((One, One), (One, One)) zero'

    clearVertices = Fetch "clearStream" Triangles (IV2F "position")
    clearFragmentStream = Rasterize rasterCtx (Transform clearVertexShader clearVertices)
    clearFragmentCtx = AccumulationContext Nothing (ColorOp NoBlending (V4 True True True True) :. ZT)

    clearVertexShader pos = VertexOut (v3v4 (v2v3 pos @* floatV 2 @- floatV 1)) (floatV 1) ZT ZT

    clearFragmentShader _ = FragmentOut (pack' (V4 zero zero zero zero) :. ZT)
      where
        zero = floatF 0

    blitVertices = Fetch "blitStream" Triangles (IV2F "position")
    blitFragmentStream = Rasterize rasterCtx (Transform blitVertexShader blitVertices)
    blitFragmentCtx = AccumulationContext Nothing (ColorOp maxBlending (V4 True True True True) :. ZT)

    blitVertexShader pos = VertexOut (v3v4 (blitMatrix @*. v2v3 pos)) (floatV 1) ZT (Smooth (pos @+ texelScale) :. ZT)
      where
        texelScale = floatV (0.5 / fromIntegral bakeScale)
        blitMatrix = Uni (IM33F "blitMatrix") :: Exp V M33F

    blitFragmentShader uv = FragmentOut (correctedDistances :. ZT)
      where
        correctedDistances = max' p0 (max' spreadOpposite spreadWhite)

        spreadWhite = whiteWeight @* maxWith white
        whiteWeight = max' (floatF 0) (floatF 0.5 @- dw1 @. dw2)
        white p = step' (floatF 3.5) (dot' p ones)

        spreadOpposite = maxWith id @* oppositeWeight
        oppositeWeight = pack' (V4 dw2 dw1 dw2 dw1)
        dw1 = clamp' (d1 @+ w1 @- floatF 1) (floatF 0) (floatF 1)
        dw2 = clamp' (d2 @+ w2 @- floatF 1) (floatF 0) (floatF 1)

        V4 d1 d2 w1 w2 = unpack' p0
        p0 = texture' letterSampler uv
        pu = texture' letterSampler (uv @- pack' (V2 z d))
        pd = texture' letterSampler (uv @+ pack' (V2 z d))
        pl = texture' letterSampler (uv @- pack' (V2 d z))
        pr = texture' letterSampler (uv @+ pack' (V2 d z))

        d = floatF (recip (fromIntegral bakeScale))
        z = floatF 0
        ones = pack' (V4 (floatF 1) (floatF 1) (floatF 1) (floatF 1))
        maxWith f = max' (max' (f pu) (f pd)) (max' (f pl) (f pr))
        letterSampler = Let (Sampler PointFilter Repeat letterTexture) id

    outlineVertices = Fetch "outlineStream" Triangles (IV2F "position", IV4F "distance")
    outlineFragmentStream = Rasterize rasterCtx (Transform outlineVertexShader outlineVertices)
    outlineFragmentCtx = AccumulationContext Nothing (ColorOp maxBlending (V4 True True True True) :. ZT)

    outlineVertexShader attr = VertexOut (transformVertex pos) (floatV 1) ZT (Smooth dist :. ZT)
      where
        (pos, dist) = untup2 attr

    outlineFragmentShader dists = FragmentOut (pack' (V4 dist1 dist2 weight1 weight2) :. ZT)
      where
        scale = floatF 10
        saturate x = clamp' x (floatF 0) (floatF 1)
        dist1 = saturate (channel @* scale @- floatF 0) @* dist
        dist2 = saturate (scale @- channel @* scale) @* dist
        weight1 = saturate rawWeight1
        weight2 = saturate rawWeight2
        V4 dist channel rawWeight1 rawWeight2 = unpack' dists

    fillVertices = Fetch "fillStream" Triangles (IV2F "position")
    fillFragmentCtx = AccumulationContext Nothing (ColorOp fillBlending (V4 True True True True) :. ZT)
    fillBlending = Blend (FuncSubtract, FuncSubtract) ((One, One), (One, One)) zero'
    fillFragmentStream = Rasterize rasterCtx fillPrimitiveStream
    fillPrimitiveStream = Transform fillVertexShader fillVertices

    fillVertexShader pos = VertexOut (transformVertex pos) (floatV 1) ZT ZT

    fillFragmentShader _ = FragmentOut (pack' (V4 one one one one) :. ZT)
      where
        one = floatF 1

    transformVertex vertex = v3v4 (bakeMatrix @*. v2v3 vertex)
      where
        bakeMatrix = Uni (IM33F "bakeMatrix") :: Exp V M33F

letterFillTriangles :: OutlineTriangles -> FillTriangles
letterFillTriangles = concatMap (makeTris . concat . eliminateLoops . map extractInnerVertices)
  where
    eliminateLoops curves = [trim curve is1 is2 | curve <- curves | is1 <- last intersections : intersections | is2 <- intersections]
      where
        trim curve is1 is2 = trimEnd is2 (trimStart is1 curve)
          where
            trimStart Nothing vs = vs
            trimStart (Just (vi1, _, i1)) vs = vi1 : drop i1 vs
            trimEnd Nothing vs = vs
            trimEnd (Just (vi2, i2, _)) vs = take (length vs - i2) vs ++ [vi2]
        searchDepth = 20
        intersections = [findIntersection (take searchDepth (reverse c1)) (take searchDepth c2) | c1 <- curves | c2 <- tail (cycle curves)]
        findIntersection c1@(c11:c12:_) c2@(c21:c22:_) = go c1 1
          where
            go (v11:v12:v1s) i1 = case seek c2 1 of
                Nothing -> go (v12:v1s) (i1 + 1)
                result -> result
              where
                n1 = turn (v12 &- v11)
                sign = signum (n1 &. (head c2 &- v11))
                seek (v21:v22:v2s) i2
                    | (n1 &. (v21 &- v11)) * sign < 0 = seek (v22:v2s) (i2 + 1)
                    | otherwise = if (n2 &. (v12 &- v21)) * (n2 &. (v11 &- v21)) < 0 && abs den > 1e-6
                                  then Just (ixy, i1, i2 + 2)
                                  else Nothing
                  where
                    n2 = turn (v22 &- v21)
                    (ixy, den) = intersection v11 v12 v21 v22
                seek _ _ = Nothing
            go _ _ = Just (if abs den > 1e-6 then ixy else (c11 &+ c21) &* 0.5, 1, 1)
              where
                (ixy, den) = intersection c11 (c11 &+ turn (c12 &- c11)) c21 (c21 &- turn (c22 &- c21))

    extractInnerVertices (_:(v1,_):(v2,_):_:_:_:vs) = v1:v2:extractInnerVertices vs
    extractInnerVertices _ = []
    makeTris (v:vs) = concat (zipWith makeTri vs (tail vs))
      where
        makeTri v1 v2 = [v, v1, v2]

intersection :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> (Vec2, Float)
intersection v11 v12 v21 v22 = (Vec2 ix iy &* recip den, den)
  where
    ix = (x1 * y2 - y1 * x2) * x34 - (x3 * y4 - y3 * x4) * x12
    iy = (x1 * y2 - y1 * x2) * y34 - (x3 * y4 - y3 * x4) * y12
    den = x12 * y34 - y12 * x34
    Vec2 x1 y1 = v11
    Vec2 x2 y2 = v12
    Vec2 x3 y3 = v21
    Vec2 x4 y4 = v22
    Vec2 x12 y12 = v11 &- v12
    Vec2 x34 y34 = v21 &- v22

-- Yes, this is not very efficient due to recalculating everything all the time...
letterOutlineTriangles :: Float -> OutlineCurves -> OutlineTriangles
letterOutlineTriangles outlineThickness letter = map processCurve letter
  where
    processCurve = map extrudeCurve . addTurnMeasures . removeShortCurves . map removeCollapsedEdges . separateCurves . reverse

    removeCollapsedEdges (v1:v2:vs@(_:_)) =
        if len (v1 &- v2) > outlineThickness * 0.5
        then v1 : removeCollapsedEdges (v2:vs)
        else removeCollapsedEdges (v1:vs)
    removeCollapsedEdges vs = vs

    removeShortCurves curves =
        [ extend cur nextKept prevNextKept curRev prevRev next
        | (cur, curRev, nextKept) <- filteredCurves
        | (_, prevRev, prevNextKept) <- last filteredCurves : filteredCurves
        | (next, _, _) <- tail (cycle filteredCurves)
        ]
      where
        extend cur nextKept prevNextKept curRev prevRev next = vFirst : tail (init cur) ++ [vLast]
          where
            vFirst = if prevNextKept then head cur else newFirst
            vLast = if nextKept then last cur else newLast
            newFirst = if abs denFirst > 1e-4 && closeEnough avgFirst ivFirst then ivFirst else avgFirst
            newLast = if abs denLast > 1e-4 && closeEnough avgLast ivLast then ivLast else avgLast
            closeEnough v1 v2 = len (v1 &- v2) < outlineThickness
            avgFirst = (curF1 &+ prevL1) &* 0.5
            avgLast = (curL1 &+ nextF1) &* 0.5
            (ivFirst, denFirst) = intersection curF1 curF2 prevL1 prevL2
            (ivLast, denLast) = intersection curL1 curL2 nextF1 nextF2
            curF1:curF2:_ = cur
            curL1:curL2:_ = curRev
            nextF1:nextF2:_ = next
            prevL1:prevL2:_ = prevRev
        filteredCurves = [(curve, reverse curve, nextKept) | (curve, currentKept, nextKept) <- markedCurves, currentKept]
        keptMarks = map longEnough curves
        markedCurves = zipWith3 (,,) curves keptMarks (tail (cycle keptMarks))
        longEnough vs = sum [len (v2 &- v1) | v1 <- vs | v2 <- tail vs] > outlineThickness

    separateCurves vertices = slice (zip vertices' cornerMarks')
      where
        vertices' = drop (length lastMarks) (cycle vertices)
        cornerMarks' = removeFirstMark (firstMarks ++ lastMarks)
        edges = [v2 &- v1 | v1 <- last vertices : init vertices | v2 <- cycle vertices]
        tangents = map normalize edges
        cornerMarks = [t1 &. t2 < 0.9 | t1 <- tangents | t2 <- tail (cycle tangents)]
        (lastMarks, firstMarks) = span not cornerMarks
        removeFirstMark [] = []
        removeFirstMark (_:ms) = False : ms
        slice [] = []
        slice vcms = case sliceFirst vcms of
            (curve, []) -> [curve]
            (curve, rest) -> curve : slice rest
          where
            sliceFirst [(v,_)] = ([v, head vertices'], [])
            sliceFirst ((v,c):vcm) = case c of
                False -> (v:curve, rest)
                True -> ([v], (v,False):vcm)
              where
                (curve, rest) = sliceFirst vcm

    addTurnMeasures curves =
        [(curve, parity, te1, ts1, te2, ts2) | curve <- rotatedCurves | parity <- parities
                                             | (_, te1) <- tangents | (ts1, te2) <- tail tangents | (ts2, _) <- tail (tail tangents)]
      where
        rotatedCurves = tail curves ++ [head curves]
        count = length curves
        tangents = cycle (map firstLastTangent curves)
        lengths = map length rotatedCurves
        maxLength = maximum lengths
        parities = case count of
            1 -> [if sharpTurn then 2 else 1]
            _ | even count -> cycle [1, 0]
              | otherwise  -> go 1 lengths
          where
            sharpTurn = uncurry (&.) (head tangents) < 0.9
            go p [] = []
            go p (l:ls)
                | l == maxLength = 3 - p : cycle [p, 1 - p]
                | otherwise      = p : go (1 - p) ls
        firstLastTangent vertices = (head tangents, last tangents)
          where
            tangents = [normalize (v2 &- v1) | v1 <- vertices | v2 <- tail vertices]

    extrudeCurve (vs, parity, te1, ts1, te2, ts2) = concat [makeQuad a1 a2 | a1 <- attributes | a2 <- tail attributes]
      where
        turnStart = turnMeasure te1 ts1
        turnEnd = turnMeasure te2 ts2
        turnMeasure te ts = ((ts &. te) - 1) * signum (det (ts, te))
        cwStart = counterWeight turnStart
        cwEnd = counterWeight turnEnd
        counterWeight turn = max (-0.5) (remap turn (-1) 1 0 (-1))

        insetFactorStart = insetFactor turnStart
        insetFactorEnd = insetFactor turnEnd
        insetFactor turn = min 2 (max 0.5 (remap turn 0 2 0.25 2))

        makeQuad (v1,n1,p1,c1) (v2,n2,p2,c2) = [(xy1, df1), (xy2, df2), (xy3, df3), (xy3, df3), (xy4, df4), (xy1, df1)]
          where
            n = turn (normalize (v2 &- v1))
            n1' = n1 &* (outlineThickness / (n1 &. n))
            n2' = n2 &* (outlineThickness / (n2 &. n))
            xy1 = v1 &+ n1'
            xy2 = interpolate c1 (v1 &- n1') xy1
            xy3 = interpolate c2 (v2 &- n2') xy4
            xy4 = v2 &+ n2'
            df1 = Vec4 0 (channel p1) (weight p1) (weight' p1)
            df2 = interpolate c1 (Vec4 1 (channel p1) (weight p1) (weight' p1)) df1
            df3 = interpolate c2 (Vec4 1 (channel p2) (weight p2) (weight' p2)) df4
            df4 = Vec4 0 (channel p2) (weight p2) (weight' p2)
            blendChannel p = min 1 (max 0 (2 - p))
            blendWeight p
                | p < 1 = remap p 0 1 cwStart 1
                | p > 2 = 0
                | True  = remap p 1 2 1 0 ** 2
            blendWeight' p
                | p < 1 = 0
                | p > 2 = remap p 3 2 cwEnd 1
                | True  = remap p 2 1 1 0 ** 2
            channel p
                | parity == 2 = blendChannel p
                | parity == 3 = blendChannel (3 - p)
                | True        = parity
            rawWeight p
                | p < 1 = remap p 0 1 cwStart 1
                | p > 2 = remap p 3 2 cwEnd 1
                | True  = 1
            weight p
                | parity == 2 = blendWeight p
                | parity == 3 = blendWeight' p
                | True        = rawWeight p * parity
            weight' p
                | parity == 2 = blendWeight' p
                | parity == 3 = blendWeight p
                | True        = rawWeight p * (1 - parity)

        attributes = zip4 allVertices allNormals allParams allCuts

        allVertices = vFirst : insetVertices ++ [vLast]
          where
            vFirst = head insetVertices &- head tangents &* outlineThickness
            vLast = last insetVertices &+ last tangents &* outlineThickness
        allNormals = head vertexNormals : vertexNormals ++ [last vertexNormals]
        allParams = 0 : vertexParams ++ [3]
        allCuts = firstCut : replicate (length vertexParams) 0 ++ [lastCut]
          where
            firstCut = if turnStart > 0 && turnStart < 1 then cut te1 ts1 else 0
            lastCut = if turnEnd > 0 && turnEnd < 1 then cut te2 ts2 else 0
            cut te ts = min 1 ((te &. ts) / det (te, ts) * 0.5)

        edges = [v2 &- v1 | v1 <- vs | v2 <- tail vs]
        edgeLengths = map len edges
        curveLength = sum edgeLengths
        startDistance = min (outlineThickness * insetFactorStart) (curveLength * 0.5)
        endDistance = max (curveLength - outlineThickness * insetFactorEnd) (curveLength * 0.5)
        (insetVertices, insetDistances) = unzip (inset vs edges edgeLengths 0)
          where
            inset [v] [] [] d = [(v, d)]
            inset (v:vs) (e:es) (l:ls) d
                | d < startDistance && startDistance < d' = (v, d) : restStart
                | d < endDistance && endDistance < d' = (v, d) : (vEnd, endDistance) : rest
                | otherwise = (v, d) : rest
              where
                d' = d + l
                vStart = v &+ e &* ((startDistance - d) / l)
                vEnd = v &+ e &* ((endDistance - d) / l)
                rest = inset vs es ls d'
                restStart = inset (vStart:vs) ((e &+ v &- vStart):es) ((d' - startDistance):ls) startDistance

        insetEdges = [v2 &- v1 | v1 <- insetVertices | v2 <- tail insetVertices]
        tangents = map normalize insetEdges
        edgeNormals = map turn tangents
        vertexNormals = [normalize (n1 &+ n2) | n1 <- zero : edgeNormals | n2 <- edgeNormals ++ [zero]]
        insetRatio factor = 1 / (factor + 1)
        vertexParams = map distanceToParam insetDistances
          where
            distanceToParam d
                | d <= startDistance = remap d 0 startDistance (insetRatio insetFactorStart) 1
                | d >= endDistance = remap d curveLength endDistance (3 - insetRatio insetFactorEnd) 2
                | otherwise = remap d startDistance endDistance 1 2

remap :: Float -> Float -> Float -> Float -> Float -> Float
remap x x1 x2 y1 y2 = (x - x1) * (y2 - y1) / (x2 - x1) + y1

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
        [ ("position", A_V2F $ SV.fromList [V2 x y | (Vec2 x y, _) <- triangles])
        , ("distance", A_V4F $ SV.fromList [V4 d1 d2 w1 w2 | (_, Vec4 d1 d2 w1 w2) <- triangles])
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
  where
    triangles = concat (concat outlineTriangles)