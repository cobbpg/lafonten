{-# LANGUAGE PackageImports, OverloadedStrings, DataKinds, TypeOperators #-}

import Control.Monad
import Control.Monad.Fix
import Data.Time.Clock
import qualified Data.Trie as T
import qualified Data.Vector.Storable as SV
import Graphics.Text.TrueType
import "GLFW-b" Graphics.UI.GLFW as GLFW
import LambdaCube.Font.Atlas
import LambdaCube.Font.Common
import qualified LambdaCube.Font.SimpleDistanceField as SDF
import qualified LambdaCube.Font.CompositeDistanceField as CDF
import LambdaCube.GL
import LambdaCube.GL.Mesh
import System.Environment
import System.Exit

useCompositeDistanceField = True

textStyle = defaultTextStyle { textLetterSpacing = 0.0, textLineHeight = 1.25 }
fontOptions = defaultOptions { atlasSize = 1024, atlasLetterPadding = 2 }

main = do
    args <- getArgs
    when (null args) $ do
        putStrLn "Usage: HelloWorld <ttf-file> [<pixels-per-em>]"
        exitSuccess

    initialize
    openWindow defaultDisplayOptions
        { displayOptions_width              = 1024
        , displayOptions_height             = 768
        , displayOptions_openGLVersion      = (3, 2)
        , displayOptions_openGLProfile      = CoreProfile
        }
    setWindowTitle "LambdaCube 3D Text Demo"

    Right font <- loadFontFile (head args)
    let fontRenderer = if useCompositeDistanceField then CDF.fontRenderer else SDF.fontRenderer
        letterScale = if length args > 1 then read (args !! 1) else 72
    atlas <- createFontAtlas font fontRenderer fontOptions { atlasLetterScale = letterScale }
    t1 <- getCurrentTime
    textMesh <- buildTextMesh atlas textStyle $ unlines
                [ "Hello, gorgeous world!"
                , "LambdaCube 3D shall prevail!"
                , "Move with arrows, zoom with A/Q."
                , "árvíztűrő tükörfúrógép"
                , "ÁRVÍZTŰRŐ TÜKÖRFÚRÓGÉP"
                ]
    t2 <- getCurrentTime
    putStrLn $ "Text rendering time: " ++ show (diffUTCTime t2 t1)

    renderer <- compileRenderer (ScreenOut (PrjFrameBuffer "" tix0 testRender))
    setScreenSize renderer 1024 768

    textBuffer <- compileMesh textMesh
    textObject <- addMesh renderer "textMesh" textBuffer []

    quadBuffer <- compileMesh quadMesh
    quadObject <- addMesh renderer "quadMesh" quadBuffer []

    let uniforms = uniformSetter renderer
        letterScale = atlasLetterScale (atlasOptions atlas)
        letterPadding = atlasLetterPadding (atlasOptions atlas)
    uniformFTexture2D "fontAtlas" uniforms (getTextureData atlas)

    startTime <- getCurrentTime
    flip fix (startTime, V2 (-0.95) 0, 0.2) $ \loop (prevTime, V2 ofsX ofsY, scale) -> do
        uniformM33F "textTransform" uniforms (V3 (V3 (scale * 0.75) 0 0) (V3 0 scale 0) (V3 ofsX ofsY 1))
        uniformFloat "outlineWidth" uniforms (min 0.5 (fromIntegral letterScale / (768 * fromIntegral letterPadding * scale * sqrt 2 * 0.75)))
        render renderer
        swapBuffers
        escPressed <- keyIsPressed KeyEsc
        curTime <- getCurrentTime
        let dt = realToFrac (diffUTCTime curTime prevTime) :: Float
        [left, right, up, down, zoomIn, zoomOut] <- mapM keyIsPressed [KeyLeft, KeyRight, KeyUp, KeyDown, CharKey 'Q', CharKey 'A']
        let inputX = (if right then -1 else 0) + (if left then 1 else 0)
            inputY = (if up then -1 else 0) + (if down then 1 else 0)
            inputScale = (if zoomOut then -1 else 0) + (if zoomIn then 1 else 0)
            scaleChange = (1 + dt) ** inputScale
            scale' = scale * scaleChange
            ofsX' = ofsX * scaleChange + inputX * dt * 2
            ofsY' = ofsY * scaleChange + inputY * dt * 2
        unless escPressed (loop (curTime, V2 ofsX' ofsY', scale'))

    terminate

testRender :: Exp Obj (FrameBuffer 1 V4F)
testRender = renderQuad (renderText emptyBuffer)
  where
    renderText = Accumulate textFragmentCtx PassAll textFragmentShader textFragmentStream
    renderQuad = Accumulate quadFragmentCtx PassAll quadFragmentShader quadFragmentStream
    emptyBuffer = FrameBuffer (ColorImage n1 (V4 0 0 0 1) :. ZT)
    rasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex

    textFragmentCtx = AccumulationContext Nothing (ColorOp textBlending (V4 True True True True) :. ZT)
    textBlending = Blend (FuncAdd, FuncAdd) ((One, One), (OneMinusSrcAlpha, One)) zero'
    textFragmentStream = Rasterize rasterCtx textStream
    textStream = Transform vertexShader (Fetch "textMesh" Triangles (IV2F "position", IV2F "uv"))

    quadFragmentCtx = AccumulationContext Nothing (ColorOp NoBlending (V4 True True True True) :. ZT)
    quadFragmentStream = Rasterize rasterCtx quadStream
    quadStream = Transform vertexShader (Fetch "quadMesh" Triangles (IV2F "position", IV2F "uv"))

    vertexShader attr = VertexOut point (floatV 1) ZT (Smooth uv :. ZT)
      where
        point = v3v4 (transform @*. v2v3 pos)
        transform = Uni (IM33F "textTransform") :: Exp V M33F
        (pos, uv) = untup2 attr

    textFragmentShader uv = FragmentOut (pack' (V4 result result result result) :. ZT)
      where
        result = step distance
        distance = case useCompositeDistanceField of
            False -> SDF.sampleDistance "fontAtlas" uv
            True -> CDF.sampleDistance "fontAtlas" uv
        step = smoothstep' (floatF 0.5 @- outlineWidth) (floatF 0.5 @+ outlineWidth)
        outlineWidth = Uni (IFloat "outlineWidth") :: Exp F Float

    quadFragmentShader uv = FragmentOut (texture' (Sampler LinearFilter Repeat tex) uv :. ZT)
      where
        tex = TextureSlot "fontAtlas" (Texture2D (Float RGBA) n1)

quadMesh :: Mesh
quadMesh = Mesh
    { mAttributes   = T.fromList
        [ ("position", A_V2F (SV.fromList [V2 0 1, V2 1 1, V2 1 2, V2 1 2, V2 0 2, V2 0 1]))
        , ("uv", A_V2F (SV.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 1, V2 0 1, V2 0 0]))
        ]
    , mPrimitive    = P_Triangles
    , mGPUData      = Nothing
    }
