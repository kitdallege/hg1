{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Protolude hiding ((&))
import Control.Lens
import System.IO (hSetBuffering, BufferMode(..))
import Foreign.C.Types
import Data.Bits (clearBit, testBit)
import Data.Aeson.Tiled hiding (Vector)
import SDL (($=))
import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import qualified SDL.Time
import qualified Data.Vector as V
import Control.Monad.Loops    (iterateUntilM)

import qualified Common as C

mapfile :: FilePath
mapfile = "/home/code/haskell/games/tiled-test/resources/orthogonal-outside.json"

-- loadMapFile needs [withSubstDTDEntities no] added to it.
-- otherwise you must remove the doctype declaration cuz it'll cause errors
-- https://stackoverflow.com/a/22858640  explains the error and how to fix it.

data FPS = FPS
    { fpsFrameCount :: Int
    , fpsLastTime :: Word32
    , fpsNumber :: Float
    } deriving (Show)


-- type Tile = Word32

data World = World
    { worldExit :: Bool
    , worldFPS :: FPS
    , worldFont :: SDL.Font.Font
    , worldTexture :: SDL.Texture
    , worldTiles :: V.Vector GlobalId
    , worldViewport :: SDL.Point SDL.V2 CInt
    }

mapWidth, mapHeight, imageWidth, imageHeight, columns, rows,tileCount, tileHeight, tileWidth :: CInt
mapWidth = 45
mapHeight = 31
imageWidth = 384
imageHeight = 192
columns = 24
rows = 12
tileCount = 288
tileHeight = 16
tileWidth = 16
tileAmountWidth :: CInt
tileAmountWidth = imageWidth `div` tileWidth


mkWorld ::
        FPS ->
        SDL.Texture ->
        SDL.Font.Font ->
        V.Vector GlobalId ->
        SDL.Point SDL.V2 CInt ->
        World
mkWorld fps texture font tilemap viewport = World
    { worldExit = False
    , worldTexture = texture
    , worldFPS = fps
    , worldFont=font
    , worldTiles=tilemap
    , worldViewport=viewport
    }


defaultViewport :: SDL.Point SDL.V2 CInt
defaultViewport = C.mkPoint
            (imageWidth `div` (-2))     -- ^ center of width
            (imageHeight `div` (-2))    -- ^ center of height

data Direction = North | South | East | West

data Intent
    = Idle
    | Quit
    | Move Direction

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    C.withSDL $ C.withSDLFont $ C.withSDLImage $ do
        C.setHintQuality
        C.withWindow "Loading/Rendering TiledMap" (640, 480) $ \w ->
            C.withRenderer w $ \r -> do
                font   <- SDL.Font.load "resources/font.ttf" 32
                (texture, textureInfo) <- C.loadTextureWithInfo r "resources/buch-outdoor.png"
                SDL.rendererScale r $= SDL.V2 3 3
                print textureInfo
                gameMap <- loadTiledmap mapfile
                case gameMap of
                    Left err -> print err
                    Right gameMap' -> do
                        let tiles = loadGameTiles gameMap'
                        print (length tiles)
                        curTicks <- SDL.ticks
                        let fps = FPS {fpsFrameCount=0, fpsLastTime=curTicks, fpsNumber=0}
                            initWorld = mkWorld fps texture font tiles defaultViewport
                            doRender = renderWorld r
                        _ <- iterateUntilM
                            worldExit
                            (\world-> do
                                world' <- updateWorld world <$> SDL.pollEvents
                                --w'' <- calcFPS w'
                                world' <$ doRender world'
                            ) initWorld
                        return ()
                SDL.destroyTexture texture
                SDL.Font.free font
                return ()

updateWorld :: World -> [SDL.Event] -> World
updateWorld w = foldl' (flip applyIntent) w . fmap (payloadToIntent . SDL.eventPayload)

loadGameTiles :: Tiledmap -> V.Vector GlobalId
loadGameTiles gmap = let Just l = head $ tiledmapLayers gmap
                         Just ld = layerData l
                         in ld


calcFPS :: World -> IO World
calcFPS w' = do
    let fps = worldFPS w'
        lastTime  = fpsLastTime fps
        numFrames = fpsFrameCount fps
    -- NEW: report FPS every second
    newTime <- SDL.ticks
    let newTime' = fromIntegral newTime

    let timeNextReport = lastTime + 1000
        timeDiff = newTime' - lastTime     -- time passed since last report
        -- Time passed, in seconds
        timeDiffSecs :: Float
        timeDiffSecs = fromIntegral timeDiff / 1000

        -- Num frames in time passed
        fpsNum :: Float
        fpsNum = fromIntegral numFrames / timeDiffSecs
    putStrLn $ show fpsNum <> ": " <> (show fps :: Text) <> " " <> show timeDiff
    if newTime' > timeNextReport then
        return $ w' {worldFPS=FPS {fpsFrameCount=0, fpsLastTime=newTime', fpsNumber=0}}
    else
        return $ w' {worldFPS=FPS{fpsLastTime=lastTime, fpsFrameCount=numFrames + 1, fpsNumber=fpsNum}}

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
-- payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.KeyboardEvent k)    = keyEventToIntent k
payloadToIntent _                        = Idle

keyEventToIntent :: SDL.KeyboardEventData -> Intent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
    case SDL.keysymKeycode keysym of
        SDL.KeycodeEscape   -> Quit
        SDL.KeycodeUp       -> Move North
        SDL.KeycodeDown     -> Move South
        SDL.KeycodeRight    -> Move East
        SDL.KeycodeLeft     -> Move West
        _                   -> Idle

keyEventToIntent _ = Idle


applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld
applyIntent (Move dir)  = moveWorld dir

idleWorld :: World -> World
idleWorld = identity

quitWorld :: World -> World
quitWorld w = w { worldExit = True }

moveWorld :: Direction -> World -> World
moveWorld d w = case d of
    North   -> moveViewport w 0 2
    South   -> moveViewport w 0 (-2)
    East    -> moveViewport w (-2) 0
    West    -> moveViewport w 2 0
  where
    --   moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
    --   moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (C.mkPoint x y) d
      moveViewport :: World -> CInt -> CInt -> World
      moveViewport w' x y = let
            vp = worldViewport w
            vp' = vp & SDL._x %~ (+ x) & SDL._y %~ (+ y)
            in w' {worldViewport=vp'}



renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r w = do
    SDL.clear r
    drawWorld r w
    SDL.present r

drawWorld :: SDL.Renderer -> World -> IO ()
drawWorld r w = do
    SDL.rendererDrawColor r $=  SDL.V4 199 242 245 maxBound
    let tiles = worldTiles w
        tilesByIndex = V.zip (V.enumFromN 0 (length tiles + 1)) tiles
    forM_ tilesByIndex $ uncurry (drawTile r w)
    return ()

coordFromNum :: CInt -> CInt -> SDL.V2 CInt
coordFromNum n w = SDL.V2 ( n `mod` w) (n `div` w)

numFromCoord :: CInt -> (CInt, CInt) -> Int
numFromCoord (CInt width) (CInt x, CInt y) = fromIntegral (y * width + x) :: Int

drawLayer :: SDL.Renderer -> World -> Layer -> IO ()
drawLayer = undefined

drawTile :: SDL.Renderer -> World ->  CInt -> GlobalId -> IO ()
drawTile r w idx tile = do
    let gid = CInt $ fromIntegral (unGlobalId tile `clearBit` 30 `clearBit` 31 `clearBit` 29)
    -- TODO: Account for rotation via bit-flipping
    -- http://doc.mapeditor.org/en/latest/reference/tmx-map-format/#tile-flipping
    -- https://github.com/chrra/htiled/blob/master/src/Data/Tiled/Load.hs#L127
    -- if gid is 0 then there is nothing to draw
    when (gid > 0 && gid < tileCount) $ do
        let idxX = idx `mod` mapWidth
            idxY = idx `div` mapWidth
            lgid = gid - 1 -- gid minus the layers.lastgid (happens 1 in this case)
            srcX = lgid `mod` tileAmountWidth
            srcY = lgid `div` tileAmountWidth
            srcRect :: SDL.Rectangle CInt
            srcRect = C.mkRect
                        (srcX * tileWidth)
                        (srcY * tileHeight)
                        tileWidth tileHeight
            -- mkRect x y w h = ...
            --TODO: factor in worldViewport
            destRect :: SDL.Rectangle CInt
            destRect = C.mkRect
                        ((idxX * tileWidth) + view SDL._x (worldViewport w))    -- move left|right
                        ((idxY * tileHeight) + view SDL._y (worldViewport w))   -- move up|down
                        tileWidth
                        tileHeight
        SDL.copy r (worldTexture w) (Just srcRect) (Just destRect)
        --TODO: have to deal with rotation via copyEx
        return ()





{-
data Tile = Tile
    { tileImgRect :: Maybe (SDL.Rectangle CInt)
      -- ^ Rectangle corresponding to the section of the tileset texture the tile comes from.
    , tileRect    :: Maybe (SDL.Rectangle CInt)
      -- ^ Rectangle corresponding to the section of the screen the tile is to be placed.
    , tileValue   :: Int
      -- The numerical value of the tile.
    }

data Tileset = Tileset
    { tilesetImage   :: Image
     -- ^ Returns the Image
    , tileSize       :: Dimension
    -- ^ The individual tile size. eg: (32, 32)
    , tilesetSpacing :: Dimension
     -- ^ The spacing between each tile in the tileset
    , tileClips      :: [Maybe (SDL.Rectangle CInt)]
    -- ^ All the Rectangles relative to the Tileset
    }

data Layer = Layer
    { layerTileset   :: Tileset
     -- ^ Return the Tileset
    , layerDimension :: Dimension
     -- ^ The layer dimension realtive to the number of tiles. eg: (20, 15)
    , layerTiles     :: [Tile]
     -- ^ A list of the the Tiles in the Layer.
    }


-}
