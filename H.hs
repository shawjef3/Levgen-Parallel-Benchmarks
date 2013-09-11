import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import System.Environment
import qualified Data.Vector as V
import qualified Data.List as L
import Random.Xorshift
import Control.Monad
import Control.Monad.Random
import Control.Concurrent
import Control.DeepSeq
import Control.Concurrent.Chan
import Control.Monad.Trans.Class

type Pos = (Int,Int)

data Tile = Wall | Space deriving (Show)

instance NFData Tile

data Room = Room
    { rx, ry, rw, rh :: Int
    } deriving (Show)

instance NFData Room where
    rnf (Room rx ry rw rh) = rx `seq` ry `seq` rw `seq` rh `seq` ()

data Lev = Lev
    { lRooms :: V.Vector Room
    , lTiles :: [Tile]
    }

instance NFData Lev where
    rnf (Lev lRooms lTiles) = lRooms `deepseq` lTiles `deepseq` ()

levDim, minWid, maxWid :: Int
levDim = 50
minWid = 2
maxWid = 8

numLevs = 10
numTries = 50000

getRandomPos :: Monad m => RandT Xorshift m Int
getRandomPos = do
  r <- getRandom
  let rPos = abs r
  rPos `seq` return rPos

genRoom :: Monad m => RandT Xorshift m Room
genRoom = do
    r1 <- getRandomPos
    r2 <- getRandomPos
    r3 <- getRandomPos
    r4 <- getRandomPos
    let x = rem r1 levDim
    let y = rem r2 levDim
    let w = rem r3 maxWid + minWid
    let h = rem r4 maxWid + minWid
    return Room {rx = x, ry = y, rw = w, rh = h} 

genGoodRooms' :: Monad m => Int -> Int -> RandT Xorshift m (V.Vector Room)
genGoodRooms' n t = aux n t V.empty
    where aux 0 _ accum = return accum
          aux _ 0 accum = return accum
          aux count t accum = do
            room <- genRoom
            if goodRoom accum room
                then aux (count-1) (t-1) (V.cons room accum)
                else aux count (t-1) accum


goodRoom :: V.Vector Room -> Room -> Bool
goodRoom rooms room =
    not (checkBound room || checkColl room rooms)

checkBound :: Room -> Bool
checkBound (Room x y w h) =
    x<=0 || y<=0 || x+w >= levDim || y+h >= levDim

checkColl :: Room -> V.Vector Room -> Bool
checkColl room = V.any (roomHitRoom room)

roomHitRoom :: Room -> Room -> Bool
roomHitRoom (Room x y w h) (Room x2 y2 w2 h2)
    = not ((x2+w2+1) < x || x2 > (x+w+1)
        || (y2+h2+1) < y || y2 > (y+h+1))

inRoom :: Pos -> Room -> Bool
inRoom (x, y) (Room rx ry rw rh) =
        (rx <= x) && (x < rx + rw)
    &&  (ry <= y) && (y < ry + rh)

showTiles :: [Tile] -> String
showTiles = unlines . chunksOf levDim . map toChar
  where toChar Wall = '0'
        toChar Space = '1'

genLevel :: Monad m => RandT Xorshift m Lev
genLevel = do
    rooms <- genGoodRooms' 100 numTries
    let tiles = map (toTile rooms) [1 .. levDim ^ 2]
    return $ Lev{lRooms = rooms, lTiles = tiles}
  where
    toTile rooms n = if (V.any (toPos n `inRoom`) rooms) then Space else Wall
    toPos n = let (y, x) = quotRem n levDim in (x, y)

genLevelChan :: Chan Lev -> RandT Xorshift IO ()
genLevelChan chan =
    do level <- genLevel
       level `deepseq` lift (writeChan chan level)

genLevels :: Chan Lev -> RandT Xorshift IO ()
genLevels = forever . genLevelChan

biggestLev :: [Lev] -> Lev
biggestLev = L.maximumBy (comparing (V.length . lRooms))

readChanN :: Chan a -> Int -> IO [a]
readChanN chan n = do
  chanContents <- getChanContents chan
  return $ take n chanContents

main :: IO ()
main = do
    (v:_) <- fmap (++ ["18"]) $ getArgs
    putStr "The random seed is: "
    putStrLn v
    let levelCount = numLevs
    let gen = makeXorshift (read v)
    let rand = fst $ next gen
    numCapabilities <- getNumCapabilities
    let gens = map makeXorshift [rand .. rand+numCapabilities-1]
    levelChan <- newChan
    mapM_ (forkIO . evalRandT (genLevels levelChan)) gens
    levels <- readChanN levelChan levelCount
    putStr $ showTiles $ lTiles $ biggestLev levels
