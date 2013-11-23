module Robot (mkRobot,
              robotName,
              resetName)
where

import System.Random (randomRIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Monad (replicateM, void)
import Control.Applicative ((<$>), (<*>))

newtype Robot = Robot { getRobot :: MVar String }

mkRobot :: IO Robot
mkRobot = Robot <$> (mkName >>= newMVar)

robotName :: Robot -> IO String
robotName = readMVar . getRobot

resetName :: Robot -> IO ()
resetName r = void $ mkName >>= swapMVar (getRobot r)

mkName :: IO String
mkName = (++) <$> (randoms 2 ('A', 'B')) <*> (randoms 3 ('0', '9'))
    where randoms n (a, b) = replicateM n $ randomRIO (a, b)
