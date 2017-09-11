module MemoryChan
    ( MemoryChan
    , newMemoryChan
    , writeMemoryChan
    , readMemoryChan
    , closeMemoryChan
    , subscribe
    , dump
    )
where

import Data.Sequence as Seq
import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TMChan as Chan

data MemoryChan a = MemoryChan (TVar (Seq a)) (Chan.TMChan a)

newMemoryChan :: IO (MemoryChan a)
newMemoryChan = do
    ch <- Chan.newTMChanIO
    s <- newTVarIO Seq.empty
    pure $ MemoryChan s ch


writeMemoryChan :: MemoryChan a -> a -> STM ()
writeMemoryChan (MemoryChan buffer ch) x = do
    modifyTVar' buffer (|> x)
    Chan.writeTMChan ch x


readMemoryChan :: MemoryChan a -> STM (Maybe a)
readMemoryChan (MemoryChan _ ch) = Chan.readTMChan ch


closeMemoryChan :: MemoryChan a -> STM ()
closeMemoryChan (MemoryChan buffer ch) = readTVar buffer >> Chan.closeTMChan ch


dump :: MemoryChan a -> STM (Seq a)
dump (MemoryChan buffer _) = readTVar buffer


subscribe :: MemoryChan a -> IO (Chan.TMChan a)
subscribe (MemoryChan buffer ch) = atomically $ do
    newChan <- Chan.dupTMChan ch
    content <- readTVar buffer
    traverse (Chan.writeTMChan newChan) content
    pure newChan
