module Language.Dtfpl.NodeProc
    ( NodeProc
    , nodeProcConfig
    ) where

import           System.IO
import           System.Process.Typed

type NodeProc = Process Handle Handle ()

-- | Node process running js/main
nodeProcConfig :: ProcessConfig Handle Handle ()
nodeProcConfig = setStdin createPipe
               $ setStdout createPipe
               $ proc "node" ["js/main"]
