module Types where

import Protolude

import qualified Data.Graph.Inductive as G

type Task = G.Node
type CPU = G.Node

data CPUParams = CPUParams
  { _phLinksNumber :: Int
  , _inOutProcessor :: Bool
  , _duplexLinks :: Bool
  , _packetLength :: Int
  }
