module AST
( module ParseTree
, module AST
) where

import qualified Data.Map as Map

import ParseTree (Identifier, Variable (..), Expression (..), Type)

type Program = Map.Map Identifier Class

data Class
    = Class
    { fields :: Map.Map Identifier Type
    , constructor :: Constructor
    , methods :: Map.Map Identifier Method
    }
    deriving (Eq, Show)

type Constructor = [Variable]

data Method
    = Method
    { params :: [Variable]
    , body :: Expression
    }
    deriving (Eq, Show)
