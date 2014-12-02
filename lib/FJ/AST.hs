module FJ.AST
( module FJ.Common
, module FJ.AST
) where

import qualified Data.Map as Map

import FJ.Common

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
    { mthType :: Type
    , params :: [Variable]
    , body :: TypedExpression
    }
    deriving (Eq, Show)
