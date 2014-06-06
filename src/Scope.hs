module Scope where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map

import Type

data VarSymbol
    = VarSymbol
    { vsName :: String
    , vsType :: Type
    , vsInit :: Bool
    } deriving Show

type BlockVariables = Map.Map String VarSymbol

type Scope = [BlockVariables]

class Scoped a where
    getScope :: a -> Scope
    setScope :: a -> Scope -> a
    modifyScope :: (Scope -> Scope) -> a -> a
    modifyScope f a = setScope a $ f . getScope $ a

scope :: (Scoped s, MonadState s m) =>  m Scope
scope = gets getScope

updateScope :: (Scoped s, MonadState s m) => (Scope -> Scope) -> m ()
updateScope f = modify $ modifyScope f

addScope :: (Scoped s, MonadState s m) => m ()
addScope = updateScope $ \s -> Map.empty : s

removeScope :: (Scoped s, MonadState s m) => m ()
removeScope = updateScope tail

findLocalVar :: String -> Scope -> Maybe VarSymbol
findLocalVar n l = join . List.find isJust $ map (Map.lookup n) l

lookupLocalVar :: (Scoped s, Functor m, MonadState s m) => String -> m (Maybe VarSymbol)
lookupLocalVar n = findLocalVar n <$> scope

isLocalVarDefined :: (Scoped s, Functor m, MonadState s m) => String -> m Bool
isLocalVarDefined n = do
    var <- lookupLocalVar n
    return $ isJust var

newLocalVar :: (Scoped s, Functor m, MonadState s m) => Type -> String -> Bool -> m (Maybe String)
newLocalVar t n i = do
    defined <- isLocalVarDefined n
    if defined
        then return . Just $ "Variable with name " ++ n ++ " already defined"
        else do
            updateScope $ \(l:ls) -> Map.insert n (VarSymbol n t i) l : ls
            return Nothing
