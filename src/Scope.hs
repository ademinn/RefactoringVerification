module Scope where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map

type BlockVariables t = Map.Map String t

type Scope t = [BlockVariables t]

class Scoped a t | a -> t where
    getScope :: a -> Scope t
    setScope :: a -> Scope t -> a
    modifyScope :: (Scope t -> Scope t) -> a -> a
    modifyScope f a = setScope a $ f . getScope $ a

scope :: (Scoped s t, MonadState s m) =>  m (Scope t)
scope = gets getScope

updateScope :: (Scoped s t, MonadState s m) => (Scope t -> Scope t) -> m ()
updateScope f = modify $ modifyScope f

addScope :: (Scoped s t, MonadState s m) => m ()
addScope = updateScope $ \s -> Map.empty : s

removeScope :: (Scoped s t, MonadState s m) => m ()
removeScope = updateScope tail

findLocalVar :: String -> Scope t -> Maybe t
findLocalVar n l = join . List.find isJust $ map (Map.lookup n) l

lookupLocalVar :: (Scoped s t, Functor m, MonadState s m) => String -> m (Maybe t)
lookupLocalVar n = findLocalVar n <$> scope

localVar :: (Scoped s t, Functor m, MonadState s m) => String -> m t
localVar n = fromJust <$> lookupLocalVar n

isLocalVarDefined :: (Scoped s t, Functor m, MonadState s m) => String -> m Bool
isLocalVarDefined n = do
    var <- lookupLocalVar n
    return $ isJust var

newLocalVar :: (Scoped s t, Functor m, MonadState s m) => String -> t -> m (Maybe String)
newLocalVar n t = do
    defined <- isLocalVarDefined n
    if defined
        then return . Just $ "Variable with name " ++ n ++ " already defined"
        else do
            updateScope $ \(l:ls) -> Map.insert n t l : ls
            return Nothing
