module Analyzer where

import Control.Applicative
import Control.Monad.State

import Data.Maybe
import qualified Data.List as List

import Type
import AST

class WithProgram a where
    getProgram :: a -> Program

class WithClass a where
    getClass :: a -> Class

class WithMethod a where
    getMethod :: a -> Method

findType :: (WithProgram s, MonadState s m) => ObjectType -> m (Maybe Class)
findType typeName = do
    classes <- gets getProgram
    return $ List.find (\cls -> className cls == typeName) classes

isTypeDefined :: (WithProgram s, Functor m, MonadState s m) => Type -> m Bool
isTypeDefined (ObjectType typeName) = isJust <$> findType typeName
isTypeDefined _ = return True

program :: (WithProgram s, MonadState s m) => m Program
program = gets getProgram

getClassM :: (WithClass s, MonadState s m) => m Class
getClassM = gets getClass

classNameM :: (WithClass s, Functor m, MonadState s m) => m String
classNameM = className <$> getClassM

method :: (WithMethod s, MonadState s m) => m Method
method = gets getMethod

methodTypeM :: (WithMethod s, Functor m, MonadState s m) => m MethodType
methodTypeM = methodType <$> method

filterMethod :: Method -> [Type] -> Ternary
filterMethod ms params = castTypeLists params $ map paramType $ methodParams ms

findClass :: (WithProgram s, MonadState s m) => ObjectType -> m (Either String Class)
findClass clsName = do
    mcls <- findType clsName
    case mcls of
        Just cls -> return $ Right cls
        Nothing -> return . Left $ "No class with name " ++ clsName

findMethod :: (WithProgram s, MonadState s m) => ObjectType -> String -> [Type] -> (Method -> Bool) -> m (Either String  Method)
findMethod typeName mth prms f = do
    cls <- findClass typeName
    return $ cls >>= \cls' -> do
        let methods = filter (\m -> (methodName m == mth) && (filterMethod m prms /= Zero) && f m) $ classMethods cls'
            (ones, halfs) = List.partition (\m -> filterMethod m prms == One) methods
        if length ones == 1
            then
                Right . head $ ones
            else
                case compare (length halfs) 1 of
                    GT -> Left $ "Ambiguous method call " ++ mth
                    LT -> Left "None method match"
                    EQ -> Right . head $ halfs

findField :: (WithProgram s, MonadState s m) => ObjectType -> String -> m (Either String (Type, Int))
findField typeName fieldName = do
    cls <- findClass typeName
    return $ cls >>= \cls' -> do
        let mfl = List.find (\f -> varName f == fieldName) $ classFields cls'
        case mfl of
            Just fl -> Right (varType fl, fromJust . List.elemIndex fl $ classFields cls')
            Nothing -> Left $ "class " ++ typeName ++ " has no field " ++ fieldName

emptyConstructor :: ObjectType -> Method
emptyConstructor t = Method
    { methodType = Constructor
    , methodName = t
    , methodParams = []
    , methodBlock = []
    }

addEmptyConstructor :: Class -> Class
addEmptyConstructor cls@(Class name _ mths) =
    if null $ filter (\m -> methodType m == Constructor) mths
        then cls { classMethods = emptyConstructor name : mths }
        else cls
