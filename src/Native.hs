module Native where

import Type
import AST

consoleWriter :: Class
consoleWriter = Class
    { className = "ConsoleWriter"
    , classFields = []
    , classMethods = [writeInt]
    }
    where
        writeInt = Method
            { methodType = Void
            , methodName = "writeInt"
            , methodParams = [Parameter (PrimaryType TInt) "value"]
            , methodBlock = []
            }

