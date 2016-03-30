{-
Определите тип записи, который хранит элементы лога. Имя конструктора должно совпадать с именем типа, 
и запись должна содержать три поля:
timestamp — время, когда произошло событие (типа UTCTime);
logLevel — уровень события (типа LogLevel);
message — сообщение об ошибке (типа String).

Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel, 
и функцию logEntryToString, возвращающую текстуальное представление записи в виде:
<время>: <уровень>: <сообщение>

Для преобразование типа UTCTime в строку используйте функцию timeToString.
-}

module Demo where

import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info
instance Show LogLevel where
    show Error = "Error"
    show Warning = "Warning"
    show Info = "Info"

{-
data LogEntry' = LogEntry' UTCTime LogLevel String
timestamp' :: LogEntry' -> UTCTime
timestamp' (LogEntry' t _ _) = t
logLevel'  :: LogEntry' -> LogLevel
logLevel' (LogEntry' _ l _) = l
message' :: LogEntry' -> String
message' (LogEntry' _ _ s) = s
-}
data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String } deriving (Show)

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry t l s) = (timeToString t) ++ ": " ++ (show l) ++ ": " ++ s
