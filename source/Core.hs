module Core where


import Base
import Control.Monad.Reader


type App = ReaderT AppConfig IO

data AppConfig = AppConfig
