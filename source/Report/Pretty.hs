module Report.Pretty where

import Base
import Report.Region

import Data.Text.Prettyprint.Doc


type Message = Text
type RawSource = Text

report :: Message -> Region -> RawSource -> Doc ann
report _msg _r _raw = error "Report.Pretty.report"
