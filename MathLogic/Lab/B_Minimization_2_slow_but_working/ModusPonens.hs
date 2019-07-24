module ModusPonens where
import qualified Data.ByteString.Lazy as ByteString

data ParsedExpr = ParsedExpr ByteString.ByteString (Maybe (ByteString.ByteString, ByteString.ByteString))
