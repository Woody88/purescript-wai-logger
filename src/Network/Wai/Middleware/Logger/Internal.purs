module Network.Wai.Middelware.Logger.Internal where 

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Network.Wai (Request, Response)
import Network.Wai.Middleware.Logger.Types (ApplicationTime, Token(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Record (get, insert)
import Record.Format (class FormatParsed, class Parse)
import Record.Format as Record
import Type.Data.RowList (RLProxy(..))

-- Credits to @justinwoo followed this blog: https://qiita.com/kimagure/items/06d7eed9521b6217b771
class MapRecord (xs :: RowList) (row :: # Type) (row' :: # Type) a b | xs -> row row' a b where
  mapRec :: RLProxy xs -> (a -> b) -> Record row -> Record row'

instance mapRecordNil :: MapRecord RL.Nil row () a b where 
  mapRec _ _ _ = {}

instance mapRecordCons :: 
  ( IsSymbol name
  , Row.Cons name a t row
  , Row.Lacks name tailRow
  , MapRecord tail row tailRow a b 
  , Row.Cons name b tailRow row'  
  ) => MapRecord (RL.Cons name a tail) row row' a b where 
  mapRec _ fn rec = 
    insert nameP value rest 
    where 
      nameP = SProxy :: _ name 
      value = fn $ get nameP rec
      rest  = mapRec (RLProxy :: _ tail) fn rec

mapRecord :: forall rl r r' a b. RowToList r rl => MapRecord rl r r' a b => (a -> b) -> Record r -> Record r'
mapRecord fn r = mapRec (RLProxy :: _ rl) fn r 

formatLineToken :: forall format parsed rl r r' . 
  RowToList r rl 
  => Parse format parsed
  => FormatParsed parsed r'
  => MapRecord rl r r' Token String
  => Request -> Response -> ApplicationTime -> SProxy format -> Record r -> String
formatLineToken req res time sym r = Record.format sym $ mapRecord (\(Token func) -> func req res time) r