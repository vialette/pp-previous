import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Tuple as T

data Step = UpStep | DownStep deriving (Show, Eq, Ord)

newtype Path = Path { getSteps :: [Step] } deriving (Show, Eq, Ord)

instance Semigroup Path where
  p <> p' = mk (getSteps p ++ getSteps p')

instance Monoid Path where
    mempty = mk []

mk ss = Path { getSteps = ss }
