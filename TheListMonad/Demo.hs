-- import Control.Monad
import Prelude hiding (Monad, return, fail, (>>=), (>>))

class Monad m where
 return :: a -> m a
 (>>=) :: m a -> (a -> m b) -> m b
 (>>) :: m a -> m b -> m b
 fail :: String -> m a

instance Monad [] where
 return x = [x]
 xs >>= k = concat (map k xs)
 fail _ = []

foo1 = do { x <- [1,2,3]; y <- [4,5,6]; return(x, y) }
foo2 =
    [1, 2, 3] >>= (
        \x -> [4, 5, 6] >>= (
            \y -> return (x,y)
        )
    )