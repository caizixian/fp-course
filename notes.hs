-- Day 1

x :: Integer
x = 99

f :: Integer -> Integer
f a = a + 10

g :: Integer -> Integer -> Integer
g a b = (a + b) * 2

-- infix position by default
(.+.) :: Integer -> Integer -> Integer
(.+.) a b = (a + b) * 2

h :: Integer -> Integer -> Integer
h = \a b -> (a + b) * 2

i :: (Integer -> Integer) -> Integer
i k = k 100

j :: (Integer -> a) -> a
j k = k 100

z :: anything -> anything
z c = c

y :: a -> b -> a
y p _ = p

data Shape =
    Circle Integer
    | Rectangle Integer Integer
    | Triangle Integer Integer Integer
    deriving (Eq, Show)

pie = 3

perimeter :: Shape -> Integer
perimeter = \s -> case s of
    Circle r -> r * 2 * pie
    Rectangle w h -> (w + h) * 2
    Triangle a b c -> a + b + c

perimeter2 :: Shape -> Integer
perimeter2 (Circle r) = r * 2 * pie
perimeter2 (Rectangle w h) = (w + h) * 2
perimeter2 (Triangle a b c) = a + b + c

data Three a = T a a a deriving (Eq, Show)

multiply :: Three Integer -> Integer
multiply (T a b c) = a * b * c

m :: (a -> b) -> Three a -> Three b
m f (T a1 a2 a3) = T (f a1) (f a2) (f a3)

isInList e list =
    case list of
        [] -> False
        (x:xs) -> (e == x) || isInList e xs

-- data List a = Nil | Cons a (List a)
data List t =
    Nil
    | t :. List t
    deriving (Eq, Ord)

addList :: List Integer -> Integer
addList Nil = 0
addList (x:.xs) = x + addList xs

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :. t) = f h (foldRight f b t)

-- Day 2
data Optional a =
    Full a
    | Empty
    deriving (Eq, Show)

class ThingsThatMap k where
    howtodomap :: (a -> b) -> k a -> k b

instance ThingsThatMap Optional where
    howtodomap _ Empty = Empty
    howtodomap f (Full a) = Full (f a)

instance ThingsThatMap List where
    howtodomap f = foldRight ((:.) . f) Nil

instance ThingsThatMap ((->) t) where
    howtodomap a2b t2a = \t -> a2b $ t2a t

amapAnything :: ThingsThatMap k => b -> k a -> k b
amapAnything b x = howtodomap (\_ -> b) x

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    -- laws
    -- mappend x mempty === x
    -- mappend mempty x === x
    -- mappend (mappend x y) z === mappend x (mappend y z)

instance Monoid (List a) where
    mempty = Nil
    mappend = (++)

-- instance Monoid (Optional a) where
--     mempty = Empty
--     mappend Empty a = a
--     mappend a Empty = a

instance Monoid a => Monoid (Optional a) where
    mempty = Empty
    mappend Empty a = a
    mappend a Empty = a
    mappend (Full x) (Full y) = Full (x `mappend` y)

data Endo a = Endo (a -> a)

instance Monoid (Endo a) where
    mempty = Endo id
    mappend (Endo f) (Endo g) = Endo (f . g)

data Sum = Sum Int

instance Monoid Sum where
    mempty = Sum 0
    mappend (Sum x) (Sum y) = Sum (x + y)

foldMap :: Monoid m => List a -> (a -> m) -> m
foldMap Nil _ = mempty
foldMap (x:.xs) f = f x `mappend` foldMap xs f

sum xs = case foldMap xs Sum of
    Sum result -> result

fold :: Monoid m => List m -> m
fold xs = foldMap xs id

composed :: Int -> Int
composed = case foldMap [(+1), (*2), subtract 3] Endo of
    Endo f -> f