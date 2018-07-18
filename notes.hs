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

data List a = Nil | Cons a (List a)

addList :: List Integer -> Integer
addList Nil = 0
addList (Cons x xs) = x + addList xs

-- Day 2