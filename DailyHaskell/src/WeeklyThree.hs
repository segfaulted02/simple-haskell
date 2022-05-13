module WeeklyThree where

--new type Vec as a list of Doubles
data Vec = Vec [Double]

--instantiated Vec as a member of Show, defines the show function
instance Show Vec where
    show (Vec a) = "Vec " ++ show a

--instantiated Vec as a member of Num, defines all the associated functions of Num
instance Num Vec where
    (+) (Vec a) (Vec b) = Vec (zipWith (+) a b)
    (-) (Vec a) (Vec b) = Vec (zipWith (-) a b)
    (*) (Vec a) (Vec b) = Vec (zipWith (*) a b)
    negate (Vec a) = Vec (map (negate) a)
    abs (Vec a) = Vec (map (abs) a)
    fromInteger a = Vec (repeat (fromIntegral a))
    signum (Vec a) = Vec (map signum a)

--instantiated Vec as a member of Eq, demfines all the associated functions of Eq
instance Eq Vec where
    (==) (Vec a) (Vec b) = a == b
    (/=) (Vec a) (Vec b) = a /= b

--instantiated Vec as a member of Ord, defines all the associated functions of Ord
instance Ord Vec where
    compare (Vec a) (Vec b) = (compare a b)
    (<) (Vec a) (Vec b) = (a < b)
    (<=) (Vec a) (Vec b) = (a <= b)
    (>) (Vec a) (Vec b) = (a > b)
    (>=) (Vec a) (Vec b) = (a >= b)
    max (Vec a) (Vec b) = Vec (max a b)
    min (Vec a) (Vec b) = Vec (min a b)

--new VecT typeclass, with only the function magnitude declared
class VecT a where
    magnitude :: a -> Double

--instantiated Vec as a member of VecT, defines the magnitude function
instance VecT Vec where
    magnitude (Vec a) = sqrt (foldr (+) 0 (map (**2) a))

--instantiated Vec as a member of Semigroup, defines the associative function of Semigroup
instance Semigroup Vec where
    (<>) (Vec a) (Vec b) = Vec (a ++ b)

--instantiated Vec as a member of Monoid, defines all the associated functions of Monoid
instance Monoid Vec where
    mempty = mempty
    mappend = (<>)
    mconcat = foldr mappend mempty