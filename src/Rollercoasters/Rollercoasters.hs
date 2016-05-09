import Prelude hiding (repeat)

type Vector3       = (Double, Double, Double)

type Coordinate    = Vector3

type Viewvector    = Vector3

type Time          = Double

data Result        = Going (Coordinate, Viewvector) | Done Time (Coordinate, Viewvector) deriving (Show)

type Rollercoaster = (Time -> Result) -> (Time -> Result)

muld :: (Coordinate, Viewvector) -> Coordinate -> Time -> (Coordinate, Viewvector)
muld ((a0, a1, a2), v) (c0, c1, c2) t = ((a0+c0*t, a1+c1*t, a2+c2*t), v)

-- Sequential composition of rollercoasters
(<>) :: Rollercoaster -> Rollercoaster -> Rollercoaster
(<>) r r' f t = case r f t of
                v@(Going _)    -> v
                Done t' cv     -> ((delay (t - t') r') . r) f t

start :: Time -> Result
start t = Done t ((0, 0, 0), (0, 0, 0))

direction :: Vector3 -> Time -> Rollercoaster
direction d dur f t
    | t < dur   = case f t of
                    Going cv   -> Going (muld cv d t) 
                    Done t' cv -> Going (muld cv d t)
    | otherwise = case f t of
                    Going cv   -> Going (muld cv d dur)
                    Done t' cv -> Done (min t' (t-dur)) (muld cv d dur) 

delay :: Time -> Rollercoaster -> Rollercoaster
delay t r f t' = r (\_ -> f t') (max (t' - t) 0)

forever :: Rollercoaster -> Rollercoaster
forever r = r <> forever r

loop :: Time -> Double -> Rollercoaster 
loop dur r f t
    | t < dur   = case f t of
                    Going cv    -> Going (muld cv (r*(sin (2*pi*t/dur)), 0, r-r*(cos (2*pi*t/dur))) 1)
                    Done  t' cv -> Going (muld cv (r*(sin (2*pi*t/dur)), 0, r-r*(cos (2*pi*t/dur))) 1)
    | otherwise = case f t of
                    Going cv   -> Going cv
                    Done t' cv -> Done (min t' (t - dur)) cv

repeat :: Int -> Rollercoaster -> Rollercoaster
repeat 0 _ = id
repeat n r = r <> repeat (n-1) r

limit :: Time -> Rollercoaster -> Rollercoaster
limit d r f t
    | t <= d    = r f t
    | otherwise = case r f d of
                    Going cv   -> Done (t - d) cv 
                    Done t' cv -> Done (t' + t - d) cv
