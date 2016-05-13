import Data.Vect.Float.Base
import Data.Vect.Float.Util.Quaternion
import Prelude hiding (repeat)

type Coordinate    = Vec3

type Radians       = Float

type Time          = Float 

data Result        = Going (Coordinate, UnitQuaternion) | Done Time (Coordinate, UnitQuaternion) deriving (Show)

type Rollercoaster = (Time -> Result) -> (Time -> Result)

-- Sequential composition of rollercoasters
(<>) :: Rollercoaster -> Rollercoaster -> Rollercoaster
(<>) r r' f t = case r f t of
                    v@(Going _)    -> v
                    Done t' cv     -> ((delay (t - t') r') . r) f t

-- Delay a rollercoaster
delay :: Time -> Rollercoaster -> Rollercoaster
delay t r f t' = r (\_ -> f t') (max (t' - t) 0)

-- Keep repeating a rollercoaster forever
forever :: Rollercoaster -> Rollercoaster
forever r = r <> forever r

-- Repeat a rollercoaster a limited number of times
repeat :: Int -> Rollercoaster -> Rollercoaster
repeat 0 _ = id
repeat n r = r <> repeat (n-1) r

-- Limit the running time of the rollercoaster
limit :: Time -> Rollercoaster -> Rollercoaster
limit d r f t
    | t <= d    = r f t
    | otherwise = case r f d of
                    Going cv   -> Done (t - d) cv 
                    Done t' cv -> Done (t' + t - d) cv

-- The start of a rollercoaster
start :: Time -> Result
start t = Done t (Vec3 0.0 0.0 0.0, unitU)

-- Rotate a rollercoaster
rotate :: Vec3 -> Radians -> Rollercoaster -> Rollercoaster 
rotate v rad r f = r rot  
    where
        rot t = case f t of
                    Going (v', q)   -> Going (v', multU q (rotU v rad))
                    Done t' (v', q) -> Done t' (v', multU q (rotU v rad))

-- Go straight with a given velocity for a given amount of time
direction :: Vec3 -> Time -> Rollercoaster
direction d dur r t
    | t <= dur  = case r t of
                    Going (v, q)   -> Going (v &+ (t *& (actU q d)), q) 
                    Done t' (v, q) -> Going (v &+ (t *& (actU q d)), q)
    | otherwise = case r t of
                    Going (v, q)   -> Going (v &+ (dur *& (actU q d)), q)
                    Done t' (v, q) -> Done (min t' (t - dur)) (v &+ (dur *& (actU q d)), q)
