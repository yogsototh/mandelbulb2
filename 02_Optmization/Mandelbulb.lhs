 ## Optimization

This time we won't change anything in our main file.
But to make things faster we will use unpacked strict datas.
It will help GHC optimize a lot our computation (about 30% in my tests).
The notation is clearly verbose and ugly but it doesn't spread all other the code. And it will make our program faster.

We will only change two data declarations.
One in [`ExtComplex`](code/02_Mandelbulb/ExtComplex.hs):

<code class="haskell">
module ExtComplex where

import Graphics.Rendering.OpenGL

-- The magic is here
-- This is a Verbose and Ugly GHC Pragma
data ExtComplex = C {-# UNPACK #-} !GLfloat
                    {-# UNPACK #-} !GLfloat
                    {-# UNPACK #-} !GLfloat
                  deriving (Show,Eq)

instance Num ExtComplex where
    -- The shape of the 3D mandelbrot 
    -- will depend on this formula
    (C x y z) * (C x' y' z') = C (x*x' - y*y' - z*z') 
                                 (x*y' + y*x' + z*z') 
                                 (x*z' + z*x' )
    -- The rest is straightforward
    fromInteger n = C (fromIntegral n) 0 0
    (C x y z) + (C x' y' z') = C (x+x') (y+y') (z+z')
    abs (C x y z)     = C (sqrt (x*x + y*y + z*z)) 0 0
    signum (C x y z)  = C (signum x) (signum y) (signum z)

extcomplex :: GLfloat -> GLfloat -> GLfloat -> ExtComplex
extcomplex x y z = C x y z

real :: ExtComplex -> GLfloat
real (C x _ _)    = x

im :: ExtComplex -> GLfloat
im   (C _ y _)    = y

strange :: ExtComplex -> GLfloat
strange (C _ _ z) = z

magnitude :: ExtComplex -> GLfloat
magnitude = real.abs
</code>

I also made the same change in [`YGL.hs`](code/02_Mandelbulb/YGL.hs) 
for the `Point` data type.

before:

<code class="haskell">
data Point3D = P (Point,Point,Point) deriving (Eq,Show,Read)
</code>

after:

<code class="haskell">
data Point3D = P {-# UNPACK #-} !Point
                 {-# UNPACK #-} !Point
                 {-# UNPACK #-} !Point 
               deriving (Eq,Show,Read)
</code>


- [`YGL.hs`](code/06_Mandelbulb/YGL.hs), the 3D rendering framework
- [`Mandel`](code/06_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/06_Mandelbulb/ExtComplex.hs), the extended complexes

<div style="display:none">

> import YGL -- Most the OpenGL Boilerplate
> import Mandel -- The 3D Mandelbrot maths
> 
> -- Centralize all user input interaction
> inputActionMap :: InputMap World
> inputActionMap = inputMapFromList [
>      (Press ' ' , switchRotation)
>     ,(Press 'k' , rotate xdir 5)
>     ,(Press 'i' , rotate xdir (-5))
>     ,(Press 'j' , rotate ydir 5)
>     ,(Press 'l' , rotate ydir (-5))
>     ,(Press 'o' , rotate zdir 5)
>     ,(Press 'u' , rotate zdir (-5))
>     ,(Press 'f' , translate xdir 0.1)
>     ,(Press 's' , translate xdir (-0.1))
>     ,(Press 'e' , translate ydir 0.1)
>     ,(Press 'd' , translate ydir (-0.1))
>     ,(Press 'z' , translate zdir 0.1)
>     ,(Press 'r' , translate zdir (-0.1))
>     ,(Press '+' , zoom 1.1)
>     ,(Press '-' , zoom (1/1.1))
>     ,(Press 'h' , resize 2.0)
>     ,(Press 'g' , resize (1/2.0))
>     ]
> data World = World {
>       angle       :: Point3D
>     , anglePerSec :: Scalar
>     , scale       :: Scalar
>     , position    :: Point3D
>     , box         :: Box3D
>     , told        :: Time 
>     -- We replace shape by cache
>     , cache       :: [YObject]
>     } 
> instance DisplayableWorld World where
>   winTitle _ = "The YGL Mandelbulb"
>   camera w = Camera {
>         camPos = position w, 
>         camDir = angle w,
>         camZoom = scale w }
>   -- We update our objects instanciation
>   objects = cache
> xdir :: Point3D
> xdir = makePoint3D (1,0,0)
> ydir :: Point3D
> ydir = makePoint3D (0,1,0)
> zdir :: Point3D
> zdir = makePoint3D (0,0,1)
> 
> rotate :: Point3D -> Scalar -> World -> World
> rotate dir angleValue world = 
>   world {
>      angle = angle world + (angleValue -*< dir) }
>
> switchRotation :: World -> World
> switchRotation world = 
>   world {
>      anglePerSec = if anglePerSec world > 0 then 0 else 5.0 }
> 
> translate :: Point3D -> Scalar -> World -> World
> translate dir len world = 
>   world {
>     position = position world + (len -*< dir) }
> 
> zoom :: Scalar -> World -> World
> zoom z world = world {
>     scale = z * scale world }

> main :: IO ()
> main = yMainLoop inputActionMap idleAction initialWorld
> -- We initialize the world state
> -- then angle, position and zoom of the camera
> -- And the shape function
> initialWorld :: World
> initialWorld = World {
>    angle = makePoint3D (30,30,0)
>  , anglePerSec = 5.0
>  , position = makePoint3D (0,0,0)
>  , scale = 1.0
>  , box = Box3D { minPoint = makePoint3D (0-eps, 0-eps, 0-eps)
>                , maxPoint = makePoint3D (0+eps, 0+eps, 0+eps)
>                , resolution =  0.1 }
>  , told = 0
>  -- We declare cache directly this time
>  , cache = objectFunctionFromWorld initialWorld
>  }
>  where eps=2
> objectFunctionFromWorld :: World -> [YObject]
> objectFunctionFromWorld w = [Atoms atomListPositive]
>   where atomListPositive = 
>           atomsFromFunction3D 
>               (shapeFunc $ resolution b) b
>         b = box w
> resize :: Scalar -> World -> World
> resize r world = 
>   tmpWorld { cache = objectFunctionFromWorld tmpWorld }
>   where 
>       newres = sqrt ((resolution (box world))**2 * r)
>       eps3D = (30 * newres) -*< makePoint3D (1,1,1)
>       tmpWorld = world { box = (box world) {
>               --  minPoint = position world - eps3D
>               --, maxPoint = position world + eps3D
>               resolution = newres }}
> idleAction :: Time -> World -> World
> idleAction tnew world = 
>       world {
>         angle = angle world + (delta -*< zdir)
>       , told = tnew
>       }
>   where 
>       delta = anglePerSec world * elapsed / 1000.0
>       elapsed = fromIntegral (tnew - (told world))
> 
> shapeFunc :: Scalar -> Function3D
> shapeFunc res x y z = 
>   if v == 0 && any (>0) vs
>     then Just $ colorFromValue v
>     else Nothing 
>   where 
>       values = [(i,j,k) | 
>                     i <- [x - res, x+res]
>                   , j <- [y - res, y+res]
>                   , k <- [z - res, z+res] ]
>       v = ymandel x y z
>       vs = map (\(i,j,k)->ymandel i j k) values
>
> colorFromValue :: Point -> Color
> colorFromValue n =
>   let 
>       t :: Point -> Scalar
>       t i = 0.0 + 0.5*cos( i /10 )
>   in
>     makeColor (t n) (t (n+5)) (t (n+10))
> 
> -- given f min max nbtest,
> -- considering 
> --  - f is an increasing function
> --  - f(min)=0
> --  - f(max)≠0
> -- then maxZeroIndex f min max nbtest returns x such that
> --    f(x - ε)=0 and f(x + ε)≠0
> --    where ε=(max-min)/2^(nbtest+1) 
> maxZeroIndex :: (Fractional a,Num a,Num b,Eq b) => 
>                  (a -> b) -> a -> a -> Int -> a
> maxZeroIndex _ minval maxval 0 = (minval+maxval)/2
> maxZeroIndex func minval maxval n = 
>   if func medpoint /= 0 
>        then maxZeroIndex func minval medpoint (n-1)
>        else maxZeroIndex func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2
> 
> ymandel :: Point -> Point -> Point -> Point
> ymandel x y z = fromIntegral (mandel x y z 64) / 64

</div>

