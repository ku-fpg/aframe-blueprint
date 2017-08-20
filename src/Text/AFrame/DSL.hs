{-# LANGUAGE GADTs, OverloadedStrings, StandaloneDeriving, KindSignatures, DataKinds, DeriveFunctor, FlexibleInstances, InstanceSigs, RankNTypes #-}
-- | Small monadic DSL for AFrame generation.
module Text.AFrame.DSL 
  (  -- * Entity DSL
    DSL,
    AFrame,
    scene,
    assets,
    entity,
    box,
    camera,
    collada_model,
    cone,
    cursor,
    curvedimage,
    cylinder,
    image,
    light,
    obj_model,
    plane,
    ring,
    sky,
    sphere,
    torus,
    video,
    videosphere,
    -- * Asset DSL
    img,
    -- * Component DSL
    fog,
    geometry,
    material,
    stats,
    template,
    wasd_controls,
    -- * Attribute DSL
    attribute,
    attribute_,
    begin,
    color,
    dir,
    delay,
    depth,
    dur,
    easing,
    elasticity,
    fill,
    fov,
    height,
    id_,
    lookControlsEnabled,
    loop,
    metalness,
    opacity,
    open,
    position,
    radius,
    radiusTop,
    radiusBottom,
    repeat_,
    rotation,
    roughness,
    round',
    selector,
    scale,
    src,
    transparent,
    vec3,
    wasdControlsEnabled,
    width,
    -- * Kevin's 'kframe-extras' 
    animation,
    look_at,
    -- * Property builder sub-DSL
    List,
    Single,
    -- * DSL classes
    ToProperty,
    toProperty,
    Entity,
    primitive,
    Component,
    component,
    Attributes, 
    -- * Variable Types
    Color,
    rgb,
    Number,
    number,
    -- * Unique Property generator
    uniqId,
    -- * Pretty Printer for DSL
    showAsDSL,
    -- * Others
    Attribute,
    Property,
    Label
  ) where



import Control.Monad
import Data.Text(Text,unpack,pack)
import qualified Data.Text as T
import Data.Monoid
import Data.String
import Text.AFrame

---------------------------------------------------------------------------------

class Component f where
  component :: ToProperty c => Label -> c -> f ()

class Attributes f where
  attribute :: ToProperty c => Label -> c -> f ()

class (Attributes f, Component f) => Entity f where
  primitive :: Label -> f a -> f a

---------------------------------------------------------------------------------
-- Primitive DSL

newtype DSL a = DSL { runDSL :: Int -> (a,Int,[Attribute],[AFrame]) }

instance Functor DSL where
  fmap f g = pure f <*> g

instance Applicative DSL where
  pure = return
  (<*>) = ap

instance Monad DSL where
  return a = DSL $ \ i -> (a,i,[],[])
  DSL m >>= k2 = DSL $ \ i0 -> case m i0 of
     (r1,i1,as1,af1) -> case runDSL (k2 r1) i1 of
                             (r2,i2,as2,af2) -> (r2,i2,as1 ++ as2,af1 ++ af2)

instance Entity DSL where
  primitive :: Label -> DSL a -> DSL a
  primitive (Label nm) m = DSL $ \ i0 -> case runDSL m i0 of
     (r1,i1,as1,af1) -> (r1,i1,[],[AFrame (Primitive nm) as1 af1])

instance Component DSL where
  component :: ToProperty c => Label -> c -> DSL ()
  component lab c = DSL $ \ i0 -> ((),i0,[(lab,toProperty c)],[])

instance Attributes DSL where
  attribute :: ToProperty c => Label -> c -> DSL ()
  attribute lab c = DSL $ \ i0 -> ((),i0,[(lab,toProperty c)],[])

uniqId :: DSL Property
uniqId = DSL $ \ i -> (Property (pack $ "id_" ++ show i),i+1,[],[])

scene :: DSL () -> AFrame
scene m = case runDSL (primitive "a-scene" m) 0 of
             (_, _, [], [f]) -> f
             (_, _, _,  [] ) -> error "scene internal error: no top-level primitive"
             (_, _, _,  [_]) -> error "scene internal error: top-level attribute"
             (_, _, _,  _  ) -> error "scene internal error: to many top-level primitives"


---------------------------------------------------------------------------------
-- Properties DSL

data List x a where
  List :: [x] -> a -> List x a
  
instance Functor (List x) where
  fmap f g = pure f <*> g

instance Applicative (List x) where
  pure = return
  (<*>) = ap

instance Monad (List x) where
  return = List []
  List ps a >>= k = case k a of
                      List ps' a' -> List (ps ++ ps') a'

instance ToProperty (List Attribute ()) where
  toProperty (List xs ()) = packProperty xs

instance Attributes (List Attribute) where
  attribute lab c = List [(lab,toProperty c)] ()

instance IsString (List Attribute ()) where
  fromString str = List (unpackProperty $ Property  $ pack $ str) ()

---------------------------------------------------------------------------------------------------------
-- Single DSL, with no monadic support (by design)

data Single x a where
  Single :: x -> Single x ()

instance ToProperty (Single Attribute ()) where
  toProperty (Single x) = packProperty [x]

instance Attributes (Single Attribute) where
  attribute lab c = Single (lab,toProperty c)

instance Component (Single Attribute) where
  component lab c = Single (lab,toProperty c)

---------------------------------------------------------------------------------------------------------
-- Primitives

entity :: DSL a -> DSL a
entity = primitive "a-entity"

assets :: DSL a -> DSL a
assets = primitive "a-assets" 

box :: DSL a -> DSL a
box = primitive "a-box"

camera :: DSL a -> DSL a
camera = primitive "a-camera"

collada_model :: DSL a -> DSL a
collada_model = primitive "a-collada-model"

cone :: DSL a -> DSL a
cone = primitive "a-cone"

cursor :: DSL a -> DSL a
cursor = primitive "a-cursor"

curvedimage :: DSL a -> DSL a
curvedimage = primitive "a-curvedimage"

cylinder :: DSL a -> DSL a
cylinder = primitive "a-cylinder"

image :: DSL a -> DSL a
image = primitive "a-image"

light :: DSL a -> DSL a
light = primitive "a-light"

obj_model :: DSL a -> DSL a
obj_model = primitive "a-obj-model"

plane :: DSL a -> DSL a
plane = primitive "a-plane"

ring :: DSL a -> DSL a
ring = primitive "a-ring"

sky :: DSL a -> DSL a
sky = primitive "a-sky"

sphere :: DSL a -> DSL a
sphere = primitive "a-sphere"

torus :: DSL a -> DSL a
torus = primitive "a-torus"

video :: DSL a -> DSL a
video = primitive "a-video"

videosphere :: DSL a -> DSL a
videosphere = primitive "a-videosphere"

---------------------------------------------------------------------------------------------------------
-- Assets

img :: DSL a -> DSL a
img = primitive "img"

---------------------------------------------------------------------------------------------------------
-- Components

fog :: Component k => List Attribute () -> k ()
fog = component "fog"

geometry :: Component k => List Attribute () -> k ()
geometry = component "geometry"

material :: Component k => List Attribute () -> k ()
material = component "material"

stats :: Component k => k ()
stats = component "stats" ()

template :: Component k => List Attribute () -> k ()
template = component "template"

wasd_controls :: Component k => List Attribute () -> k ()
wasd_controls = component "wasd-controls"

------------------------------------------------------
-- Attributes

attribute_ :: Attributes k => Text -> k ()
attribute_ = attribute "attribute"

begin :: Attributes k => Int -> k ()
begin = attribute "begin"

color :: Attributes k => Color -> k ()
color = attribute "color"

delay :: Attributes k => Number -> k ()
delay = attribute "delay"

depth :: Attributes k => Number -> k ()
depth = attribute "depth"

-- | dir "normal" | "alternative" | "reverse"
dir :: Attributes k => Text -> k ()
dir = attribute "dir"

dur :: Attributes k => Number -> k ()
dur = attribute "dur"

easing :: Attributes k => Text -> k ()
easing = attribute "easing"

elasticity :: Attributes k => Number -> k ()
elasticity = attribute "elasticity"

fill :: Attributes k => Text -> k ()
fill = attribute "fill"

fov :: Attributes k => Number -> k ()
fov = attribute "fov"

height :: Attributes k => Number -> k ()
height = attribute "height"

id_ :: Attributes k => Text -> k ()
id_ = attribute "id" 

lookControlsEnabled :: Attributes k => Bool -> k ()
lookControlsEnabled = attribute "look-controls-enabled" 

loop :: Attributes k => Maybe Int -> k ()
loop (Just n) = attribute "loop" n
loop Nothing  = attribute "loop" True

metalness :: Attributes k => Number -> k ()
metalness = attribute "metalness"

open :: Attributes k => Bool -> k ()
open = attribute "open"

opacity :: Attributes k => Number -> k ()
opacity = attribute "opacity"

position :: Attributes k => (Number,Number,Number) -> k ()
position = attribute "position"

selector :: Attributes k => Text -> k ()
selector = attribute "selector"

radius :: Attributes k => Number -> k ()
radius = attribute "radius"

radiusTop :: Attributes k => Number -> k ()
radiusTop = attribute "radius-top"

radiusBottom :: Attributes k => Number -> k ()
radiusBottom = attribute "radius-bottom"

repeat_ :: Attributes k => Text -> k ()
repeat_ = attribute "repeat"

rotation :: Attributes k => (Number,Number,Number) -> k ()
rotation = attribute "rotation"

roughness :: Attributes k => Number -> k ()
roughness = attribute "roughness"

round' :: Attributes k => Bool -> k ()
round' = attribute "round"

scale :: Attributes k => (Number,Number,Number) -> k ()
scale = attribute "scale"

src :: Attributes k => Text -> k ()
src = attribute "src"

transparent :: Attributes k => Bool -> k ()
transparent = attribute "transparent"

vec3 :: Attributes k => (Number,Number,Number) -> k ()
vec3 = attribute "vec3"

wasdControlsEnabled :: Attributes k => Bool -> k ()
wasdControlsEnabled = attribute "wasd-controls-enabled" 

width :: Attributes k => Number -> k ()
width = attribute "width"

------------------------------------------------------
-- Pretty Printer

showAsDSL :: AFrame -> String
showAsDSL (AFrame p0 as fs) = 
    showPrimitiveAsDSL p0 ++ " $ do\n" ++
    indent 2 (unlines (
        map showAttributeAsDSL as ++
        map showAsDSL fs))
  where
    showPrimitiveAsDSL :: Primitive -> String
    showPrimitiveAsDSL (Primitive "a-scene") = "scene"
    showPrimitiveAsDSL (Primitive p) | "a-" `T.isPrefixOf` p = drop 2 $ unpack p
    showPrimitiveAsDSL (Primitive p) = unpack p

    indent :: Int -> String -> String
    indent n = unlines . map (take n (repeat ' ') ++) . lines

    showAttributeAsDSL :: Attribute -> String
    showAttributeAsDSL (Label l,Property p) 
        | l `elem` ["width","height","radius"] = case () of
              _ | "-" `T.isPrefixOf` p -> unpack l ++ " (" ++ unpack p ++ ")"
              _                        -> unpack l ++ " " ++ unpack p
        | l `elem` ["position","rotation"] = case words $ unpack p of
            [a,b,c] -> unpack l ++ " (" ++ a ++ "," ++ b ++ "," ++ c ++ ")"
            _ -> def
        | l `elem` ["template"] = case unpackProperty (Property p) of
              xs -> unpack l ++ " $ do\n" ++
                      indent 2 (unlines (map showAttributeAsDSL xs))
        | otherwise = def
      where def = unpack l ++ " " ++ show (unpack p)

------------------------------------------------------
-- Color

newtype Color = Color Text

instance Show Color where
  show (Color c) = show c

instance ToProperty Color where
  toProperty (Color e) = Property $ e

instance IsString Color where
  fromString = Color . pack
  
-- | color takes three values, rgb, between 0 and 1
rgb :: RealFrac a => (a,a,a) -> Color
rgb (r,g,b) = fromString $ "rgb(" ++ show(round(r*255) :: Int) ++ "," ++ show (round(g*255) :: Int ) ++ "," ++ show (round(b*255) :: Int) ++ ")"

------------------------------------------------------
-- Numbers

type Number = Double

number :: Real a => a -> Number
number = fromRational . toRational

------------------------------------------------------
-- Animation support (aframe-animation-component)

animation :: (ToProperty a, Component k) => (a -> Single Attribute ()) -> a -> a -> List Attribute () -> k ()
animation k from' to' m = component (Label ("animation__" <> T.map f nm)) $ do
                           attribute "property" nm
                           attribute "from" from'
                           attribute "to"   to'
                           m
  where
      Single (Label nm,_) = k from'
      f '.' = '-'
      f c   = c

-- | 'look_at' takes a selector (or a vec3 in string format)
look_at :: Component k => Property -> k ()
look_at = component "look-at"

------------------------------------------------------