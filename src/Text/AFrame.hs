{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, ScopedTypeVariables, KindSignatures, GADTs, InstanceSigs, TypeOperators, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module Text.AFrame 
 (  -- * Principal Types
    AFrame(..)
  , Primitive(..)
  , Label(..)
  , Property(..)
  , Attribute(..)
  , Path(..)
    -- * Attribute setter/getter
  , setAttribute
  , getAttribute
  , resetAttribute
   -- * render and parse AFframe
  , showAFrame
--  , readAFrame
    -- * Property Builder
  , ToProperty(..)
    -- * nested Property
  , unpackProperty
  , packProperty
  ) where
 

import Control.Applicative

import Data.Char (isSpace)
import Data.Map(Map)
import Data.String
import Data.Text(Text,pack,unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe (listToMaybe)
import Data.List as L
import Data.Monoid ((<>))
import Data.Monoid
--import qualified Text.Taggy as T
--import qualified Text.Taggy.Renderer as T
import qualified Data.HashMap.Strict as H
import Numeric
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.Blaze (Markup, (!))
import Text.Blaze.Internal (customParent, textTag, customAttribute, textValue, preEscapedText)

-- | 'AFrame' describes the contents of an a-frame scene,
--   and is stored as a classical rose tree.
--   'AFrame' follows the DOM, except there are no textual  
--   content; it is tags all the way down. 
--
--   An exception is that \<script>ABC\</script> is encoded using 
--  \<script text=\"ABC\">\</script>

data AFrame       = AFrame Primitive [Attribute] [AFrame]
  deriving (Show, Eq)

newtype Primitive = Primitive Text
  deriving (Show, Eq, Ord, IsString)

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString)
  
newtype Property  = Property Text
  deriving (Show, Eq, Ord, IsString)

type Attribute = (Label,Property)

-- | A valid css or jquerty-style path, in Haskell from.
--   An example of the string form might be
--     $('a-scene > a-entity:nth-of-type(2) > a-collada-model:nth-of-type(1) > a-animation:nth-of-type(1)')
--  
--  Note that the number offset of 1-based (1 is the first)
data Path = Path Primitive [(Int,Primitive)]
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------------------------

setAttribute :: Label -> Property -> AFrame -> AFrame
setAttribute lbl prop (AFrame p as af) = AFrame p ((lbl,prop) : [ (l,p) | (l,p) <- as, l /= lbl ]) af

getAttribute :: Label -> AFrame -> Maybe Property
getAttribute lbl (AFrame p as af) = lookup lbl as

resetAttribute :: Label -> AFrame -> AFrame
resetAttribute lbl (AFrame p as af) = AFrame p [ (l,p) | (l,p) <- as, l /= lbl ] af

-------------------------------------------------------------------------------------------------

--setPath :: Path -> Label -> Property -> AFrame -> AFrame

--getPath :: Path -> Label -> AFrame -> Mabe Property

-------------------------------------------------------------------------------------------------

-- | 'aFrameToMarkup' converts an 'AFrame' to an Blaze Markup'. Total.
aFrameToMarkup :: AFrame -> Markup
aFrameToMarkup (AFrame prim attrs rest) =
    foldl (!) (customParent tag) attrs' $
    mconcat rest'
  where
    Primitive prim' = prim
    tag = textTag prim'
    attrs'          = [ customAttribute (textTag a) (textValue p)
                      | (Label a,Property p) <- attrs 
                      , not (prim' == "script" && a == "text")
                      ]
    rest'           = [ preEscapedText p
                      | (Label "text",Property p) <- attrs 
                      , prim' == "script" 
                      ]
                   ++ map aFrameToMarkup rest

{-
-- | 'aFrameToElement' converts an (HTML) 'Element' to an 'AFrame'. Total.
-- Strips out any text (which is not used by 'AFrame' anyway.)
elementToAFrame :: Element -> AFrame
elementToAFrame ele = AFrame prim' attrs' content'
  where
    prim'    = Primitive $ eltName $ ele
    attrs'   = [ (Label a,Property p)| (a,p) <- H.toList $ eltAttrs ele ]
            ++ [ (Label "text",Property txt)| NodeContent txt <- eltChildren ele ]
    content' = [ elementToAFrame ele' | NodeElement ele' <- eltChildren ele ]
-}    

{-
-- | reads an aframe document. This can be enbedded in an XML-style document (such as HTML)
readAFrame :: String -> Maybe AFrame
readAFrame str = do
    let doms = parseDOM True (LT.fromStrict $ pack str)
    case doms of
      (NodeElement dom:_) -> do
        let aframe  = elementToAFrame dom
        findAFrame aframe
      _ -> error $ show ("found strange DOM",doms)
  where 
    findAFrame :: AFrame -> Maybe AFrame
    findAFrame a@(AFrame (Primitive "a-scene") _ _) = return a
    findAFrame (AFrame _ _ xs) = listToMaybe
      [ x
      | Just x <- map findAFrame xs
      ]
-}

showAFrame :: AFrame -> String
showAFrame = renderMarkup . aFrameToMarkup


data AFrameUpdate = AFrameUpdate 
    { aframePath     :: Path
    , aframeLabel    :: Label
    , aframeProperty :: Property
    }

{-
    compareAFrame :: AFrame -> AFrame -> Maybe [([Text],Attribute)]
compareAFrame aframe1 aframe2 = fmap (fmap (\ (xs,a) -> (intercalate " > " xs,a))) 
    $ deltaAFrame aframe1 aframe2
-}
deltaAFrame :: AFrame -> AFrame -> Maybe [(Path,Attribute)]
deltaAFrame (AFrame p1@(Primitive primName) attrs1 aframes1)
             (AFrame p2 attrs2 aframes2)
      | p1 /= p2 = fail "element name does not match in deltasAFrame"
      | length aframes1 /= length aframes2
                 = fail "sub elements count do not match in deltasAFrame"          
      | otherwise = do
              attrsD <- fmap (\ a -> (Path p1 [],a)) <$> deltaAttributes attrs1 attrs2
              let ps = [ p | AFrame p _ _ <- aframes1 ]
                  xs = [ length [ () | x' <- xs, x' == x ] | (x:xs) <- tail $ scanl (flip (:)) [] ps ]
              aframesD <- concat <$> sequence
                    [ do ds <- deltaAFrame a1 a2
                         return $ fmap (\ (Path p ps,at) -> (Path p1 ((x,p):ps),at)) ds
                    | (a1,a2,x) <- zip3 aframes1 aframes2 xs
                    ]
              return $ attrsD ++ aframesD

deltaAttributes :: [Attribute] -> [Attribute] -> Maybe [Attribute]
deltaAttributes xs ys | length xs /= length ys = fail "different number of arguments for deltaAttributes"
deltaAttributes xs ys = concat <$> sequence [ deltaAttribute x y | (x,y) <- xs `zip` ys ]
  
deltaAttribute :: Attribute -> Attribute -> Maybe [Attribute]
deltaAttribute attr1@(lbl1,_) attr2@(lbl2,_)
  | attr1 == attr2 = return []       -- same result
  | lbl1 == lbl2   = return [attr2]  -- true update
  | otherwise      = fail "labels do not match in deltaAttributes"

------------------------------------------------------------------------------------------

unpackProperty :: Property -> [(Label,Property)]
unpackProperty (Property prop) = 
      [ (Label (T.dropWhile isSpace l), Property (T.dropWhile (\ c -> isSpace c || c == ':') p))
      | (l,p) <- map (T.span (/= ':')) (T.splitOn ";" prop) 
      , not (T.null p)
      ]

packProperty :: [(Label,Property)] -> Property
packProperty = Property 
             . T.intercalate "; " 
             . map (\ (Label lbl,Property txt) -> lbl <> ": " <> txt)

------------------------------------------------------------------------------------------

preOrderFrame :: Monad m => (AFrame -> m AFrame) -> AFrame -> m AFrame
preOrderFrame f af = do
  AFrame prim attrs aframes <- f af
  aframes' <- traverse (preOrderFrame f) aframes
  return $ AFrame prim attrs aframes'

-- This finds \<script src=\"...\"> and inserts the text=\"..\" into the \<script>.
resolveScript :: Monad m => (Text -> m LT.Text) -> AFrame -> m AFrame
resolveScript rf  = preOrderFrame fn
  where 
    fn af@(AFrame "script" attrs aframes) = case lookup "src" attrs of
      Nothing -> return af
      Just (Property path) ->
            do txt <- rf path
               return $ AFrame "script" 
                         ((Label "text",Property (LT.toStrict txt))
                             : [(l,p) | (l,p) <- attrs, l `notElem` ["src","text"]]
                         )
                         aframes
    fn af = return af
{-
instantiateTemplates :: Monad m => ([Attribute] -> AFrame -> m AFrame) -> AFrame -> m AFrame
instantiateTemplates f root = preOrderFrame fn root
  where
    fn (aEntity@(AFrame "a-entity" attrs aframes)) = case lookup "template" attrs of
          Nothing -> return aEntity
          Just templ -> case lookup "src" (unpackProperty templ) of
            Nothing -> return aEntity
            Just (Property src) | T.take 1 src == "#" -> 
              case getElementById root (T.drop 1 src) of
                Just (script@(AFrame "script" attrs _)) -> do
                    txt <- f attrs script
                    return aEntity
                _ -> return aEntity  -- id not found
    fn af = return af
-}

---------------------------------------------------------------------------------------------------------
-- ToProperty overloadings

class ToProperty c where
  toProperty :: c -> Property

instance ToProperty Text where
  toProperty = Property

instance ToProperty Property where
  toProperty = id

instance ToProperty (Double,Double,Double) where
  toProperty (a,b,c) = Property $ pack $ unwords $ map show' [a,b,c]
   where show' v = showFFloat Nothing v ""

instance ToProperty Double where
  toProperty = Property . pack . show' 
   where show' v = showFFloat Nothing v ""

instance ToProperty Int where
  toProperty = Property . pack . show

instance ToProperty () where
  toProperty () = Property ""

instance ToProperty Bool where
  toProperty True  = Property "true"
  toProperty False = Property "false"
