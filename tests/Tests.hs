{-# LANGUAGE GADTs, ScopedTypeVariables, InstanceSigs, OverloadedStrings #-}
module Main where
	
import Control.Applicative ((<|>))
import Control.Monad  

import Data.Monoid ((<>))
import Debug.Trace

import System.Random

import Text.AFrame.Geometry

import Test.QuickCheck

import System.IO.Unsafe

import Text.AFrame.WebPage 
import qualified Text.AFrame.DSL as DSL


------------------------------------------------------------------------------------
-- Testing

draw :: Double -> Char
draw n | n > 1.5 = '#'
       | n > 0.5 = '*'
       | otherwise = last $ show (round n :: Int)


splat :: ((Double,Double) -> Maybe Char) -> String
splat f = unlines 
        [ [ case f (x,y) of
              Nothing -> '.'
              Just c -> c
          | x <- fmap (*2) $ fmap (/xsz) $ fmap (subtract xsz) $ [0..(xsz*2)]
          ]
        | y <- reverse $ fmap (*2) $ fmap (/ysz) $ fmap (subtract ysz) $ [0..(ysz*2)]
        ]
 where
    xsz = 50
    ysz = 20

-- Quad goes counter-clock
plane :: Fractional a => a -> a -> Quad a
plane w h = Quad (Position (-w/2,h/2,0)) (Position (-w/2,-h/2,0)) (Position (w/2,-h/2,0)) (Position (w/2,h/2,0))

-- For testing

instance (Random a, Floating a, Arbitrary a) =>  Arbitrary (Rotation a) where
  arbitrary = (\ a b c -> Rotation (a,b,c))
                     <$> choose (-360,360)
                     <*> choose (-360,360)
                     <*> choose (-360,360)

-- Two never-indeterminate quads
data Pair a = Pair (Quad a) (Quad a)
 deriving Show

instance Geometric Pair where
  run :: Floating a => Geometry a -> Pair a -> Pair a
  run g (Pair q1 q2) = Pair (run g q1) (run g q2)

instance (Random a, Floating a, Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = (\ (Rotation r1) (Rotation r2) -> Pair (run (position (-1.5,0,0) <> rotation YXZ r1) $ plane 2 2)
			       (run (position ( 1.5,0,0) <> rotation YXZ r2) $ plane 2 2))
                     <$> arbitrary
                     <*> arbitrary

-- Check comparePolygon function
prop_compare (pair :: Pair Double) (Rotation cr :: Rotation Double) =
--  trace (splat $ fmap (fmap draw) $ intersectQuads [p1,p2]) False
    forAll (elements points) $ \ (x,y) ->
    case (intersectQuad p1 (x,y), intersectQuad p2 (x,y)) of
      (Just z1,Just z2) -> case cmp of
        Behind          -> True ==> if z1 < z2 then True else trace (msg (x,y) z1 z2) False
        InFront         -> True ==> if z1 > z2 then True else trace (msg (x,y) z1 z2) False
        Disjoint        -> False ==> True
        Indeterminate   -> True ==> unsafePerformIO $ failed (x,y) z1 z2 
				 -- trace (msg (x,y) z1 z2) False -- error "should never be indeterminate"
      _                 -> False ==> True
   where
     failed :: (Double,Double) -> Double -> Double -> IO Bool
     failed (x,y) z1 z2 = do
	     webPage ["fail.html"]  $ DSL.scene $ do
		     DSL.cylinder $ do
			 DSL.color "green"
			 DSL.height 10
			 DSL.radius 0.05
			 DSL.rotation (-90,0,0)
			 DSL.position (f x,f y,-5)
		     face p1 "red"
		     face p2 "blue"
		     return ()
	     return False

     face (Quad a1 a2 a3 a4) col = do
	     sequence_ 
	        [ DSL.sphere $ do
		    DSL.color col
		    DSL.radius 0.1
		    DSL.position (f x,f y,f z)
		| Position (x,y,z) <- [a1,a2,a3,a4]
		]

     f = fromRational . toRational
     
     Pair p1 p2 = run common pair

     -- ((x,y),z1,z2,cmp)
     msg (x,y) z1 z2 = "\n" ++ 
       unlines [ "x: " ++ show x
               , "y: " ++ show y
               , "z1: " ++ show z1
               , "z2: " ++ show z2
               , "cmp: " ++ show cmp
               , "p1: " ++ show p1
               , "p2: " ++ show p2
               ] ++ (splat $ fmap (fmap draw) $ intersectQuads (if cmp==Behind then [p2,p1] else [p1,p2]))
                 ++ "\n"


     -- splat $ fmap (fmap draw) $ intersectQuads [p2,p1])


     points :: [(Double,Double)] 
     points = (0,0) : [(x,y) | Position (x,y,z) <- path p1 ++ path p2 ]

     common :: Geometry Double
     common = position (0,0,-5) <> rotation YXZ cr
{-
     p1 :: Quad Double
     p1 = run (common <> position (-1.5,0,0) <> rotation YXZ lr) $ plane 2 2

     p2 :: Quad Double
     p2 = run (common <> position ( 1.5,0,0) <> rotation YXZ rr) $ plane 2 2
-}      
     cmp :: Stacking
     cmp = comparePolygon p1 p2

qtest = quickCheckWith stdArgs { maxSuccess = 10000, maxDiscardRatio = 1000, chatty = True } prop_compare

intersectQuads :: (Ord a, Fractional a) => [Quad a] -> (a,a) -> Maybe a
intersectQuads qs p = head $ [ Just r | Just r <- map (\ q' -> intersectQuad q' p) qs ] ++ [Nothing]
	
	
main = do	
	qtest