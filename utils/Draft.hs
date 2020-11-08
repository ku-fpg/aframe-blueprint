module Main where
    
import Text.AFrame
import Text.AFrame.DSL

main = print "readAFrame no longer supported"
{-    
    txt <- getContents
    case readAFrame txt of
      Nothing -> error "bad parse"
      Just af -> putStrLn $ showAsDSL $ af
-}