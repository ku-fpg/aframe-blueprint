module Text.AFrame.WebPage where
        
import Data.List as L

import Text.AFrame

import Paths_aframe_blueprint

webPage :: [String] -> AFrame -> IO ()        
webPage [fileOut] af = do
    fileIn <- getDataFileName "static/index.html"
    file <- readFile fileIn
    writeFile fileOut $ injectAFrame af $ file

-- | inject 'AFrame' into an existing (HTML) file. Replaces complete "<a-scene>" element.
injectAFrame :: AFrame -> String -> String
injectAFrame aframe str = findScene str 0 
  where
    openTag  = "<a-scene"
    closeTag = "</a-scene>"

    findScene :: String -> Int -> String
    findScene xs     n | openTag `L.isPrefixOf` xs = insertScene (drop (length openTag) xs) n
    findScene (x:xs) n =
       case x of
         ' '  -> x : findScene xs (n+1)
         _    -> x : findScene xs 0
    findScene [] n = []

    insertScene :: String -> Int -> String
    insertScene xs n = unlines (s : map (spaces ++) (ss ++ [remainingScene xs]))
     where
       (s:ss) = lines $ showAFrame $ aframe
       spaces = take n $ repeat ' '

    -- This will mess up if the closeTag strict appears in the scene.
    remainingScene :: String -> String
    remainingScene xs | closeTag `L.isPrefixOf` xs = drop (length closeTag) xs
    remainingScene (x:xs) = remainingScene xs
    remainingScene []     = []  
  
