module Text.AFrame.WebPage
 ( webPage
 , Component
 , aframe
 , unpkg
 ) where
        
import Data.List as L

import Text.AFrame

import Paths_aframe_blueprint

type Component = String

-- | webPage takes a target file, and a list of libraries to include, as well as a AFrame.
webPage :: String -> [Component] -> AFrame -> IO () 
webPage = webPageFromTemplate "static/index.html"

type Version = String -- 0.9.2

aframe :: Version -> Component
aframe = ("https://aframe.io/releases/" ++) . (++ "/aframe.min.js")

unpkg :: String -> Version -> Component
unpkg lib ver = "https://unpkg.com/" ++ lib ++ "@" ++ ver ++ "/dist/" ++ lib ++ ".min.js"

-- | webPageFromTemplate takes a template, a target file, and a list of libraries to include, as well as a AFrame.
webPageFromTemplate :: String -> String -> [String] -> AFrame -> IO ()
webPageFromTemplate template fileOut libraries af = do
    fileIn <- getDataFileName template 
    file <- readFile fileIn
    writeFile fileOut $ injectComponents libraries $ injectAFrame af $ file

-- | inject libraries into into an existing (HTML) file.
injectComponents :: [String] -> String -> String
injectComponents libraries str = findCloseHead str 0 
  where
    openTag  = "</head>"
    ls = unlines
       [ "<script src=\"" ++ lib ++ "\"></script>"
       | lib <- libraries
       ]

    findCloseHead :: String -> Int -> String
    findCloseHead xs     n | openTag `L.isPrefixOf` xs = ls ++ xs
    findCloseHead (x:xs) n =
       case x of
         ' '  -> x : findCloseHead xs (n+1)
         _    -> x : findCloseHead xs 0
    findCloseHead [] n = []
  

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
  
