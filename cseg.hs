{-# LANGUAGE FlexibleInstances,FlexibleContexts, DeriveGeneric, DeriveAnyClass, BangPatterns, DeriveDataTypeable, ExistentialQuantification #-}

-- ghc --make -threaded -rtsopts -with-rtsopts="-N" -O2 cseg.hs 
-- ./cseg -d/home/vidas/acl2/corpus/split/en -m -nen -atrain -e".txt"
-- ./cseg -d/home/vidas/acl2/corpus/split/en -m -t -cmajority -nen -aseg -e".txt"

import Control.Monad
import qualified Data.List as L
import Data.Char
import System.IO
import System.Directory
import Control.DeepSeq
import qualified Control.Monad.Parallel as MP
import GHC.Generics (Generic)
import qualified SentenceTex as Sen
import qualified Control.Parallel.Strategies as Par
import qualified Data.Map.Strict as M
import System.Mem
import System.Console.CmdArgs    -- cabal install cmdargs

data Dict = Dict !String !Int
           deriving (Show,Ord,Eq,Generic,NFData)

data Cseg = Cseg
    {extension :: String
    ,action :: String
    ,datadir :: FilePath
    ,ngramfile :: FilePath
    ,makeSegList :: Bool
    ,combinability :: String
    ,statsAppend :: Bool
    ,useram :: Bool
    ,tokenize :: Bool
    }
    deriving (Data,Typeable,Show,Eq)

cseg :: Cseg
cseg = Cseg
    {extension = def &= typ "EXT" &= help "File extension to search" &= opt ".txt" 
    ,action = def &= typ "ACTION" &= opt "seg" &= help "Perform an action (seg|train). seg is for segmentation, train is to build ngram files from corpus data."
    ,datadir = def &= typDir &= opt "corpus" &= help "Data directory."
    ,ngramfile = def &= typFile &= opt "ngram" &= help "The path to store to or read from ngram data (defaults to ngram). Suffixes .uni and .bi will be appended automatically."
    ,makeSegList = def &= help "Make and save frequency lists of segmented texts."
    ,combinability = def &= typ "METHOD" &= opt "majority" &= help "Combinability measure between two tokens (dice|pmi|ttest|gravity|majority)."
    ,statsAppend = def &= help "Append statistics of segmented text."
    ,useram = def &= help "Load bigram statistics into memory during segmentation."
    ,tokenize = def &= help "Tokenize texts. Otherwise, only whitespace is used"
    } &=
    verbosity &=
    help "Collocation segmentation" &=
    summary "cseg v0.0.1, (C) Vidas Daudaravicius" &=
    details ["cseg is a tool to apply collocation segmentation to sequences of tokens that can be words, phrases or single characters."]

main :: IO ()
main = do
         argsC <- cmdArgs cseg
         case (action argsC) of
             "train" ->  makeDicts (datadir argsC) (ngramfile argsC) (extension argsC) argsC
             "seg" -> segmentData argsC
             _ -> error "Unknown action. Should be (train|seg)."

makeDicts :: [Char] -> [Char] -> [Char] -> Cseg -> IO ()
makeDicts path fOut ext argsC = do
         putStrLn $ path ++ "\t" ++ fOut ++ "\t" ++ ext

         files <- getFiles [path] ext
         d <- return $ L.sort files
         putStrLn $ "unigrams: "
         (us1,nn1) <- foldM (\ (b1,n) xs1 -> do
                                   bTsT <- MP.mapM (\ i -> do
                                                           txt <- readFileV i
                                                           di <- if (tokenize argsC)
                                                                   then return $!! makeDictFromTextUni $ filter (not.null) $ map (unwords.(Sen.tokenizeC).lc) $ lines $ Sen.sentencesT txt
                                                                   else return $!! makeDictFromTextUni $ filter (not.null) $ lines $ txt
                                                           return di
                                                  ) xs1
                                   bTs <- return $!! sumDicts bTsT
                                   bT <- return $!! sumLists b1 bTs
                                   performGC
                                   putStrLn $ "Files: 50 Temporary dict increment" ++ (show $ n) ++  " Dictonary size: " ++ (show $ length bT)
                                   hFlush stdout
                                   if length bT > 50000
                                   then do
                                           writeFileV (fOut ++ "." ++ (show n) ++ ".uni") $ dictToStr $ bT
                                           performGC
                                           return ([],n+1)
                                   else return $!! (bT,n)
                                   return $!! (bT,n)
                    ) ([],1::Int) $ splitInto 50 d
         performGC
         putStrLn ""
         writeFileV (fOut ++ "." ++ (show nn1) ++ ".uni") $ dictToStr $ us1
         performGC
         putStrLn $ "Merging temporary unigram files:"
         writeFileV (fOut ++ ".uni") ""
         mapM_ (\n ->
                   do
                     putStrLn (fOut ++ "." ++ (show n) ++ ".uni")
                     txt1 <- readFileV (fOut ++ ".uni")
                     txt2 <- readFileV (fOut ++ "." ++ (show n) ++ ".uni")
                     hout <- openFile (fOut ++ ".T" ++ ".uni") WriteMode
                     hSetNewlineMode hout noNewlineTranslation
                     sumFileUnigrams (lines txt1) (lines txt2) hout
                     hFlush hout
                     hClose hout
                     removeFile (fOut ++ "." ++ (show n) ++ ".uni")
                     removeFile (fOut ++ ".uni")
                     renameFile (fOut ++ ".T" ++ ".uni") (fOut ++ ".uni")
               ) [1..nn1]

         putStrLn $ "bigrams: "
         (us2,nn2) <- foldM (\ (b1,n) xs1 -> do

                                   bTsT <- MP.mapM (\ i -> do
                                                           txt <- readFileV i
                                                           di <- if (tokenize argsC)
                                                                   then return $!! makeDictFromTextBi $ filter (not.null) $ map (unwords.(Sen.tokenizeC).lc) $ lines $ Sen.sentencesT txt
                                                                   else return $!! makeDictFromTextBi $ filter (not.null) $ lines $ txt
                                                           return di
                                                  ) xs1
                                   bTs <- return $!! sumTreesB bTsT
                                   performGC



                                   bT <- return $!! sumTrees b1 bTs
                                   performGC
                                   putStrLn $ "Files: 50 Temporary dict increment" ++ (show $ n) ++  " Dictonary size: " ++ (show $ length bT)
                                   hFlush stdout
                                   if length bT > 50000
                                   then do
                                           writeFileV (fOut ++ "." ++ (show n) ++ ".bi") $ unlines $ map (\(w,ws)-> w ++ "\t" ++ (init $ concat $ map (\(w2,f)-> w2 ++ " " ++ (show f) ++ "\t") $ M.toList ws)) $ bT
                                           performGC
                                           return ([],n+1)
                                   else return $!! (bT,n)
                    ) ([],1::Int) $ splitInto 50 d
         performGC
         putStrLn ""
         writeFileV (fOut ++ "." ++ (show nn2) ++".bi") $ unlines $ map (\(w,ws)-> w ++ "\t" ++ (init $ concat $ map (\(w2,f)-> w2 ++ " " ++ (show f) ++ "\t") $ M.toList ws)) $ us2
         performGC
         putStrLn $ "Merging temporary bigram files:"
         writeFileV (fOut ++ ".bi") ""
         mapM_ (\n ->
                   do
                     putStrLn (fOut ++ "." ++ (show n) ++ ".bi")
                     txt1 <- readFileV (fOut ++ ".bi")
                     txt2 <- readFileV (fOut ++ "." ++ (show n) ++ ".bi")
                     hout <- openFile (fOut ++ ".T" ++ ".bi") WriteMode
                     hSetNewlineMode hout noNewlineTranslation
                     sumFileTrees (lines txt1) (lines txt2) hout
                     hFlush hout
                     hClose hout
                     removeFile (fOut ++ "." ++ (show n) ++ ".bi")
                     removeFile (fOut ++ ".bi")
                     renameFile (fOut ++ ".T" ++ ".bi") (fOut ++ ".bi")
               ) [1..nn2]

----------------------------------------------------------------------
segmentData :: Cseg -> IO ()
segmentData argsC = do
         putStrLn $ (datadir argsC) ++ "\t" ++ (ngramfile argsC) ++ "\t" ++ (extension argsC)
         u1 <- readFileV $ (ngramfile argsC) ++ ".uni"
         hBi <- openFile  ((ngramfile argsC) ++ ".bi") ReadMode
         hSetNewlineMode hBi noNewlineTranslation
         
         uniT <- return $!! map (\ln -> (\(w:f:_) -> (w,((read f::Int))) ) $ lines $ map (tabToNl) ln) $ lines u1
         n <- return $!! sum $ map (toEnum.snd) uniT
         uni <- return $!! M.fromList $ uniT
         cmb <- return $ case combinability argsC of
                               "dice" -> dice
                               "pmi" -> pmi
                               "gravity" -> gravity
                               "ttest" -> tscore
                               _ -> dice
         files <- getFiles [(datadir argsC)] (extension argsC)
         d <- return $ L.sort files
         gs <- if useram argsC  -- the return is a partial function which is used as an argument for bigram queries, and depends on where data is located (RAM or disk). 
                           then do
                                   bi <- loadBigrams ((ngramfile argsC) ++ ".bi")   -- load unigrams into RAM and use bigram data from disk. Slow, but little memory used.
                                   return (getStatsRam bi argsC)                    -- load all data into RAM. Very fast, but lots of memory used.
                           else return (getStats2 ((ngramfile argsC) ++ ".bi") argsC)
         mapM_ (\fs -> do
                        MP.mapM (\ f -> do
                                        txt <- readFileV f
                                        txt2 <- if (tokenize argsC)
                                                 then return $!! filter (not.null) $ map (unwords.(Sen.tokenizeC).lc) $ lines $ Sen.sentencesT txt
                                                 else return $!! filter (not.null) $ lines $ txt
--                                        duni <- return $!! makeDictFromTextUni $ txt2
                                        dbi <- return $!! makeDictFromTextBi0 $ txt2
                                        bd2 <- gs dbi
                                        segs <- case combinability argsC of
                                                     "majority" -> return $ mergeMajoritySegs $ L.zip4 (getSegs bd2 uni (dice) n txt2) (getSegs bd2 uni (pmi) n txt2) (getSegs bd2 uni (tscore) n txt2) (getSegs bd2 uni (gravity) n txt2)
                                                     _          -> return $ getSegs bd2 uni cmb n txt2
                                        writeFileV (f++".seg") segs
--                                        txt <- readFileV $ f ++ ".seg"
                                        di <- return $!! filterSegments $ makeDictFromTextUni $ lines segs
                                        writeFileV (f ++ ".seg.frq") $ dictToStr $ di
                                        putStrLn f
                              ) fs
                        performGC 
               )  $ splitInto 8 d
         if (makeSegList argsC)
         then do 
               performGC
               b <- foldM (\ b1 xs1 -> do
                                        bTs <- MP.mapM (\ i -> do
                                                                      txt <- readFileV $ (i) ++ ".seg"
                                                                      di <- return $!! filterSegments $ makeDictFromTextUni $ lines txt
                                                                      return di
                                                 ) xs1
                                        bT <- return $!! sumDicts bTs
                                        performGC
                                        putStr "*"
                                        hFlush stdout
                                        return $!! sumLists b1 bT
                          ) [] $ splitInto 100 $ L.sort $ (d)
               putStrLn ""
               writeFileV ((ngramfile argsC) ++ ".seg") $ dictToStr $ b
         else return ()

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
----------------------------Building and using ngrams------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
--getStatsRam :: M.Map String (M.Map String Int) -> Cseg -> (String, M.Map String Int) -> IO ((String, [(String, (Float, Float))]))
getStatsRam :: M.Map String (M.Map String Int) -> Cseg -> [(String, M.Map [Char] Int)] -> IO [(String, [([Char], (Float, Float))])]
getStatsRam bi argsC dbi
        = mapM (\(w1s,ws1s) -> case M.lookup w1s bi of
                                    Just bi2 -> do
                                                  n <- return $ toEnum $ M.size bi2
                                                  rm <- return $ map (\(w2,f2) -> case M.lookup w2 bi2 of
                                                                                                              Just f  -> (w2, ((appnd f f2),n))
                                                                                                              Nothing -> (w2, ((appnd 0 f2),n))
                                                                     ) $ M.toList ws1s
                                                  return $ (w1s,rm)
                                    Nothing  -> do
                                                  n <- return $ toEnum $ M.size ws1s
                                                  rm <- return $ map (\(w2,f2) -> (w2,(toEnum f2,n))) $ M.toList ws1s
                                                  return (w1s,rm)
               ) $ dbi
                 where
                  append = statsAppend argsC
                  appnd :: Int -> Int -> Float
                  appnd f1 f2 = toEnum $ if append then f1 + f2 else f1

loadBigrams :: FilePath -> IO (M.Map String (M.Map [Char] Int))
loadBigrams bi = do
                              txt <- readFileV bi
                              return $!! M.fromList $ map (toLs.lines . map (tabToNl)) $ lines txt
                    where
                       toLs (w1:ws) = (w1,((M.fromList $ map ((\[w2,f] -> (w2, ((read f)::Int))).words) ws)))
                       toLs [] = error "Empty line"
                    
getStats2 ::       FilePath
                   -> Cseg
                   -> [(String, M.Map [Char] Int)]
                   -> IO [(String, [([Char], (Float, Float))])]
getStats2 hBi argsC dbi = do
                              txt <- readFileV hBi
                              its <- return $ map (lines . map (tabToNl)) $ lines txt
                              return $!! updateStats its dbi (statsAppend argsC)

updateStats :: forall t t1.
                     (Enum t1, Enum t) =>
                     [[String]]
                     -> [(String, M.Map [Char] Int)]
                     -> Bool
                     -> [(String, [([Char], (t, t1))])]
--updateStats [] [] _ = []
updateStats _ [] _ = []
updateStats ([]:ww1) ww2 append = updateStats ww1 ww2 append
updateStats [] ((w1,ws2):ww2) append = (w1,(map (\(w2,f2) -> (w2,((appnd 0 f2), n))) wss2)) : (updateStats [] (ww2) append)
              where
                  wss2 = M.toList ws2
                  n = toEnum $ length wss2
                  appnd f1 f2 = toEnum $ if append then f1 + f2 else f1

updateStats ((w1T:ws1):ww1) ((w1,ws2):ww2) append
              | w1T < w1  = updateStats (ww1) ((w1,ws2):ww2) append
              | w1T == w1 = (w1,(mergeListT n wss1 wss2))    :updateStats ww1 ww2 append
              | otherwise = (w1,(map (\(w,f) -> (w,((toEnum f),ws2len))) $ wss2 )):updateStats ((w1T:ws1):ww1) (ww2) append
              where
                  ws2len = toEnum $ length wss2
                  wss2 = M.toList ws2
                  wss1 = map ((\[w,f] -> (w, ((read f)::Int))).words) $ ws1
                  n = toEnum $ length wss1
                  mergeListT nT ((w1M,f1):wsT1) ((w2,f2):wsT2) | w1M == w2 = (w1M,((appnd f1 f2), nT)):mergeListT nT wsT1 wsT2
                                                               | w1M < w2  = mergeListT nT wsT1 ((w2,f2):wsT2)
                                                               | otherwise = (w2,((appnd 0 f2), nT)):mergeListT nT ((w1M,f1):wsT1) wsT2
                  mergeListT nT [] ((w2,f2):wsT2) = (w2,((appnd 0 f2), nT)):mergeListT nT [] wsT2
                  mergeListT _ _ _ = []
                  appnd f1 f2 = toEnum $ if append then f1 + f2 else f1

getSegs :: forall t t1 t2 t3.
                 (Enum t2, Enum t1) =>
                 [([Char], [([Char], t3)])]
                 -> M.Map [Char] Int
                 -> (t -> t1 -> t2 -> t3 -> Float)
                 -> t
                 -> [String]
                 -> String
getSegs bd2 uni cmb n txt2 = unlines 
                             $ makeTextSegmentation (txt2) 
                             $ M.fromList 
                             $ map (\(w1,ws)-> (w1, (M.fromList 
                                                     $ map (\(w2,f1) -> (w2,(cmb n 
                                                                                 (toEnum $ M.findWithDefault (1) w1 uni) 
                                                                                 (toEnum $ M.findWithDefault (1) w2 uni) 
                                                                                 f1
                                                                            ))
                                                           ) ws
                                                    ))
                                   ) bd2

sumFileTrees :: [String] -> [String] -> Handle -> IO ()
sumFileTrees [] ls2 hout  = hPutStr hout $ unlines ls2
sumFileTrees ls1 [] hout  =  hPutStr hout $ unlines ls1
sumFileTrees (ln1:lns1) (ln2:lns2) hout
          | w1 > w2   = do 
                           hPutStrLn hout ln2
                           sumFileTrees (ln1:lns1) (lns2) hout
          | w1 == w2  = do 
                           hPutStrLn hout $ w1 ++ "\t" ++ (init $ concat $ map (\(w2T,f)-> w2T ++ " " ++ (show f) ++ "\t") $ M.toList wss)
                           sumFileTrees lns1 lns2 hout
          | otherwise = do 
                           hPutStrLn hout ln1
                           sumFileTrees (lns1) (ln2:lns2) hout
            where
               (w1:ws1) = lines $ map (tabToNl) ln1
               (w2:ws2) = lines $ map (tabToNl) ln2
               wss1 = map ((\[w,f] -> (w, ((read f)::Int))).words) ws1
               wss2 = map ((\[w,f] -> (w, ((read f)::Int))).words) ws2
               wss = if length wss1 < length wss2
                    then (M.unionWith (+) (M.fromList wss1) (M.fromList wss2))
                    else (M.unionWith (+) (M.fromList wss2) (M.fromList wss1))

sumFileUnigrams :: [String] -> [String] -> Handle -> IO ()
sumFileUnigrams [] ls2 hout  = hPutStr hout $ unlines ls2
sumFileUnigrams ls1 [] hout  =  hPutStr hout $ unlines ls1
sumFileUnigrams (ln1:lns1) (ln2:lns2) hout
          | w1 > w2   = do 
                           hPutStrLn hout ln2
                           sumFileUnigrams (ln1:lns1) (lns2) hout
          | w1 == w2  = do 
                           hPutStrLn hout $ w1 ++ "\t" ++ (show ff)
                           sumFileUnigrams lns1 lns2 hout
          | otherwise = do 
                           hPutStrLn hout ln1
                           sumFileUnigrams (lns1) (ln2:lns2) hout
            where
               [w1,f1] = words ln1
               [w2,f2] = words ln2
               ff:: Int
               ff = (read f1) + (read f2)

filterSegments :: [Dict] -> [Dict]
filterSegments segs = filter (isValidSeg.toks) segs
                   where
                    toks (Dict s _) =  words $ map (\c -> if c == '_' then ' ' else c) s
                    isValidSeg s = (isLatinString s && (not $ singleChars s)) || (isChineseString s)

singleChars :: [[Char]] -> Bool
singleChars s = and $ map (\c -> (length c) == 1) s

isLatinString :: [[Char]] -> Bool
isLatinString s = and $ map (\c -> (isAlpha c  && ord c < 10001) || c == '-') $ concat  s

isChineseString :: [[Char]] -> Bool
isChineseString s = or $ map (\c -> ord c > 10000) $ concat s

tabToNl :: Char -> Char
tabToNl '\t' = '\n'
tabToNl x = x

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
------------------------------------Segmentation-----------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
makeTextSegmentation :: [String] -> M.Map [Char] (M.Map [Char] Float) -> [String]
makeTextSegmentation linesS bi = map (segLn) linesS
                                           where
                                             segLn line  = (unwords) . init . tail $ makeLineSegmentation ( ["%."] ++ (words line) ++ [".%"] ) bi

------------------------------------------------------
makeLineSegmentation :: [[Char]] -> M.Map [Char] (M.Map [Char] Float) -> [[Char]]
makeLineSegmentation (w:ws) bi  = setBoundaries $! ws'
                                             where
                                               ws' = (w,dc): countCollocability w ws bi
                                               dc = (findDice w (ws!!0) bi)
makeLineSegmentation [] _  = []

------------------------------------------------------
countCollocability :: Ord t => t -> [t] -> M.Map t (M.Map t Float) -> [(t, Float)]
countCollocability _ []         _ = []
countCollocability _ [w2]       _ = [(w2,0)]
countCollocability w1 (w2:w3:w4:ws) bi
                        | dc13 > dc12 && dc13 > dc23                     
                          && dc24 > dc23 && dc24 > dc34  = ((w2,avg [dc13, dc23,dc24]):(countCollocability w2 (w3:w4:ws) bi))                   -- w1 w2 w3 w4
                        | dc24 > dc23 && dc24 > dc34     = ((w2,avg [dc23,dc24]):(countCollocability w2 (w3:w4:ws) bi))                   -- w1 w2 w3 w4
                        | dc13 > dc12 && dc13 > dc23     = ((w2,avg [dc13, dc23]):(countCollocability w2 (w3:w4:ws) bi))                   -- w1 w2 w3 w4
                        | otherwise                      = ((w2,dc23):(countCollocability w2 (w3:w4:ws) bi))
                                                  where
                                                         dc12 = (findDice w1 w2 bi)
                                                         dc13 = (findDice w1 w3 bi)
                                                         dc23 = (findDice w2 w3 bi)
                                                         dc24 = (findDice w2 w4 bi)
                                                         dc34 = (findDice w3 w4 bi)
countCollocability w1 (w2:w3:ws) bi
                        | dc13 > dc12 && dc13 > dc23 = ((w2,(avg [dc12,dc13,dc23])):(countCollocability w2 (w3:ws) bi))
                        | otherwise              = ((w2,dc23):(countCollocability w2 (w3:ws) bi))
                                                  where
                                                         dc12 = (findDice w1 w2 bi)
                                                         dc13 = (findDice w1 w3 bi)
                                                         dc23 = (findDice w2 w3 bi)

-- |Numerically stable mean
avg :: Fractional a => [a] -> a
avg x = fst $ L.foldl' addElement (0,0) x
    where
      addElement (!m,!n) xT = (m + (xT-m)/(n+1), n+1)



findDice::(Ord k, Ord t, Num a) => t -> k -> M.Map t (M.Map k a) -> a
findDice w1 w2 bi = f2
                  where
                     f1 = M.findWithDefault (M.empty) w1 bi
                     f2 = M.findWithDefault (0) w2 f1

under :: [Char]
under = "_"
------------------------------------------------------
setBoundaries :: (Ord t, Fractional t) => [([Char], t)] -> [[Char]]
setBoundaries  [(w1,_),(w2,_)] = [w1,w2]
setBoundaries  [(w1,_),(w2,_),(w3,_)] = [w1,w2,w3]
setBoundaries  ((w1,c1):(w2,c2):(w3,c3):ws)
                       | (length w2 == 1 && (isPunct $ head w2)) || (length w3 == 1 && (isPunct $ head w3)) = w1:setBoundaries  ((w2,c2):(w3,c3):ws)
                       | (c1 + c3)/2 < c2 = setBoundaries  ((w1,c2):((concat [w2, under, w3]),c3):ws)
                       | otherwise        = w1:setBoundaries  ((w2,c2):(w3,c3):ws)
                       where
                        isPunct c = isPunctuation c && c/= '\''
setBoundaries  _ = []

------------------------------------------------------
-- combinability functions
dice:: Float -> Float -> Float -> (Float,Float) -> Float
dice _ f11 f12 (f2,_) = 2*f2/(f11+f12)

pmi:: Float -> Float -> Float -> (Float,Float) -> Float
pmi n f11 f12 (f2,_) = log (n*f2/(f11*f12))

tscore:: Float -> Float -> Float -> (Float,Float) -> Float
tscore n f11 f12 (f2,_) = (f2 - (f11*f12/n))/(sqrt(f2))

gravity:: Float -> Float -> Float -> (Float,Float) -> Float
gravity _ f11 f12 (f2,n2) = log(f11 * f2/n2) + log(f12*f2/n2)

mergeMajoritySegs :: [(Char, Char, Char, Char)] -> [Char]
mergeMajoritySegs [] = []
mergeMajoritySegs ((c1,c2,c3,c4):xs) 
                 | c1 /= ' ' && c1 /= '_' = c1:mergeMajoritySegs xs
                 | s >=2     = '_':mergeMajoritySegs xs
                 | otherwise = ' ':mergeMajoritySegs xs
                 where
                     upp:: Char -> Integer
                     upp x = if x == '_' then 1 else 0
                     s = upp c1 + upp c2 + upp c3 + upp c4

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
----------------------------------------Dicts--------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
dictToStr :: [Dict] -> String
dictToStr ds = unlines $ map (\(Dict w f) -> w ++ "\t" ++ (show f) ) ds
-----------------------------------------------------
makeDictFromTextUni :: [String] -> [Dict]
makeDictFromTextUni linesS = Par.parMap Par.rpar (\ ws -> (Dict (head ws) (length ws))) $ L.group $ L.sort $ concat $ Par.parMap Par.rpar  (\line -> [("%.")] ++ (words line) ++ [(".%")]) linesS
------------------------------------------------------
makeDictFromTextBi :: [String] -> [([Char], M.Map [Char] Int)]
makeDictFromTextBi linesS =  biToTree $ Par.parMap Par.rpar (\ ws -> ((head ws),(length ws))) $ L.group $ L.sort $ concat $ Par.parMap Par.rpar (toListBi) linesS
               where
                 toListBi ls = bigrams $ wordsLn ls
                 wordsLn ls = ["%."] ++ (words ls) ++ [".%"]
                 bigrams (w1:w2:w3:ws) = (Par.withStrategy Par.rpar $ (w1,w2)):(Par.withStrategy Par.rpar $ (w1,w3)):(Par.withStrategy Par.rpar $ (w2,w1)):(Par.withStrategy Par.rpar $ (w3,w1)):bigrams (w2:w3:ws)
                 bigrams (w1:w2:ws) = (Par.withStrategy Par.rpar $ (w1,w2)):(Par.withStrategy Par.rpar $ (w2,w1)):bigrams (w2:ws)
                 bigrams _ = []
------------------------------------------------------
makeDictFromTextBi0 :: [String] -> [([Char], M.Map [Char] Int)]
makeDictFromTextBi0 linesS =  biToTree $ Par.parMap Par.rpar (\ ws -> ((head ws),(length ws))) $ L.group $ L.sort $ concat $ Par.parMap Par.rpar (toListBi) linesS
               where
                 toListBi ls = bigrams $ wordsLn ls
                 wordsLn ls = ["%."] ++ (words ls) ++ [".%"]
                 bigrams (w1:w2:ws) = (Par.withStrategy Par.rpar $ (w1,w2)):bigrams (w2:ws)
                 bigrams _ = []
-----------------------------------------------------
biToTree :: forall a t t1.
                  (Ord t, Eq a) =>
                  [((a, t), t1)] -> [(a, M.Map t t1)]
biToTree [] = []
biToTree ds = (Par.withStrategy Par.rpar i): (biToTree lnsR)
                     where 
                         it ((w1,w2),f) = (w1,(w2,f))
                         w = fst $ it $ head ds
                         (lnsH,lnsR) = span (\ln ->  (fst $ it ln) == w) ds
                         lnsHI = M.fromList $ map (snd.it) lnsH
                         i = (w,lnsHI)
------------------------------------------------------
sumDicts :: [[Dict]] -> [Dict]
sumDicts [] = []
sumDicts [ds] = ds
sumDicts ds = sumDicts $!! sum2MDicts $!! ds
------------------------------------------------------
sum2MDicts :: [[Dict]] -> [[Dict]]
sum2MDicts (d1:d2:ds) = (Par.withStrategy Par.rpar $!! (sumLists d1 d2)):(sum2MDicts ds)
sum2MDicts ds = ds
------------------------------------------------------
sumTreesB :: forall a k a1.
                   (NFData a1, NFData k, NFData a, Ord a1, Ord k, Num a) =>
                   [[(a1, M.Map k a)]] -> [(a1, M.Map k a)]
sumTreesB [] = []
sumTreesB [ds] = ds
sumTreesB ds = sumTreesB $!! sum2Trees $!! ds
------------------------------------------------------
sum2Trees :: forall a k a1.
                   (NFData a1, NFData k, NFData a, Ord a1, Ord k, Num a) =>
                   [[(a1, M.Map k a)]] -> [[(a1, M.Map k a)]]
sum2Trees (d1:d2:ds) = (Par.withStrategy Par.rpar $!! (sumTrees d1 d2)):(sum2Trees ds)
sum2Trees ds = ds
------------------------------------------------------
sumLists :: [Dict] -> [Dict] -> [Dict]
sumLists [] ls2  = ls2
sumLists ls1 []  = ls1
sumLists xa@(((Dict w1 f1)):ls1) ya@(((Dict w2 f2)):ls2)
          | w1 > w2   = (ls1H) ++ (sumLists xa ls1R)
          | w1 == w2  = (Par.withStrategy Par.rpar nd)        : (sumLists ls1 ls2)
          | otherwise = (ls2H) ++ (sumLists ls2R ya)
            where
               nd = (Dict w1 (f1+f2))
               (ls1H,ls1R) = span (\(Dict ww _) -> w1>ww) ya
               (ls2H,ls2R) = span (\(Dict ww _) -> ww<w2) xa
------------------------------------------------------
sumTrees :: forall a k a1.
                  (Ord a1, Ord k, Num a) =>
                  [(a1, M.Map k a)] -> [(a1, M.Map k a)] -> [(a1, M.Map k a)]
sumTrees [] ls2  = ls2
sumTrees ls1 []  = ls1
sumTrees xa@((x@(w1,ws1)):ls1) ya@((y@(w2,ws2)):ls2)
          | w1 > w2   = y : (sumTrees xa ls2)
          | w1 == w2  = (Par.withStrategy Par.rpar nd): (sumTrees ls1 ls2)
          | otherwise = x: (sumTrees ls1 ya)
            where
               nd = if length ws1 < length ws2
                    then (w1,(M.unionWith (+) ws1 ws2)) 
                    else (w1,(M.unionWith (+) ws2 ws1)) 

------------------------------------------------------
-- Helper to split list of files into n sublists for parralel processing 
splitInto :: forall a. Int -> [a] -> [[a]]
splitInto n xs
      | length xs > n = (take n xs) : (splitInto n $ drop n xs)
      | otherwise = [xs]

------------------------------------------------------
-- lowercase string
lc :: [Char] -> [Char]
lc s = map (toLower) s

-- for stability on Win and *nix systems
------------------------------------------------------
writeFileV :: FilePath -> String -> IO ()
writeFileV f t = do 
                   h <- openFile f WriteMode
                   hSetNewlineMode h noNewlineTranslation
                   hPutStr h t
                   hFlush h
                   hClose h

------------------------------------------------------
readFileV :: FilePath -> IO String
readFileV f = do 
                   h <- openFile f ReadMode
                   hSetNewlineMode h noNewlineTranslation
                   hGetContents h

------------------------------------------------------
getFiles :: [FilePath] -> [Char] -> IO [FilePath]
getFiles dirs ext = getFiles'' dirs ext []
------------------------------------------------------
getFiles'' :: [FilePath] -> [Char] -> [FilePath] -> IO [FilePath]
getFiles'' [] _ ls = return ls
getFiles'' (dir:dirs) ext ls = do
            fdo <- doesDirectoryExist(dir)
            if(fdo) then do
                        fs <- getDirectoryContents(dir)
                        r <- return [dir++"/"++f | f<-fs , f /= "." , f /= ".." ]
                        getFiles'' (dirs++r) ext ls
                    else do
                        d <- doesFileExist(dir)
                        if(d && hasExtention dir ext )
                               then getFiles'' dirs ext $! (dir:ls)
                               else getFiles'' dirs ext ls
------------------------------------------------------
hasExtention:: String -> String -> Bool
hasExtention _ [] = True
hasExtention file ext
          | (drop (length file - length ext) file) == ext = True
          | otherwise = False
