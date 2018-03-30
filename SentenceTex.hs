module SentenceTex (tokenizeC,sentencesT) where

import qualified Data.List as DL
import Data.Char

tokenizeC [] = []
tokenizeC all@(x:xs)
         | isSpace x                     = tokenizeC xs
         | isSymbol x                    = [x]:tokenizeC xs
         | isAlpha x && ord x > 10000    = [x]:tokenizeC xs
         | isAlpha x                     = (wordT):tokenizeC remT
         | isDigit x                     = numb2:tokenizeC rem2
         | isPrint x                     = [x]:tokenizeC xs
         | otherwise                     = tokenizeC xs
         where 
           (word,rem) = span (\c -> isAlpha c && ord c < 10001) all
           (numb,rem2) = span (isDigit) all
           numb2 = "N"
           (wordT,remT) = if length rem < 2
                          then (word,rem)
                          else if head rem == '-' || head rem == '—'
                               then case span (\c -> (isAlpha c && ord c < 10001) || c == '-' || c == '—') rem of
                                         ([],_) -> (word,rem)
                                         (wordX,remX) -> ((word++wordX),remX)
                               else (word,rem)
                          
sentencesT [] = []
sentencesT ('\t':'\t':xs) = sentencesT ('\t':xs)
sentencesT ('\t':' ':xs) = sentencesT ('\t':xs)
sentencesT (' ':'\t':xs) = sentencesT ('\t':xs)
sentencesT ('\t':xs) = '\n' : sentencesT xs
sentencesT (')':'.':' ':a@(x:xs))
      | isDigit x                = ')' : '.' : ' ' : '\n':sentencesT a
      | isUpper x                = ')' : '.' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = ')' : '.' : ' ' : '\n' : sentencesT a
      | otherwise                = ')' : sentencesT ('.' : ' ' :a)
sentencesT (')':':':' ':a@(x:xs))
      | isPrefixOfT "" beginSen a  = ')' : ':' : ' ' : '\n' : sentencesT a
      | otherwise                = ')' : sentencesT (':' : ' ' :a)
sentencesT (']' : '.' : ' ' : a@(x : xs))
      | isDigit x                = ']' : '.' : ' ' : '\n' : sentencesT a
      | isUpper x                = ']' : '.' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = ']' : '.' : ' ' : '\n' : sentencesT a
      | otherwise                = ']': sentencesT ('.' : ' ' : a)
sentencesT (']' : '?' : ' ' : a@(x : xs))
      | isDigit x                = ']' : '?' : ' ' : '\n' : sentencesT a
      | isUpper x                = ']' : '?' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = ']' : '?' : ' ' : '\n' : sentencesT a
      | otherwise                = ']': sentencesT ('?' : ' ' : a)
sentencesT (']' : '!' : ' ' : a@(x : xs))
      | isDigit x                = ']' : '!' : ' ' : '\n' : sentencesT a
      | isUpper x                = ']' : '!' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = ']' : '!' : ' ' : '\n' : sentencesT a
      | otherwise                = ']': sentencesT ('!' : ' ' : a)
sentencesT (']' : ' ' : a@(x : xs))
      | isBeginEntiny a  = ']' : ' ' : '\n' : sentencesT a
      | isPrefixOfT "" beginSen a = ']' : ' ' : '\n' : sentencesT a
      | otherwise                = ']': sentencesT (' ' : a)
sentencesT ('\"' : '.' : ' ' : a@(x : xs))
      | isDigit x                = '\"' : '.' : ' ' : '\n' : sentencesT a
      | isUpper x                = '\"' : '.' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = '\"' : '.' : ' ' : '\n' : sentencesT a
      | otherwise                = '\"': sentencesT ('.' : ' ' : a)
sentencesT ('\"' : '?' : ' ' : a@(x : xs))
      | isDigit x                = '\"' : '?' : ' ' : '\n' : sentencesT a
      | isUpper x                = '\"' : '?' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = '\"' : '?' : ' ' : '\n' : sentencesT a
      | otherwise                = '\"': sentencesT ('?' : ' ' : a)
sentencesT ('\"' : '!' : ' ' : a@(x : xs))
      | isDigit x                = '\"' : '!' : ' ' : '\n' : sentencesT a
      | isUpper x                = '\"' : '!' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = '\"' : '!' : ' ' : '\n' : sentencesT a
      | otherwise                = '\"': sentencesT ('!' : ' ' : a)
sentencesT ('.' : '\"' : ' ' : a@(x : xs))
      | isDigit x                = '.' : '\"' : ' ' : '\n' : sentencesT a
      | isUpper x                = '.' : '\"' : ' ' : '\n' : sentencesT a
      | x == '!'                = '.' : '\"' : ' ' : '\n' : '!' : sentencesT xs
      | x == '\"'                = '.' : '\"' : ' ' : '\n' : '\"' : sentencesT xs
      | isLower x                = '.' : '\"' : ' ' : sentencesT a
      | isBeginEntiny a      = '.' : '\"' : ' ' : '\n' : sentencesT a
      | otherwise                = '.' : sentencesT ('\"' : ' ' : a)
sentencesT ('.' : '\'' : '\'' : ' ' : a@(x : xs))
      | isDigit x                = '.' : '\'' : '\'' : ' ' : '\n' : sentencesT a
      | isUpper x                = '.' : '\'' : '\'' : ' ' : '\n' : sentencesT a
      | isLower x                = '.' : '\'' : '\'' : ' ' : sentencesT a
      | isBeginEntiny a          = '.' : '\'' : '\'' : ' ' : '\n' : sentencesT a
      | otherwise                = '.' : sentencesT ('\'' : '\'' : ' ' : a)
sentencesT ('?' : '\"' : ' ' : a@(x : xs))
      | isDigit x                = '?' : '\"' : ' ' : '\n' : sentencesT a
      | isUpper x                = '?' : '\"' : ' ' : '\n' : sentencesT a
      | x == '!'                = '?' : '\"' : ' ' : '\n' : '!' : sentencesT xs
      | x == '\"'                = '?' : '\"' : ' ' : '\n' : '\"' : sentencesT xs
      | isLower x                = '?' : '\"' : ' ' : sentencesT a
      | isBeginEntiny a      = '?' : '\"' : ' ' : '\n' : sentencesT a
      | otherwise                = '?' : sentencesT ('\"' : ' ' : a)
sentencesT ('!' : '\"' : ' ' : a@(x : xs))
      | isDigit x                = '!' : '\"' : ' ' : '\n' : sentencesT a
      | isUpper x                = '!' : '\"' : ' ' : '\n' : sentencesT a
      | x == '!'                = '!' : '\"' : ' ' : '\n' : '!' : sentencesT xs
      | x == '\"'                = '!' : '\"' : ' ' : '\n' : '\"' : sentencesT xs
      | isLower x                = '!' : '\"' : ' ' : sentencesT a
      | isBeginEntiny a      = '!' : '\"' : ' ' : '\n' : sentencesT a
      | otherwise                = '!' : sentencesT ('\"' : ' ' : a)
sentencesT ('.' : '\'' : ' ' : a@(x : xs))
      | isDigit x                = '.' : '\'' : ' ' : '\n' : sentencesT a
      | isUpper x                = '.' : '\'' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a      = '.' : '\'' : ' ' : '\n' : sentencesT a
      | otherwise                = '.' : sentencesT ('\'' : ' ' : a)     -- '
sentencesT ('?' : '\'' : ' ' : a@(x : xs))
      | isDigit x                = '?' : '\'' : ' ' : '\n' : sentencesT a
      | isUpper x                = '?' : '\'' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a      = '?' : '\'' : ' ' : '\n' : sentencesT a
      | otherwise                = '?' : sentencesT ('\'' : ' ' : a)     -- '
sentencesT ('!' : '\'' : ' ' : a@(x : xs))
      | isDigit x                = '!' : '\'' : ' ' : '\n' : sentencesT a
      | isUpper x                = '!' : '\'' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a      = '!' : '\'' : ' ' : '\n' : sentencesT a
      | otherwise                = '!' : sentencesT ('\'' : ' ' : a)     -- '
sentencesT ('.' : ')' : ' ' : a@(x : xs))
      | isDigit x                = '.' : ')' : ' ' : '\n' : sentencesT a
      | isUpper x                = '.' : ')' : ' ' : '\n' : sentencesT a
      | isLower x                = '.' : ')' : ' ' : sentencesT a
      | isBeginEntiny a  = '.' : ')' : ' ' : '\n' : sentencesT a
      | otherwise                = '.' : sentencesT (')' : ' ' : a)
sentencesT ('?' : ')' : ' ' : a@(x : xs))
      | isDigit x                = '?' : ')' : ' ' : '\n' : sentencesT a
      | isUpper x                = '?' : ')' : ' ' : '\n' : sentencesT a
      | isLower x                = '?' : ')' : ' ' : sentencesT a
      | isBeginEntiny a  = '?' : ')' : ' ' : '\n' : sentencesT a
      | otherwise                = '?' : sentencesT (')' : ' ' : a)
sentencesT ('!' : ')' : ' ' : a@(x : xs))
      | isDigit x                = '!' : ')' : ' ' : '\n' : sentencesT a
      | isUpper x                = '!' : ')' : ' ' : '\n' : sentencesT a
      | isLower x                = '!' : ')' : ' ' : sentencesT a
      | isBeginEntiny a  = '!' : ')' : ' ' : '\n' : sentencesT a
      | otherwise                = '!' : sentencesT (')' : ' ' : a)
sentencesT ('.' : ']' : ' ' : a@(x : xs))
      | isDigit x                = '.' : ']' : ' ' : '\n' : sentencesT a
      | isUpper x                = '.' : ']' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = '.' : ']' : ' ' : '\n' : sentencesT a
      | otherwise                = '.' : sentencesT (']' : ' ' : a)
sentencesT ('?' : ']' : ' ' : a@(x : xs))
      | isDigit x                = '?' : ']' : ' ' : '\n' : sentencesT a
      | isUpper x                = '?' : ']' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = '?' : ']' : ' ' : '\n' : sentencesT a
      | otherwise                = '?' : sentencesT (']' : ' ' : a)
sentencesT ('!' : ']' : ' ' : a@(x : xs))
      | isDigit x                = '!' : ']' : ' ' : '\n' : sentencesT a
      | isUpper x                = '!' : ']' : ' ' : '\n' : sentencesT a
      | isBeginEntiny a  = '!' : ']' : ' ' : '\n' : sentencesT a
      | otherwise                = '!' : sentencesT (']' : ' ' : a)
sentencesT ('.' : ' ' : '[' : a@(x : xs))
      | isDigit x                = '.' : ' ' : '\n' : '[' : sentencesT a
      | isUpper x                = '.' : ' ' : '\n' : '[' : sentencesT a
      | isBeginEntiny a  = '.' : ' ' : '\n' : '[' :  sentencesT a
      | otherwise                = '.' : sentencesT (' ' : '[' : a)
sentencesT ('?' : ' ' : '[' : a@(x : xs))
      | isDigit x                = '?' : ' ' : '\n' : '[' : sentencesT a
      | isUpper x                = '?' : ' ' : '\n' : '[' : sentencesT a
      | isBeginEntiny a  = '?' : ' ' : '\n' : '[' :  sentencesT a
      | otherwise                = '?' : sentencesT (' ' : '[' : a)
sentencesT ('!' : ' ' : '[' : a@(x : xs))
      | isDigit x                = '!' : ' ' : '\n' : '[' : sentencesT a
      | isUpper x                = '!' : ' ' : '\n' : '[' : sentencesT a
      | isBeginEntiny a  = '!' : ' ' : '\n' : '[' :  sentencesT a
      | otherwise                = '!' : sentencesT (' ' : '[' : a)
sentencesT ('.' : ' ' : '(' : b : ')' : ' ' : a@(x:xs))    -- . (a) Note
      | isDigit x                = '.' : ' ' : '\n' : '(' : b : ')' : ' ' : sentencesT a 
      | isUpper x                = '.' : ' ' : '\n' : '(' : b : ')' : ' ' : sentencesT a
      | isBeginEntiny a          = '.' : ' ' : '\n' : '(' : b : ')' : ' ' : sentencesT a
      | (isNumT a) > 0           = '.' : ' ' : '\n' : '(' : b : ')' : ' ' : sentencesT a
      | otherwise                = '.' : ' ' : '(' : b : ')' : ' ' : sentencesT a
sentencesT ('.' : ' ' : '(' : b : ')' : '-' : a@(x:xs))    -- . (a)-(b) Note
                                 = '.' : ' ' : '\n' : '(' : b : ')' : '-' : sentencesT a
sentencesT ('.' : ' ' : '(' : a@(x:xs))    -- . (Note
      | isDigit x                = '.' : ' ' : '\n' : '(' : sentencesT a 
      | isUpper x                = '.' : ' ' : '\n' : '(' : sentencesT a
      | isBeginEntiny a          = '.' : ' ' : '\n' : '(' : sentencesT a
      | (isNumT a) > 0           = '.' : ' ' : '\n' : '(' : sentencesT a
      | otherwise                = '.' : ' ' : '(' : sentencesT a
sentencesT ('?' : ' ' : '(' : a@(x:xs))    -- . (Note
      | isDigit x                = '?' : ' ' : '\n' : '(' : sentencesT a 
      | isUpper x                = '?' : ' ' : '\n' : '(' : sentencesT a
      | isBeginEntiny a  = '?' : ' ' : '\n' : '(' : sentencesT a
      | otherwise                = '?' : ' ' : '(' : sentencesT a
sentencesT ('!' : ' ' : '(' : a@(x:xs))    -- . (Note
      | isDigit x                = '!' : ' ' : '\n' : '(' : sentencesT a 
      | isUpper x                = '!' : ' ' : '\n' : '(' : sentencesT a
      | isBeginEntiny a  = '!' : ' ' : '\n' : '(' : sentencesT a
      | otherwise                = '!' : ' ' : '(' : sentencesT a
sentencesT ('.' : ' ' : a@(x : xs))
      | isDigit x                = '.' : ' ' : '\n' : sentencesT a
      | isUpper x                = '.' : ' ' : '\n' : sentencesT a
      | x == '\"' || x == '\''   =  '.' : ' ' : '\n' : sentencesT a  -- '
      | isBeginEntiny a  = '.' : ' ' : '\n' : sentencesT a
      | length rN > 0 
        && (not $ null enumerate) 
        && headS rN == ')'        = ('.' : ' ' : '\n' : enumerate) ++  (sentencesT rN)
      | length rN2 > 0 
        && x == '(' 
        && (not $ null enumerate2) 
        && headS rN2 == ')'       = ('.' : ' ' : '\n' : '(' : enumerate2) ++  (sentencesT rN2)
      | otherwise                = '.' : ' ' : sentencesT a
      where 
           (enumerate,rN) = span (isAlphaNum) a
           (enumerate2,rN2) = span (isAlphaNum) xs
sentencesT ('?' : ' ' : a@(x : xs))
      | isDigit x                = '?' : ' ' : '\n' : sentencesT a
      | isUpper x                = '?' : ' ' : '\n' : sentencesT a
      | x == '\"' || x == '\''   =  '?' : ' ' : '\n' : sentencesT a  -- '
      | isBeginEntiny a  = '?' : ' ' : '\n' : sentencesT a
      | length rN > 0 
        && (not $ null enumerate) 
        && headS rN == ')'        = ('?' : ' ' : '\n' : enumerate) ++  (sentencesT rN)
      | length rN2 > 0 
        && x == '(' 
        && (not $ null enumerate2) 
        && headS rN2 == ')'       = ('?' : ' ' : '\n' : '(' : enumerate2) ++  (sentencesT rN2)
      | otherwise                = '?' : ' ' : sentencesT a
      where 
           (enumerate,rN) = span (isAlphaNum) a
           (enumerate2,rN2) = span (isAlphaNum) xs

sentencesT ('!' : ' ' : a@(x : xs))
      | isDigit x                = '!' : ' ' : '\n' : sentencesT a
      | isUpper x                = '!' : ' ' : '\n' : sentencesT a
      | x == '\"' || x == '\''   =  '!' : ' ' : '\n' : sentencesT a  -- '
      | isBeginEntiny a  = '!' : ' ' : '\n' : sentencesT a
      | length rN > 0 
        && (not $ null enumerate) 
        && headS rN == ')'        = ('!' : ' ' : '\n' : enumerate) ++  (sentencesT rN)
      | length rN2 > 0 
        && x == '(' 
        && (not $ null enumerate2) 
        && headS rN2 == ')'       = ('!' : ' ' : '\n' : '(' : enumerate2) ++  (sentencesT rN2)
      | otherwise                = '!' : ' ' : sentencesT a
      where 
           (enumerate,rN) = span (isAlphaNum) a
           (enumerate2,rN2) = span (isAlphaNum) xs
sentencesT xs 
      | DL.isPrefixOf "REF. " xs            = (take 4 xs) ++ (" \n"++(sentencesT (drop 5 xs)))
      | numLen > 0 &&  isPrefixOfN "" beginSen rmNum = num ++ (" \n" ++ (sentencesT $ if headSp rmNum then tailS rmNum else rmNum))
      | numLen > 0 &&  (isNumT rmNum) > 0 = num ++ ((sentencesT rmNum))
      | titleLen > 0 && not (isPrefixOfT "" beginSen (drop titleLen xs)) = (take titleLen xs) ++ ((sentencesT (drop titleLen xs)))
      | abbrLen > 0 && (isPrefixOfN "" beginSen rmAbbr) = abbr ++ (" \n" ++ (sentencesT $ if headSp rmAbbr then tailS rmAbbr else rmAbbr))
      | abbrLen > 0 && abbr == "etc." && isUpperT rmAbbr = abbr ++ ("\n" ++ (sentencesT $ rmAbbr))
      | abbrLen > 0                                   = abbr ++ ((sentencesT rmAbbr))
      | DL.isPrefixOf "CITE. " xs           = (take 5 xs) ++ ("\n"++(sentencesT (drop 5 xs)))
      | DL.isPrefixOf "MATH. " xs           = (take 5 xs) ++ ("\n"++(sentencesT (drop 5 xs))) 
      | DL.isPrefixOf "MATHDISP. " xs       = (take 9 xs) ++ (" \n"++(sentencesT (drop 10 xs)))
      | DL.isPrefixOf "REF). " xs           = (take 5 xs) ++ (" \n"++(sentencesT (drop 6 xs)))
      | DL.isPrefixOf "CITE). " xs          = (take 6 xs) ++ (" \n"++(sentencesT (drop 7 xs)))
      | DL.isPrefixOf "MATH). " xs          = (take 6 xs) ++ (" \n"++(sentencesT (drop 7 xs)))
      | DL.isPrefixOf "MATHDISP). " xs      = (take 10 xs) ++ (" \n"++(sentencesT (drop 11 xs)))
      | DL.isPrefixOf "REF]. " xs           = (take 5 xs) ++ (" \n"++(sentencesT (drop 6 xs)))
      | DL.isPrefixOf "CITE]. " xs          = (take 6 xs) ++ (" \n"++(sentencesT (drop 7 xs)))
      | DL.isPrefixOf "MATH]. " xs          = (take 6 xs) ++ (" \n"++(sentencesT (drop 7 xs)))
      | DL.isPrefixOf "MATHDISP]. " xs      = (take 10 xs) ++ (" \n"++(sentencesT (drop 11 xs)))
      | isPrefixOfT "MATHDISP)" beginSen xs = (take 9 xs) ++ if sh (drop 9 xs) then (" \n"++(sentencesT (drop 10 xs))) else ("\n"++(sentencesT (drop 9 xs)))
      | isPrefixOfT "MATHDISP]" beginSen xs = (take 9 xs) ++ if sh (drop 9 xs) then (" \n"++(sentencesT (drop 10 xs))) else ("\n"++(sentencesT (drop 9 xs)))
      | isPrefixOfT "MATHDISP" beginSen xs  = (take 8 xs) ++ if sh (drop 8 xs) then (" \n"++(sentencesT (drop 9 xs))) else ("\n"++(sentencesT (drop 8 xs)))
      | isPrefixOfT "MATH" beginSen xs      = (take 4 xs) ++  if sh (drop 4 xs) then (" \n"++(sentencesT (drop 5 xs))) else ("\n"++(sentencesT (drop 4 xs)))
      | isPrefixOfT "REF" beginSen xs       = (take 3 xs) ++  if sh (drop 3 xs) then (" \n"++(sentencesT (drop 4 xs))) else ("\n"++(sentencesT (drop 3 xs)))
      | isPrefixOfT "CITE" beginSen xs      = (take 4 xs) ++  if sh (drop 4 xs) then (" \n"++(sentencesT (drop 5 xs))) else ("\n"++(sentencesT (drop 4 xs)))
      | isPrefixOfN ":" beginSen xs           = ":\n" ++  (sentencesT ((drop 1 xs)))  
      | isPrefixOfN ";" beginSen xs           = ";\n" ++  (sentencesT ((drop 1 xs)))    
      | (isUpper $ headS xs) 
        && isUpperT rm
        && last isProbablyTitle == '.' 
        && length isProbablyTitle < 6 =
                                         if isNextBegin
                                            || (not (isTitled $ init isProbablyTitle))    -- titled = Abs , A , not titled = ABS
                                            || (not (isName (dropWhile (==' ') rm)))
                                           then if length isProbablyTitle == 2
                                                then if (not isNextBegin) || (isTitleP isProbablyTitle2) 
                                                     then (isProbablyTitle) ++ ((sentencesT rm))
                                                     else (isProbablyTitle) ++ (spRem ++ "\n" ++ (sentencesT $ rm2))
                                                else if (headS rm /= ' ' && ((isTitled isProbablyTitle2) || (isTitleP isProbablyTitle2)))
                                                        then (isProbablyTitle) ++ ((sentencesT rm))
                                                        else  (isProbablyTitle) ++ (spRem ++ "\n" ++ (sentencesT $ rm2))
                                           else (isProbablyTitle) ++ ((sentencesT rm))
      | otherwise = i ++ (sentencesT j)
         where  
              sh (' ':xs) = True
              sh (xs) = False
              isNextBegin = isPrefixOfT "" beginSen (dropWhile (==' ') rm)
              titleLen = isTitle xs titles
              abbrLen = isAbbr xs abbreviations
              (abbr,rmAbbr) = splitAt abbrLen xs
              numLen = isNum xs
              (num,rmNum) = splitAt numLen xs
              (isProbablyTitle,rm) = (\(n,ns) -> case ns of ('.':ms) -> ((n++"."), ms); _ -> (n,ns)) $ span (\x -> (isAlpha x) || (x == '-')) xs
              (isProbablyTitle2,rmT) = (\(n,ns) -> case ns of ('.':ms) -> ((n++"."), ms); _ -> (n,ns)) $ span (\x -> (isAlpha x) || (x == '-')) $ dropWhile (== ' ') rm
              (iT,jT) = span (isAlpha) xs
              (i,j) | null jT = (iT,jT)
                    | null iT = ([head jT] , tailS jT)
                    | otherwise = (iT, jT)
              (spRem, rm2) = if null rm 
                                then ("","") 
                                else if (head rm) == ' ' 
                                        then ((take 1 rm),(drop 1 rm))
                                        else ("",rm)

tailS [] = " "
tailS (x:xs) = xs

headSp (' ':xs) = True
headSp xs = False

headS [] = ' '
headS (x:xs) = x

isTitleP [] = False
isTitleP x
      | (isUpper $ headS x) 
        && last x == '.' 
        && length x < 6 
        && (isTitled $ init x) = True
      | otherwise = False

isTitled (x:xs) = (isUpper x) && (and (map (\n -> isLower n) xs))
isTitled [] = False



isUpperT [] = False
isUpperT (x:xs)
      | isSp x  = isUp $ dropWhile (isSp) xs
      | otherwise = False

isSp x = x == ' ' || x == '\"'

isUp (x:xs) = isUpper x
isUp [] = False


beginSen = ["A"        ,"All"       ,"Almost"    ,"An"        ,"Another"   ,"Any"    ,"Both"     ,"Certain"
           ,"Different","Each"      ,"Either"    ,"Enough"    ,"Every"     ,"Few"    ,"Fewer"    ,"Fewest"  ,"He"
           ,"His"      ,"Her"       ,"How"       ,"If"        ,"It"        ,"Its"    ,"Last"     ,"Least"   ,"Less"
           ,"Let"      ,"Little"    ,"Lots"      ,"Many"      ,"More"      ,"Most"   ,"Much"     ,"My"      ,"Neither"
           ,"Nil"      ,"No"        ,"None"      ,"Not"       ,"Only"      ,"Other"  ,"Our"      ,"Overmuch"
           ,"Plenty"   ,"Quite"     ,"Same"      ,"Several"   ,"Severall"  ,"She"    ,"So"       ,"Some"    ,"Sufficient"
           ,"That"     ,"The"       ,"Their"     ,"Them"      ,"Then"      ,"There"  ,"These"    ,"They"    ,"This"   ,"Those"
           ,"Tons"     ,"Too"       ,"Us"        ,"Various"   ,"Very"      ,"We"     ,"What"     ,"Whatever","When"
           ,"Which"    ,"Whichever" ,"Whose"     ,"You"       ,"Your"
           ,"Abaft"    ,"Abeam"     ,"Aboard"    ,"About"     ,"Above"     ,"Absent" ,"According"
           ,"Across"   ,"Afore"     ,"After"     ,"Against"   ,"Ahead"     ,"Along"  ,"Alongside","Amid"    ,"Amidst" ,"Among"
           ,"Amongst"  ,"Anenst"    ,"Apart"     ,"Apropos"   ,"Apud"      ,"Around" ,"As"       ,"Aside"   ,"Astride","At"    ,"Athwart","Atop"
           ,"Barring"  ,"Because"   ,"Before"    ,"Behind"    ,"Below"     ,"Beneath","Beside"   ,"Besides" ,"Between","Beyond","But"    ,"By"
           ,"Chez"     ,"Circa"     ,"Close"     ,"Concerning","Despite"   ,"Down"   ,"Due"      ,"During"
           ,"Except"   ,"Excluding" ,"Each"
           ,"Failing"  ,"Far"       ,"Finally"   ,"Following" ,"For"       ,"Forenenst" ,"From"  ,"Furthermore"
           ,"Given"
           ,"However"
           ,"In"       ,"Including" ,"Inside"    ,"Instead"   ,"Into"      ,"It","Its"
           ,"Like"     ,"Let"       ,"Lets"
           ,"Minus"    ,"Modulo"
           ,"Near"     ,"Next"      ,"Notwithstanding", "Now"
           ,"Of"       ,"Off"       ,"On"        ,"Onto"      ,"Opposite"  ,"Otherwise" ,"Out"    ,"Outside"  ,"Over"
           ,"Pace"     ,"Past"      ,"Per"       ,"Plus"      ,"Prior"     ,"Pro"    ,"Pursuant", "Prove"
           ,"Qua"
           ,"Rather"   ,"Regarding" ,"Regardless","Round"
           ,"Sans"     ,"Save"      ,"Since"     ,"Subsequent","Such", "Suppose"
           ,"Than"     ,"Thanks"    ,"Through"   ,"Throughout","Till"      ,"Times"  ,"To"       ,"Toward"  ,"Towards"
           ,"Under"    ,"Underneath","Unlike"    ,"Until"     ,"Unto"      ,"Up"     ,"Upon"
           ,"Versus"   ,"Via"       ,"Vice"      ,"Vis"
           ,"With"     ,"Within"    ,"Without"   ,"Worth", "Why"
           ,"mRNAs","mRNA","siRNAs","siRNA","RNA","mRNAs"
           ]

names = 
     ["Abigail"    ,"Abraham"    ,"Ada"    ,"Addison"    ,"Addys"    ,"Adelaide"    ,"Adele"    ,"Adie"    ,"Adrian"    ,"Agatha"    ,"Agnes"    ,"Aidan"    ,"Alaina"    ,"Alan"    ,"Alanna"    ,"Albert"    ,"Alberta"    ,"Alec"    ,"Alex"    ,"Alfred"    ,"Alice"    ,"Alison"    ,"Alma"    ,"Alvin"    ,"Alvina"    ,"Amanda"    ,"Amber"    ,"Amelia"    ,"Amory"    ,"Amy"    ,"Andrea"    ,"Andrew"    ,"Andy"    ,"Angel"    ,"Angela"    ,"Angie"    ,"Angus"    ,"Anna"    ,"Annabel"    ,"Archibald"    ,"Archie"    ,"Arda"    ,"Arlo"    ,"Arthur"    ,"Ashley"    ,"Ashton"    ,"Aubrey"    ,"Audrey"    ,"Augusta"    ,"Auley"    ,"Auliffe"    ,"Austen"    ,"Autumn"    ,"Ava"    ,"Avery"    ,"Avis"    ,"Awley"
    ,"Barbara"    ,"Barnabe"    ,"Barnes"    ,"Barry"    ,"Bartholomew"    ,"Basil"    ,"Beatrice"    ,"Belinda"    ,"Bella"    ,"Benedict"    ,"Berenice"    ,"Bernadine"    ,"Bert"    ,"Bertha"    ,"Bertram"    ,"Bessie"    ,"Bethany"    ,"Bette"    ,"Betty"    ,"Blair"    ,"Blanche"    ,"Braden"    ,"Bradley"    ,"Bramwell"    ,"Brandon"    ,"Breanna"    ,"Brenda"    ,"Brendan"    ,"Brennan"    ,"Bret"    ,"Brett"    ,"Brian"    ,"Briana"    ,"Brianne"    ,"Bridget"    ,"Brina"    ,"Britney"    ,"Bronwen"    ,"Brooklyn"    ,"Burdine"    ,"Byam"
    ,"Caden"    ,"Cadence"    ,"Calum"    ,"Cameron"    ,"Cardew"    ,"Carl"    ,"Carlene"    ,"Carlton"    ,"Carmelita"    ,"Caroline"    ,"Carolyn"    ,"Carolynn"    ,"Carrington"    ,"Cassandra"    ,"Cathal"    ,"Catriona"    ,"Cecilia"    ,"Cecily"    ,"Cedric"    ,"Celia"    ,"Chad"    ,"Chance"    ,"Charis"    ,"Charisse"    ,"Charity"    ,"Charles"    ,"Charlotte"    ,"Charmaine"    ,"Chay"    ,"Cheryl"    ,"Chet"    ,"Chloe"    ,"Chrissie"    ,"Chrissy"    ,"Christabel"    ,"Christiane"    ,"Christina"    ,"Christopher"    ,"Claribel"    ,"Clarissa"    ,"Clementine"    ,"Cleo"    ,"Clinton"    ,"Colin"    ,"Coloman"    ,"Conn"    ,"Connor"    ,"Cordelia"    ,"Corina"    ,"Cormac"    ,"Cowden"    ,"Craig"    ,"Cristalyn"    ,"Crystal"    ,"Curtis"    ,"Cuthbert"    ,"Cynthia"
    ,"Daisy"    ,"Daniel"    ,"Daphne"    ,"Darby"    ,"Daris"    ,"Darleen"    ,"Darlene"    ,"Darnell"    ,"Darrell"    ,"Darryl"    ,"David"    ,"Davina"    ,"Dawn"    ,"Deanna"    ,"Deanne"    ,"Debbie"    ,"Deborah"    ,"Dede"    ,"Delia"    ,"Demi"    ,"Denise"    ,"Dennis"    ,"Derek"    ,"Destiny"    ,"Devin"    ,"Diamond"    ,"Diana"    ,"Dickon"    ,"Dolores"    ,"Don"    ,"Donald"    ,"Donovan"    ,"Dora"    ,"Doreen"    ,"Dorian"    ,"Dorothy"    ,"Dougie"    ,"Douglas"    ,"Dove"    ,"Drusilla"    ,"Dulcie"
    ,"Eamonn"    ,"Earl"    ,"Ebenezer"    ,"Edgar"    ,"Edith"    ,"Edmund"    ,"Edna"    ,"Edward"    ,"Edwin"    ,"Edwina"    ,"Effie"    ,"Eileen"    ,"Elaine"    ,"Eleanor"    ,"Elektra"    ,"Elfriede"    ,"Elizabeth"    ,"Ella"    ,"Ellen"    ,"Elliot"    ,"Emil"    ,"Emily"    ,"Emma"    ,"Emory"    ,"Enid"    ,"Eric"    ,"Erika"    ,"Estelle"    ,"Ethel"    ,"Eudora"    ,"Eunice"    ,"Eva"    ,"Evan"    ,"Eve"    ,"Evelyn"
    ,"Faith"    ,"Felicity"    ,"Fiona"    ,"Flora"    ,"Florence"    ,"Floyd"    ,"Frank"    ,"Franklin"    ,"Frederick"    ,"Gabriel"    ,"Galenka"    ,"Galton"    ,"Gareth"    ,"Gaynor"    ,"Gemma"    ,"Genevieve"    ,"George"    ,"Georgiana"    ,"Gerald"    ,"Gerard"    ,"Gertie"    ,"Gertrude"    ,"Gia"    ,"Gilbert"    ,"Gladys"    ,"Glenda"    ,"Godfrey"    ,"Gorden"    ,"Gordon"    ,"Graham"    ,"Grant"    ,"Greenbury"    ,"Gregory"    ,"Greig"    ,"Gwen"    ,"Gwenda"    ,"Gwendolen"    ,"Gwendoline"    ,"Gwendolyn"
    ,"Hamish"    ,"Hannah"    ,"Harley"    ,"Harriet"    ,"Hastings"    ,"Hayden"    ,"Hayley"    ,"Hazel"    ,"Heather"    ,"Helen"    ,"Helton"    ,"Henrietta"    ,"Henry"    ,"Hero"    ,"Hervey"    ,"Hester"    ,"Heston"    ,"Holly"    ,"Honor"    ,"Hope"    ,"Howard"    ,"Hudson"    ,"Hugh"    ,"Hulda"    ,"Ian"    ,"Ida"    ,"Imelda"    ,"Imogen"    ,"India"    ,"Innogen"    ,"Iona"    ,"Irene"    ,"Iris"    ,"Isla"    ,"Ivy"
    ,"Jack"    ,"Jacqueline"    ,"Jacqui"    ,"Jade"    ,"Jaime"    ,"James"    ,"Jane"    ,"Jason"    ,"Jean"    ,"Jeanie"    ,"Jeffrey"    ,"Jemima"    ,"Jemma"    ,"Jenna"    ,"Jennifer"    ,"Jenny"    ,"Jenson"    ,"Jerald"    ,"Jerrold"    ,"Jerry"    ,"Jessica"    ,"Jessie"    ,"Jet"    ,"Jethro"    ,"Jigar"    ,"Jill"    ,"Joan"    ,"Joanna"    ,"Joanne"    ,"Jocelyn"    ,"Jodie"    ,"Joe"    ,"Joelle"    ,"Joey"    ,"John"    ,"Johnnie"    ,"Johnny"    ,"Joice"    ,"Jolyon"    ,"Jonas"    ,"Jonathan"    ,"Jonny"    ,"Jordan"    ,"Joseph"    ,"Josephine"    ,"Joshua"    ,"Joyce"    ,"Judith"    ,"Julia"    ,"Julian"    ,"Julianne"    ,"Julie"    ,"June"    ,"Juniper"    ,"Justin"    ,"Justine"
    ,"Kadia"    ,"Kale"    ,"Karen"    ,"Kate"    ,"Kathleen"    ,"Kathryn"    ,"Katie"    ,"Katy"    ,"Kaylee"    ,"Keaton"    ,"Ken"    ,"Kendall"    ,"Kendra"    ,"Kenneth"    ,"Kerr"    ,"Kerry"    ,"Kierra"    ,"Kimball"    ,"Kimberly"    ,"Kirsten"    ,"Kirsty"    ,"Kit"    ,"Kitty"    ,"Kristen"    ,"Kurt"    ,"Kylie"    ,"Kymber"
    ,"Lanny"    ,"Lara"    ,"Larry"    ,"Laura"    ,"Lauren"    ,"Laurence"    ,"Lawrence"    ,"Lawson"    ,"Lawton"    ,"Leah"    ,"Leanne"    ,"Lee"    ,"Leonora"    ,"Lesley"    ,"Lester"    ,"Lettice"    ,"Lexi"    ,"Liana"    ,"Lianne"    ,"Lilla"    ,"Lillian"    ,"Linda"    ,"Lisa"    ,"Liza"    ,"Lizabeth"    ,"Lizzie"    ,"Lois"    ,"Lorelei"    ,"Loretta"    ,"Lorna"    ,"Lorraine"    ,"Louisa"    ,"Louise"    ,"Luci"    ,"Lucinda"    ,"Lucius"    ,"Lucretia"    ,"Lucy"    ,"Lynn"    ,"Lynnette"
    ,"Mabel"    ,"Mackenzie"   ,"Maddox"    ,"Madelaine"    ,"Madeleine"    ,"Madge"    ,"Madison"    ,"Maggie"    ,"Malcolm"    ,"Malford"    ,"Mallory"    ,"Malvina"    ,"Marcia"    ,"Marcie"    ,"Marcus"    ,"Margaret"    ,"Marian"    ,"Marianne"    ,"Marilyn"    ,"Marissa"    ,"Marjorie"    ,"Mark"    ,"Marlene"    ,"Marsha"    ,"Marshall"    ,"Martin"    ,"Marvin"    ,"Mary"    ,"Mason"    ,"Matilda"    ,"Matt"    ,"Maud"    ,"Maude"    ,"Maurice"    ,"Mavis"    ,"Maximilian"    ,"May"    ,"Medea"    ,"Mehitable"    ,"Melanie"    ,"Melinda"    ,"Melissa"    ,"Melville"    ,"Michael"    ,"Michele"    ,"Mildred"    ,"Miles"    ,"Miley"    ,"Millicent"    ,"Mindi"    ,"Mindy"    ,"Minna"    ,"Moira"    ,"Molly"    ,"Morgan"    ,"Morris"    ,"Mort"    ,"Muriel"    ,"Murray"    ,"Mycroft"    ,"Myra"    ,"Myrna"    ,"Myron"    ,"Myrtle"
    ,"Nadine"    ,"Naila"    ,"Nancy"    ,"Naomi"    ,"Narcissa"    ,"Nate"    ,"Nathan"    ,"Neil"    ,"Nelson"    ,"Nevaeh"    ,"Nicholas"    ,"Nicola"    ,"Nicolas"    ,"Nicole"    ,"Nigel"    ,"Noel"    ,"Nora"    ,"Norman"    ,"Nowell"
    ,"Oliver"    ,"Olivia"    ,"Opal"    ,"Osbert"    ,"Oscar"    ,"Osric"    ,"Oswald"    ,"Ottilie"    ,"Ottiwell"    ,"Owen"    ,"Pamela"    ,"Paris"    ,"Parker"    ,"Pascal"    ,"Pascoe"    ,"Patience"    ,"Patrice"    ,"Patricia"    ,"Patrick"    ,"Patsy"    ,"Paul"    ,"Paula"    ,"Payton"    ,"Pearl"    ,"Peleg"    ,"Penelope"    ,"Percy"    ,"Persis"    ,"Philip"    ,"Philippa"    ,"Phillipps"    ,"Pippa"    ,"Poppy"    ,"Precious"    ,"Priscilla"
    ,"Quentin"    ,"Quinton"    ,"Quintus"    ,"Ralph"    ,"Ranald"    ,"Randall"    ,"Randel"    ,"Randell"    ,"Randi"    ,"Randolf"    ,"Randolph"    ,"Randy"    ,"Ranulf"    ,"Raven"    ,"Raymond"    ,"Rebecca"    ,"Reginald"    ,"Renesmee"    ,"Renssalaer"    ,"Reynold"    ,"Rhoda"    ,"Rhonda"    ,"Rhys"    ,"Richard"    ,"Richeldis"    ,"Riley"    ,"Rita"    ,"Robert"    ,"Roberta"    ,"Robin"    ,"Roderick"    ,"Rodger"    ,"Roger"    ,"Roland"    ,"Ronald"    ,"Ronnie"    ,"Rosaleen"   ,"Rosalie"    ,"Rosamund"    ,"Rose"    ,"Rosemary"    ,"Rosie"    ,"Rowan"    ,"Rowland"    ,"Roxanne"    ,"Ruby"    ,"Rufus"    ,"Rupert"    ,"Russell"    ,"Ruth"    ,"Ryan"
    ,"Sabrina"    ,"Samantha"    ,"Samuel"    ,"Sandra"    ,"Sanford"    ,"Sarah"    ,"Satyana"    ,"Savannah"    ,"Scarlett"    ,"Schuyler"    ,"Scott"    ,"Sebastian"    ,"Selina"    ,"Selma"    ,"Shahaf"    ,"Shanna"    ,"Sharon"    ,"Shayne"    ,"Sheridan"    ,"Sheryl"    ,"Shiloh"    ,"Sibyl"    ,"Sidney"    ,"Simon"    ,"Simone"    ,"Sophia"    ,"Sorley"    ,"Spencer"    ,"Stacy"    ,"Stephen"    ,"Steven"    ,"Stringer"    ,"Summer"    ,"Susan"    ,"Susanna"    ,"Suzanne"    ,"Swaine"    ,"Sylvia"
    ,"Tabitha"    ,"Talitha"    ,"Tallulah"    ,"Tammy"    ,"Tanith"    ,"Tanner"    ,"Tara"    ,"Tegan"    ,"Teresa"    ,"Terry"    ,"Thaddeus"    ,"Thelma"    ,"Theodore"    ,"Thomas"    ,"Thomasina"    ,"Thurza"    ,"Tiffany"    ,"Timothy"    ,"Tobias"    ,"Tom"    ,"Torquil"    ,"Tracy"    ,"Travis"    ,"Trent"    ,"Tyler"
    ,"Ulick"    ,"Ursula"    ,"Valerie"    ,"Vanessa"    ,"Velma"    ,"Venetia"    ,"Vera"    ,"Vicary"    ,"Victor"    ,"Victoria"    ,"Vincent"    ,"Violet"    ,"Virgil"    ,"Virginia"    ,"Vivian"
    ,"Wallis"    ,"Wanda"    ,"Warren"    ,"Wayne"    ,"Wendy"    ,"Whitney"    ,"Whittaker"    ,"Wilfred"    ,"Wilfried"    ,"William"    ,"Winifred"    ,"Winston"    ,"Woodrow"    ,"Yasmin"    ,"Yvette"    ,"Zadoc"    ,"Zelda"    ,"Zivar"    ,"Zuleika"
    ]
 
isBeginEntiny xs = or $ map (\x -> DL.isPrefixOf x xs) ["MATH","MATHDISP","CITE","REF"]

isPrefixOfT end begin s
               | null end  && (elem x begin) = True
               | (DL.isPrefixOf end s) && (elem x begin) = True
               | otherwise = False
                where
                    x = takeWhile (isAlphaNum) $ dropWhile (\c -> elem c spaces) $ drop (length end) s

isPrefixOfN end begin s
               | null end  && not (null sp) && (elem x begin) = True
               | (DL.isPrefixOf end s) && not (null sp)  && (elem x begin) = True
               | otherwise = False
                where
                    x = takeWhile (isAlphaNum) $ xx
                    (sp,xx) = span (== ' ') $ drop (length end) s

titles :: [String]
titles = [ "Ref.", "Refs.","Mr.", "Mrs.", "Dr.", "St.", "Prof.","Ms.","Sr.","Phd.","Sect.","B.Sc.",
           "phd.","sect.","dr.", "st.", "prof.","ms.","ref.", "refs.","fig.", "figs.", "tab.", "tabs.", "sec.", "secs.","eq.","eqs.","eqn.","eqns.","Ex.","Rem.","Lem.","Prop.", 
           "Fig.", "Figure.", "Figs.", "Tab.", "Table.", "Tabs.", "Sec.", "Secs.","Eq.","Eqs.","D.C.","Smt.","U.S.","B.A.","Ann."]

isName x = elem name names
           where
               name = takeWhile (isAlpha) $ dropWhile (== ' ') x

isTitle x [] = 0
isTitle x (a:as) = if DL.isPrefixOf a x then length a else isTitle x as


spaces :: String
spaces = " "

abbreviations :: [String]
abbreviations = [ "d.", "p.", "pp.", "cf.", "c.f.", "ca.", "eg.", "ie.", "i.e.", "e.g.", "vs.","etc.","no.","esp.","inc.","ltd.","co.","corp.","dept.","ann.",
                  "univ.","assn.","bross.","col.","gen.","lt.","adm.","viz.","et al.","al.","ph.d.","phd.","b.sc.","bsc.","s.t.","resp.","vol.","c.c.s.","w.r.t.","w.r.t",
                  "trans.","apr.","proc.","info.","sep.","sept.","nov.","dec.","jun.","feb.","mar.","jul.","aug.","oct.","par.","ref.","refs.","eq.","eqs.","r.v.","a.k.a."]

isAbbr x [] = 0
isAbbr x (a:as) = if (map (toLower) $ take (length a) x) == a then length a else isAbbr x as

isNum xx
       | null nums && null romanNums = 0
       | nums == "." = 0
       | nums == "," = 0
       | length nums > 1 && last nums == '.' = (length sp + (length nums))   --- + (length $ takeWhile (=='.') r)
       | length romanNums > 1 && last romanNums == '.' = ((length sp) + (length romanNums))   --- + (length $ takeWhile (=='.') r)
       | otherwise = 0
            where 
              (sp,x) = span (==' ') xx
              (nums,r) = span (\i -> isDigit i || elem i ".,") x
              (romanNums,r2) = span (\i -> elem i ".,xviXVI") x

isNumT xx
       | null nums && null romanNums = 0
       | nums == "." = 0
       | nums == "," = 0
       | length nums > 1 = (length sp + (length nums))   --- + (length $ takeWhile (=='.') r)
       | length romanNums > 1 = ((length sp) + (length romanNums))   --- + (length $ takeWhile (=='.') r)
       | otherwise = 0
            where 
              (sp,x) = span (==' ') xx
              (nums,r) = span (\i -> isDigit i || elem i ".,") x
              (romanNums,r2) = span (\i -> elem i ".,xviXVI") x

