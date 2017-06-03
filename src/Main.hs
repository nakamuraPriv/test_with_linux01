{-# LANGUAGE GADTs #-}

module Main(main) where

import System.Random
import Control.Exception
import Control.Monad
import Data.Char

data Operation = Operaiton{
    operationName :: String,
    func :: IO()
}

selectSpeakerOp :: Operation
selectSpeakerOp = undefined

main :: IO()
main = do
    dispMessage "処理を選択してください"
    putStrLn "1.スピーチ候補を選ぶ"
    putStrLn "2.既にスピーチした生徒を表示する"
    putStrLn "3.まだスピーチしていない生徒を表示する"
    putStrLn "4.処理を終了する"
    putStrLn "↓"
    inputStr <- getLine 
    if (all isNumber inputStr)
        then do
            let
                inputNum :: Integer
                inputNum = read inputStr
            case inputNum of
                1 -> selectSpeaker
                2 -> showSpeachedStudents
                3 -> showUnSpeachedStudents
                _ -> return ()
        else do
            dispMessage "数字で入力してください"
            main


getRangeRandomNum :: Integer -> Integer -> IO Integer
getRangeRandomNum from to = getStdRandom (randomR (from, to))

getRandomNumsWithRange :: Integer -> Integer -> Integer -> IO [Integer]
getRandomNumsWithRange num from to = do
    let result = []
    subfunc []
    where subfunc res
            | 1 + abs (to - from) < num = getRandomNumsWithRange (1 + abs (to - from)) from to
            | num == fromIntegral (length res)      = return res
            | otherwise                             = do
                newNum <- getRangeRandomNum from to
                if elem newNum res
                    then subfunc res
                    else subfunc (newNum : res)

showUnSpeachedStudents :: IO()
showUnSpeachedStudents = do
    dispMessage "まだスピーチしていない生徒を表示します"
    dispMessage "参照するファイル名を入力してください(-m:メニューに戻る)"
    putStrLn "↓"
    input <- getLine
    if input == "-m"
        then backToMenu
        else do
            readResult <- readDataOfStudentsFromFile input
            case readResult of
                Just (_, _, unSelectedStudents) -> do
                    putStrLn . showStudents $ unSelectedStudents
                    main
                Nothing -> do
                    dispMessage "そのようなファイルは存在しません"
                    showSpeachedStudents

showSpeachedStudents :: IO()
showSpeachedStudents = do
    dispMessage "既にスピーチをした生徒を表示します"
    dispMessage "参照するファイル名を入力してください(-m:メニューに戻る)"
    putStrLn "↓"
    input <- getLine
    if input == "-m"
        then backToMenu
        else do
            readResult <- readDataOfStudentsFromFile input
            case readResult of
                Just (_, selectedStudents, _) -> do
                    putStrLn . showStudents $ selectedStudents
                    main
                Nothing -> do
                    dispMessage "そのようなファイルは存在しません"
                    showSpeachedStudents

backToMenu :: IO()
backToMenu = do
    dispMessage "メニューに戻ります"
    main

selectSpeaker :: IO()
selectSpeaker = do
    dispMessage "参照するファイル名を入力してください(-m:メニューに戻る)"
    putStrLn "↓"
    input <- getLine
    if input == "-m"
        then backToMenu
        else do
            readResult <- readDataOfStudentsFromFile input
            case readResult of
                Just (_,_,unSelectedStudents) -> do
                    dispMessage "選ぶ人数を入力してください"
                    putStrLn "↓"
                    inputNum <- (reads <$> getLine) :: IO [(Integer, String)]
                    case inputNum of
                        [] -> do
                            dispMessage "数値で入力してください"
                            selectSpeaker
                        [(num, _)] -> do
                            selectedNums <- getRandomNumsWithRange num 1 (fromIntegral . length $ unSelectedStudents)
                            let speakers = getStudentsForNums selectedNums unSelectedStudents
                            dispMessage "選出された生徒↓"
                            putStrLn . showStudents $ speakers
                            main 
                Nothing -> do
                    dispMessage "そのようなファイルは存在しません"
                    selectSpeaker



readDataOfStudentsFromFile :: String -> IO (Maybe ([Student],[Student],[Student]))
readDataOfStudentsFromFile fileName = do
    do
        csvStrings <- readFile (checkFileName fileName)
        let allStudents = map csvLineToStudent $ lines csvStrings
            selectedStudents = filter hasSelected allStudents
            unSelectedStudents = filter (not . hasSelected) allStudents
        return (Just (allStudents, selectedStudents, unSelectedStudents))
        `catch` \(SomeException ex) -> do
            return Nothing

getStudentsForNums :: [Integer] -> [Student] -> [Student]
getStudentsForNums nums students = subfunc nums students []
    where
        subfunc :: [Integer] ->[Student] -> [Student] -> [Student]
        subfunc [] _ result = result
        subfunc nss@(n:ns) students result
            | length nss > length students = subfunc (take (length students) nss) students result
            | 1 <= n && n <= (fromIntegral . length $ students) =
                let index :: Int
                    index = fromIntegral (n - 1)
                in  subfunc ns students ((students !! index) : result)
            | otherwise                                         = result

showStudents :: [Student] -> String
showStudents students = subfunc students ""
    where
        subfunc :: [Student] -> String -> String
        subfunc [] temp     = temp
        subfunc (x:xs) temp = subfunc xs ("[ " ++ (name x) ++ " ]\n" ++ temp)

dispMessage :: String -> IO()
dispMessage message = putStrLn $ "$$" ++ message

--拡張子がついていなければ.csvをつける
checkFileName :: String -> String
checkFileName = addCSV . splitByFirstDot

addCSV :: String -> String
addCSV fileName = fileName ++ ".csv"

splitByFirstDot :: String -> String
splitByFirstDot str =
    let
        subFunc :: (String, String) -> String
        subFunc (_, []) = ""
        subFunc (checked, (x:xs))
            | x == '.' = reverse checked
            | xs == [] = reverse $ x : checked 
            |otherwise = subFunc (x : checked, xs)
    in
        subFunc ([], str)

data Student = Student {
    student_id :: Integer,
    name :: String,
    hasSelected :: Bool}
    deriving(Show)



csvLineToStudent :: String -> Student
csvLineToStudent csv =
    let
        params = words $ commaToSpace csv
    in
        makeStudent params

spaceToUnderScore :: String -> String
spaceToUnderScore str = subFunc [] str
    where
        subFunc :: String  -> String -> String
        subFunc result [] = reverse result
        subFunc result (x:xs)
            | x == ' '  = subFunc ('_':result) xs
            | otherwise = subFunc (x:result) xs

makeStudent :: [String] -> Student
makeStudent (_id_str:_name:_hasSelected_str:xs) =
    let
        _student_id = read _id_str :: Integer
        tAndNilToTrueFalse :: String -> Bool
        tAndNilToTrueFalse "t"   = True
        tAndNilToTrueFalse "nil" = False
        tAndNilToTrueFalse _     = False
        _hasSelected = tAndNilToTrueFalse _hasSelected_str
    in
        Student{
            student_id = _student_id,
            name = _name,
            hasSelected = _hasSelected
        }

commaToSpace :: String -> String
commaToSpace csv = subFunc [] csv
    where
        subFunc result [] = reverse result
        subFunc result (x:xs)
            | x == ',' = subFunc (' ' : result) xs
            |otherwise = subFunc (x : result) xs
