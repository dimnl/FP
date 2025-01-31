{-# LANGUAGE PackageImports #-}
module MakeZip where

-- You may use this file to generate a zip-file that follows the submission criteria.
-- No support is given for using this tool -  if it does not work (correctly), please do not spend too much time on getting it to work and create the zip-file by hand following the criteria.

-- Make sure the "-- Student 1" and "-- Student 2" lines are in each file, and only change the names and student numbers.
-- When you work alone, please do not change the "-- Student 2" line.

-- Installation:
-- Package: zip-1.2.0  (cabal install zip)
-- Ubuntu / Linux: libbz2-dev

import Codec.Archive.Zip
import Data.Maybe

getStudent :: String -> IO String
getStudent s = do putStrLn $ "Student number of student " ++ s
                  m <- getLine
                  return m

addFile :: String -> String -> ZipArchive ()
addFile prefix file = do entry <- mkEntrySelector $ prefix ++ "/" ++ file
                         loadEntry Store entry file

makeArchive :: String -> ZipArchive ()
makeArchive prefix = do addFile prefix "Introduction.idr"
                        addFile prefix "Matrix.idr"
                        addFile prefix "Selection.idr"
                        addFile prefix "Presentation.pdf"

data Student = Student { stname :: String, stnum :: String} deriving Eq

findstudent :: Int -> [String] -> Maybe Student
findstudent id [] = Nothing
findstudent id  (x:xs) | lbl == "-- Student 1: " && id == 1 = Just (Student (init name) (tail . init $ snum'))
                       | lbl == "-- Student 2: " && id == 2 = Just (Student (init name) (tail . init $ snum'))
                       | otherwise                          = findstudent id xs
                      where lbl          = take 14 x                   -- Ugly...
                            (name,snum') = span (/='(') $ drop 14 x    -- ... parser :-)

checkFile :: FilePath -> IO (Student, Student)
checkFile fname  = do contents <- lines <$> readFile fname
                      let s1 = findstudent 1 contents
                      let s2 = findstudent 2 contents
                      if (isNothing s1) then fail ("`-- Student 1' not specified in " ++ fname) else return ()
                      if (isNothing s2) then fail ("`-- Student 2' not specified in " ++ fname) else return ()
                      if (stnum (fromJust s1) == "sxxxxxxx" ||  stname (fromJust s1)  == "Your Name") then
                        do fail $ "Please add your name and student numbers to " ++ fname
                      else
                        return ()
                      return (fromJust s1, fromJust s2)

verifyFile :: FilePath -> (Student, Student) -> IO ()
verifyFile fname (s1,s2) = do (s1',s2') <- checkFile fname
                              if (s1 /= s1') then (fail $ "Incorrect student 1 in " ++ fname ++ ": it should match PComb.hs") else return ()
                              if (stnum s2 /= "syyyyyyy") then
                                if (s2 /= s2') then (fail $ "Incorrect student 2 in " ++ fname ++ ": it should match PComb.hs") else return ()
                              else
                                return ()

prompt :: String -> IO Char
prompt s = do putStrLn $ s ++ " [Y/N]"
              r <- getChar
              if (r `elem` "YN") then (return r) else (prompt s)

main :: IO ()
main = do (s1, s2) <- checkFile "PComb.hs"
          a1 <- prompt $ "[first student] Is  the student number of " ++ stname s1 ++ "`" ++ stnum s1 ++ "'?"
          if (a1 /= 'Y') then fail "Please update the student number in PComb.hs" else return ()
          let hass2 = stnum s2 /= "syyyyyyy" -- Is there a second student?
          putStrLn $ stnum s2
          if hass2 then
            do a2 <- prompt $ "[second student] Is the student number of " ++ stname s2 ++ "`" ++ stnum s2 ++ "'?"
               if (a2 /= 'Y') then fail "Please update the student number in PComb.hs" else return ()
          else
            putStrLn "No second student found in PComb.hs, assuming a group of one student"

          verifyFile "BasicParsers.hs" (s1,s2)
          verifyFile "MicroFP.hs" (s1,s2)

          let prefix = if hass2 then stnum s1 ++ "_" ++ stnum s2 else stnum s1
          putStrLn $ "Creating the archive: " ++ prefix ++ ".zip"
          createArchive (prefix ++ ".zip") (makeArchive prefix)
          putStrLn "Archive created! Please check if everything is correct before submitting."
