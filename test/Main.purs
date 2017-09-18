module Test.Main where

import Prelude

import Control.Monad.Aff (finally)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, exists, readFile, readTextFile, writeTextFile)
import Node.FS.Aff.Extra as E
import Node.FS.Aff.Extra.Simple (copyFile, mergeDir, mkdirSafe, overwriteDir, overwriteFile, rmdirRecurse)
import Node.FS.Aff.Extra.Types (From(..), Source(..), Target(..), To(..), filePath)
import Node.FS.Aff.Extra.Types as T
import Node.Path (sep)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: forall t93.                   
  Eff                         
    ( console :: CONSOLE      
    , testOutput :: TESTOUTPUT
    , avar :: AVAR            
    , buffer :: BUFFER
    , fs :: FS              
    | t93                     
    )                         
    Unit
main = runTest do
  suite "Node.FS.Aff.Extra" do
    
    let tmpDir = T.SystemPath "tmp"
    let tmpDir2 = T.SystemPath "tmp2"
    let tmpFile = tmpDir <> (T.SystemPath "tmpFile")
    let tmpFile2 = tmpDir <> (T.SystemPath "tmpFile2")

    let cleanup = do
          E.rmdirRecurse tmpDir
          E.rmdirRecurse tmpDir2

    test "safeMkdir" $ finally cleanup do
      E.mkdirSafe tmpDir
      Assert.assert "new tmp folder should exist" =<< exists (filePath tmpDir)
    
    test "rmdirRecurse" $ finally cleanup do
      E.mkdirSafe tmpDir      
      E.writeTextFile UTF8 tmpFile "test"
      Assert.assert "temp file should exist" =<< exists (filePath tmpFile)
      E.rmdirRecurse tmpDir
      Assert.assert "tmp folder should not exist" =<< (not <$> exists (filePath tmpDir))

    test "overwriteFile" $ finally cleanup do
      E.mkdirSafe tmpDir      
      E.writeTextFile UTF8 tmpFile "test"
      E.writeTextFile UTF8 tmpFile2 "test2"
      c1 <- readTextFile UTF8 (filePath tmpFile)
      c2 <- readFile (filePath tmpFile2)
      Assert.assert "temp file should have 'test' in it" (c1 == "test")
      E.overwriteFile tmpFile c2
      c3 <- readTextFile UTF8 (filePath tmpFile)
      Assert.assert "temp file should have 'test2' in it" (c3 == "test2")

    test "overwriteDir" $ finally cleanup do
      mkdirSafe "tmp"
      writeTextFile UTF8 "tmp/file" "test"
      writeTextFile UTF8 "tmp/file3" "test"
      mkdirSafe "tmp2"
      writeTextFile UTF8 "tmp2/file" "test2"
      writeTextFile UTF8 "tmp2/file2" "test22"
      E.overwriteDir (Target "tmp") (Source "tmp2")
      Assert.assert "file2 should exist in tmp folder" =<< exists "tmp/file2"
      c <- readTextFile UTF8 "tmp/file"
      Assert.assert "file should contain 'test2'" (c == "test2")
      Assert.assert "file3 should not exist" =<< (not <$> exists "tmp/file3")

    test "mergeDir" $ finally cleanup do
      mkdirSafe "tmp"
      writeTextFile UTF8 "tmp/file" "test"
      writeTextFile UTF8 "tmp/file3" "test"
      mkdirSafe "tmp2"
      writeTextFile UTF8 "tmp2/file" "test2"
      writeTextFile UTF8 "tmp2/file2" "test22"
      E.mergeDir (From "tmp2") (To "tmp")
      Assert.assert "file2 should exist in tmp folder" =<< exists "tmp/file2"
      c <- readTextFile UTF8 "tmp/file"
      Assert.assert "file should contain 'test2'" (c == "test2")
      Assert.assert "file3 should exist" =<< exists "tmp/file3"


  suite "Node.FS.Aff.Extra.Simple" do

    let tmpDir = "tmp"
    let tmpFile = tmpDir <> sep <> "tmpFile"
    let tmpFile2 = tmpDir <> sep <> "tmpFile2"
    let cleanup = do
          rmdirRecurse "tmp"
          rmdirRecurse "tmp2"

    test "safeMkdir" $ finally cleanup do
      mkdirSafe tmpDir
      Assert.assert "new tmp folder should exist" =<< exists tmpDir

    test "rmdirRecurse" $ finally cleanup do
      mkdirSafe tmpDir      
      writeTextFile UTF8 tmpFile "test"
      Assert.assert "temp file should exist" =<< exists tmpFile
      rmdirRecurse tmpDir
      Assert.assert "tmp folder should not exist" =<< (not <$> exists tmpDir)

    test "overwriteFile" $ finally cleanup do
      mkdirSafe tmpDir      
      writeTextFile UTF8 tmpFile "test"
      writeTextFile UTF8 tmpFile2 "test2"
      c1 <- readTextFile UTF8 tmpFile
      c2 <- readFile tmpFile2
      Assert.assert "temp file should have 'test' in it" (c1 == "test")
      overwriteFile tmpFile c2
      c3 <- readTextFile UTF8 tmpFile
      Assert.assert "temp file should have 'test2' in it" (c3 == "test2")
      
    test "overwriteDir" $ finally cleanup do
      mkdirSafe "tmp"
      writeTextFile UTF8 "tmp/file" "test"
      writeTextFile UTF8 "tmp/file3" "test"
      mkdirSafe "tmp2"
      writeTextFile UTF8 "tmp2/file" "test2"
      writeTextFile UTF8 "tmp2/file2" "test22"
      overwriteDir "tmp" "tmp2"
      Assert.assert "file2 should exist in tmp folder" =<< exists "tmp/file2"
      c <- readTextFile UTF8 "tmp/file"
      Assert.assert "file should contain 'test2'" (c == "test2")
      Assert.assert "file3 should not exist" =<< (not <$> exists "tmp/file3")

    test "mergeDir" $ finally cleanup do
      mkdirSafe "tmp"
      writeTextFile UTF8 "tmp/file" "test"
      writeTextFile UTF8 "tmp/file3" "test"
      mkdirSafe "tmp2"
      writeTextFile UTF8 "tmp2/file" "test2"
      writeTextFile UTF8 "tmp2/file2" "test22"
      mergeDir "tmp2" "tmp"
      Assert.assert "file2 should exist in tmp folder" =<< exists "tmp/file2"
      c <- readTextFile UTF8 "tmp/file"
      Assert.assert "file should contain 'test2'" (c == "test2")
      Assert.assert "file3 should exist" =<< exists "tmp/file3"

    test "copyFile" $ finally cleanup do
      mkdirSafe "tmp"
      writeTextFile UTF8 "tmp/file" "test"
      writeTextFile UTF8 "tmp/file2" "test2"
      mkdirSafe "tmp2"
      copyFile "tmp/file" "tmp2/file"
      Assert.assert "tmp2/file should exist" =<< exists "tmp2/file"
      copyFile "tmp/file2" "tmp2/file"
      c <- readTextFile UTF8 "tmp2/file"
      Assert.assert "file should contain 'test2'" (c == "test2")