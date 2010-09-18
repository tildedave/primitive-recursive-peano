import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import GHC.IOBase
import System.Cmd(system) 

-- taken from http://gbacon.blogspot.com/2009/06/setting-up-simple-test-with-cabal.html

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
  where testprog = (buildDir lbi) ++ "/test"