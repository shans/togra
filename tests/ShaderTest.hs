import Control.Exception
import Graphics.UI.GLUT
import Shader
import Test.HUnit

requireError :: String -> IO a -> IO ()
requireError m e = do
  failed <- Control.Exception.catch (do e; return False) 
	      ((\a -> return True) :: SomeException -> IO Bool)
  assertBool m failed

-- Test that an incorrect vertex shader does not compile
test1 = TestLabel "badVertexShader" (
  TestCase (do
    requireError 
      "Invalid vertex shader didn't generate error"
      (prepareShaders ["this is clearly not valid C"]
		     ["void main() {gl_FragColor = vec4(0, 0, 0, 0)};"])))

test2 = TestLabel "minimalShader" (
  TestCase (do
    prepareShaders ["void main() {}\n"] ["void main() {}\n"]
    return ()))

tests = TestList [test1, test2]

main = do
  (progname, _) <- getArgsAndInitialize
  createWindow "test"
  runTestTT tests
