import Test.Hspec
import qualified FileSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Network.Wai.Servlet.File" FileSpec.spec
