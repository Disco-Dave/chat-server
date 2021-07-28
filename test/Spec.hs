import qualified ChatSpec
import Test.Hspec ( hspec, describe, parallel )

main :: IO ()
main =
  hspec . parallel $
    describe "Chat" ChatSpec.spec
