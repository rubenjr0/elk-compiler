import Parser.CustomTypeSpec qualified
import Parser.FunctionSpec qualified
import Parser.ProgramSpec qualified
import Parser.TypeSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Parsing" $ do
        describe "Types" Parser.TypeSpec.spec
        describe "Custom Types" Parser.CustomTypeSpec.spec
        describe "Functions" Parser.FunctionSpec.spec
        describe "Programs" Parser.ProgramSpec.spec
