import Test.Framework (defaultMain)
import Unit.Matches

tests = [unit_matches]

main :: IO ()
main = defaultMain tests
