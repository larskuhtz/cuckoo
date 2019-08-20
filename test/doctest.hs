import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Data/Cuckoo.hs"]
