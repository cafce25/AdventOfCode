import Data.List
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Digest.Pure.MD5

main = print . head $ [(x, md5Sum x)  | x <- [0..], replicate 6 '0' `isPrefixOf` md5Sum x]
    where md5Sum x = show.md5.LB.pack $ "bgvyzdsv" ++ show x
