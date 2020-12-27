import Data.List
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Digest.Pure.MD5

main = print . head $ [(x, md5Sum x)  | x <- [254575..], "000000" `isPrefixOf` md5Sum x]
    where md5Sum x = show.md5.LB.pack $ "bgvyzdsv" ++ show x
