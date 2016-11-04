import AnnotationsFile
import Types
import CarExports

main :: IO ()
main = do
    let path = "hello.cbor"
    anns <- openAnnotations path
    return ()
