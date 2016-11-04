import CAR.AnnotationsFile
import CAR.Types
import CAR.CarExports

main :: IO ()
main = do
    let path = "hello.cbor"
    anns <- openAnnotations path
    return ()
