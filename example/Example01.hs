import Control.Monad (when)
import Language.Halide

main :: IO ()
main = do
  let !host = hostTarget
  putStrLn $ "[+] host target is " <> show host
  when (hostSupportsTargetDevice (setFeature host FeatureOpenCL)) $ do
    putStrLn "[+] OpenCL is supported! Testing ..."
    testOpenCL
  when (hostSupportsTargetDevice (setFeature host FeatureCUDA)) $ do
    putStrLn "[+] CUDA is supported! Testing ..."
    testCUDA