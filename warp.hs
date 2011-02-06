import Controller
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = putStrLn "Loaded" >> withDnP (run 3000)
