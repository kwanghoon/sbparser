module Main where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import Config

import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Smallbasic" $ do
    let benchmark1_sb = "If x "              -- examples/exp/benchmark1.sb
    let benchmark2_sb = "If x < 10 Then\n"   -- examples/exp/benchmark2.sb

    let config_simple = True
    
    let config =
          Configuration
            {
              config_SIMPLE       = config_simple,
              config_R_LEVEL      = 1,
              config_GS_LEVEL     = 1,
              config_DEBUG        = False,
              config_DISPLAY      = False,
              config_PRESENTATION = 0,
              config_ALGORITHM    = 3
            }
    
    it ("[Benchmark1] " ++ benchmark1_sb) $
      do results <- computeCand False benchmark1_sb "" config_simple
         mapM_ (item benchmark1_sb config) [1..9]  -- Max GS level (9) for Smallbasic

    it ("[Benchmark2] " ++ benchmark2_sb) $
      do results <- computeCand False benchmark2_sb "" config_simple
         mapM_ (item benchmark2_sb config) [1..9]  -- Max GS level (9) for Smallbasic

item benchmark init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         case configMaybe of
           Just config ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

main :: IO ()
main = spec
