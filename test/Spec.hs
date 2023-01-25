module Main where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import Config

import Data.Maybe (isJust)
import System.IO (readFile)

spec = hspec $ do
  describe "Smallbasic" $ do
    -- let benchmark1_sb = "If x "              -- examples/exp/benchmark1.sb
    -- let benchmark2_sb = "If x < 10 Then\n"   -- examples/exp/benchmark2.sb
    
    let config_simple = True
    let max_gslevel = 9
    
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
    
    let benchmark1_sb = "./examples/exp/benchmark1.sb"
    
    it ("[Benchmark1] ") $
      do mapM_ (itemText "If x " config) [1..max_gslevel]  -- Max GS level (9) for Smallbasic

    let benchmark2_sb = "./examples/exp/benchmark2.sb"
    
    it ("[Benchmark2] ") $
      do mapM_ (item benchmark2_sb "" config) [1..max_gslevel]  -- Max GS level (9) for Smallbasic

    let benchmark3_sb = "./examples/exp/benchmark3.sb"

    it ("[Benchmark3] ") $
      do mapM_ (item benchmark3_sb "If x " config) [1..max_gslevel]  -- Max GS level (9) for Smallbasic

    let benchmark4_sb = "./examples/exp/benchmark4.sb"
    
    it ("[Benchmark4] ") $
      do mapM_ (item benchmark4_sb "If x < 10 Then\n" config) [1..max_gslevel]  -- Max GS level (9) for Smallbasic


item benchmark_file text init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         benchmark_prefix <- readFile benchmark_file
         let benchmark = benchmark_prefix ++ text
         -- putStrLn $ "[" ++ benchmark ++ "]"
         case configMaybe of
           Just _ ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

itemText benchmark init_config gslevel = 
      do let test_config = init_config{config_GS_LEVEL=gslevel}
         putStrLn (show test_config)
         
         configMaybe <- readConfig
         -- putStrLn $ "[" ++ benchmark ++ "]"
         case configMaybe of
           Just _ ->
             do writeConfig test_config  -- set
                results <- computeCand False benchmark "" (config_SIMPLE test_config)
                writeConfig init_config       -- restore
                
           Nothing -> isJust configMaybe `shouldBe` True

main :: IO ()
main = spec
