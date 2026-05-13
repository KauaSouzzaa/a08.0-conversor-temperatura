module Temperatura (main, converteTemperatura) where

import Data.Maybe (fromJust, isNothing)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

converteTemperatura :: Double -> Maybe (Double, Double)
converteTemperatura celsius
 | celsius < zeroAbsoluto = Nothing
 | otherwise = Just (fah,kel)
 where
    fah = (celsius*9 / 5) + 32
    kel = celsius - zeroAbsoluto
    zeroAbsoluto = -273.15

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        putStr "Digite a temperatura em Celsius: "
        entrada <- getLine
        let maybeTemperatura = readMaybe entrada :: Maybe Double
        if isNothing maybeTemperatura
          then
            putStrLn "Entrada inválida!"
          else do
            let temp = converteTemperatura (fromJust maybeTemperatura)
            if isNothing temp
              then 
                putStrLn "Entrada inválida!"
              else do
                let fah = fst (fromJust temp)
                let kel = snd (fromJust temp)

                putStr (printf "Temperatura em Fahrenheit: %.2f\n" fah :: String)
                putStr (printf "Temperatura em Kelvin: %.2f\n" kel :: String)
            
