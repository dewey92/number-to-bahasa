module Main (main) where

import System.IO
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trim = dropWhileEnd isSpace . dropWhile isSpace

-- Memisahkan angka per 3 nol e.g 1234567 -> [1,234,567]
pisahAngka :: String -> [String]
pisahAngka ns = pisah' panjangString 3 ns where
  panjangString = length ns
  pisah' lxs _ [] = []
  pisah' lxs n xs
    | sisa /= 0 = (take sisa xs) : (pisah' (panjangString-sisa) n $ drop sisa xs) -- pertama dipanggil
    | sisa == 0 = (take n xs) : (pisah' lxs n $ drop n xs) -- dipanggil seterusnya
    where
      sisa = lxs `rem` n

satuan n
  | n == '0' = "nol"
  | n == '1' = "satu"
  | n == '2' = "dua"
  | n == '3' = "tiga"
  | n == '4' = "empat"
  | n == '5' = "lima"
  | n == '6' = "enam"
  | n == '7' = "tujuh"
  | n == '8' = "delapan"
  | n == '9' = "sembilan"

puluhan "10"    = "sepuluh"
puluhan "11"    = "sebelas"
puluhan ['1',n] = satuan n ++ " belas"
puluhan ['0',s] = satuan s
puluhan [p,'0'] = satuan p ++ " puluh"
puluhan [p,s]   = satuan p ++ " puluh " ++ satuan s

ratusan "100"    = "seratus"
ratusan (r:"00") = satuan r ++ " ratus"
ratusan (r:ps)
  | r == '0' || r == '1' = "seratus " ++ puluhan ps
  | otherwise            = satuan r ++ " ratus " ++ puluhan ps

bacaAngka :: String -> String
bacaAngka n
  | n == "000" = ""
  | a < 10     = satuan $ head $ show a
  | a < 100    = puluhan $ show a
  | a < 1000   = ratusan $ show a
  where a = read n :: Int

bacaKelipatanSeribu :: [String] -> [String]
bacaKelipatanSeribu xs = reverse $ map bacaKelipatanSeribu' $ zip (reverse xs) [1,2..] where
  bacaKelipatanSeribu' :: (String, Int) -> String
  bacaKelipatanSeribu' pair
    | null bahasaAngka = ""
    | counter == 1 = bahasaAngka
    | counter == 2 = if bahasaAngka == "satu" then "seribu" else bahasaAngka ++ " ribu"
    | counter == 3 = bahasaAngka ++ " juta"
    | counter == 4 = bahasaAngka ++ " milyar"
    | counter == 5 = bahasaAngka ++ " triliun"
    | counter == 6 = bahasaAngka ++ " kuadriliun"
    | counter == 7 = bahasaAngka ++ " kuantiliun"
    | counter == 8 = bahasaAngka ++ " sekstiliun"
    | counter == 9 = bahasaAngka ++ " septiliun"
    | counter == 10 = bahasaAngka ++ " oktiliun"
    | counter == 11 = bahasaAngka ++ " noniliun"
    | counter == 12 = bahasaAngka ++ " desiliun"
    | otherwise = ""
    where
      bahasaAngka = fst pair
      counter     = snd pair

ubahAngkaKeBahasa :: Int -> String
ubahAngkaKeBahasa = trim . unwords . bacaKelipatanSeribu . map bacaAngka . pisahAngka . show

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Masukkan angkamu: "
  inputanAngka <- getLine
  let toInt = read inputanAngka :: Int
  putStrLn $ inputanAngka ++ " -> " ++ ubahAngkaKeBahasa toInt
