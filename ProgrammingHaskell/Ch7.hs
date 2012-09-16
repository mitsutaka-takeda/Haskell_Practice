module Ch7 where

import Prelude hiding (map, iterate)
import Data.Char (ord,chr)
import Test.QuickCheck

{-
5. 以下の定義が謝りである理由を説明せよ。
  sumsqreven = compose [sum, map (^2), filter even]

  composeは、型aから型aへの関数のリストを受けとらなければいけない。
  関数sumは、型aのリストから型aへの関数であるため、composeへのリスト引数として
  渡せない。

  compose :: [a -> a] -> (a -> a)
  compose = foldr . id

  sum :: Num a => [a] -> a
  even :: Integral a => a -> Bool
  filter :: (a -> Bool) -> [a] -> [a]
  map :: (a -> b) -> [a] -> [b]
-}

{-
6. 標準ライブラリの定義を見ないで、以下の二つの高階関数を定義よ。

 ・「引数に組を取る関数」を「カリー化された関数」へ変換するcurry。
 ・「引数が二つのカリー化された関数」を「引数に組を取る関数」へ変換するuncurry
-}

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f (x,y) = f x y

{-
7. 高階関数unfoldは、リストを生成する簡単な再帰の様式を閉じ込める。
   この関数は、以下のように定義できる。
     unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
     unfold p h t x | p x = []
                    | otherwise = h x: unfold p h t (t x)

  すなわち、関数unfold p h t は、述語pを引数に適用した結果Trueとなれば、
  空リストを返す。そうでなければ、関数hを引数へ適用することで先頭の
  要素を作り、残りのリストは自分自身を呼び出すことで生成して、
  全体として空でないリストを作る。再帰する際には、関数tを引数に適用して、
  新たな引数を作る。たとえば関数unfoldを用いると、関数int2binは
  もっと簡潔に書き直せる。

    int2bin = unfold (==0) (`mod` 2) (`div` 2)

  関数unfoldを用いて関数chop8、map f、iterate f を再定義せよ。
-}
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x: unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (null) (take 8) (drop 8)

map :: (a -> b) -> [a] -> [b]
map f = unfold (null) (f . head) tail

-- 初めて関数const使った。。。
iterate :: (a -> a) -> a -> [a]
iterate f = unfold (const False) f f

{-
8. パリティービットを用いることで、文字列を通信するプログラムを軽度の
   通信エラーを検出できるように変更せよ。すなわち、8ビットの二進表記
   は、符号化の際にパリティービットを付加される。パリティービットは、
   1個の個数が奇数であれば1,そうでなければ0となる。逆に復号の際は、
   9ビットの二進表記のパリティービットが正しいか検査する。正しければ
   パリティービットを捨て、誤りであればパリティーエラーを報告する。
-}

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

parityBit :: [Bit] -> Bit
parityBit bits
  | (odd . length . filter odd) (make8 bits) = 1
  | otherwise = 0

{-
lenth . filter odd = foldl countUp 0 . filter

-}

{-
  lengthをfoldlを使用して定義したい。
    foldl :: (a -> b -> a) -> a -> [b] -> a
    length :: [b] -> Int

  この式を眺めていると、関数fと定数eが必要となる。
    f :: Int -> b -> Int
    e :: Int

    foldr :: (a -> b -> b) -> b -> [a] -> b
-}
length' :: [a] -> Int
length' = foldl countUp 0

countUp :: Int -> a -> Int
countUp x _ = x + 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr check []
  where check x xs = if p x then x:xs else xs
