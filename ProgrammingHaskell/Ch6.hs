import Prelude hiding ((^), (!!), and, concat, replicate, elem)
import Test.QuickCheck

{-
1 乗算演算子*の再帰を参考にして、負でない整数に対する累乗演算子^を定義せよ。
  また、その定義を使って、2^3を簡約せよ。

  2 ^ 3 = 2 * (2 ^ 2)
        = 2 * (2 * (2 ^ 1))
        = 2 * (2 * (2 * (2 ^ 0)))
        = 2 * (2 * (2 * 1))
        = 2 * (2 * 2)
        = 2 * 4
        = 8
-}

(^) :: Int -> Int -> Int
x ^ 0 = 1
x ^ n = x * (x ^ (n-1))

{-
2 この章で与えた定義を使って、length [1,2,3]、drop 3 [1,2,3,4,5]、および
  init [1,2,3]を簡約せよ。

   length :: [a] -> Int
   length [] = 0
   length (_:xs) = 1 + length xs

   length [1,2,3] = 1 + length [2,3]
                  = 1 + (1 + length [3])
                  = 1 + (1 + (1 + length []))
                  = 1 + (1 + (1 + 0))
                  = 1 + (1 + 1)
                  = 1 + 2
                  = 3

   drop :: Int -> [a] -> [a]
   drop 0 [] = []
   drop 0 (x:xs) = x:xs
   drop (n+1) [] = []
   drop (n+1) (x:xs) = drop n xs

   drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5]
                      = drop 1 [3,4,5]
                      = drop 0 [4,5]
                      = [4,5]

   init :: [a] -> [a]
   init [_] = []
   init (x:xs) = x:init xs

   init [1,2,3] = 1 : init[2,3]
                = 1 : (2 : init [3])
                = 1 : (2 : [])
                = [1,2]
-}

{-
3 標準ライブラリを見ないで、以下のライブラリ関数を再帰を使って定義せよ。
 ・リストの要素が全てTrueであるか検査する。
   and :: [Bool] -> Bool
 ・リストのリストを取り、要素であるリストを連結する。
   concat :: [[a]] -> [a]
 ・指定された要素をn個持つリストを生成する
   replicate :: Int -> a -> [a]
 ・空でないリストのn番目の要素を取り出す。
   (!!) :: [a] -> Int -> a
 ・リストの要素に含まれるか検査する。
   elem :: Eq a => a -> [a] -> Bool

 標準ライブラリでは、これらの関数の多くは、再帰ではなく他のライブラリ関数を
 用いて定義されていることに注意せよ。
-}

and :: [Bool] -> Bool
and [] = True -- Trueがandの単位元(True and x = x 、または、 x and True = x)のため。
and (False:_) = False
and (True:xs) = and xs

concat :: [[a]] -> [a]
concat [] = []
concat [[]] = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
[] !! _ = error "index too large"
xs !! 0 = head xs
(x:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs) = e == x || elem e xs

{-
4. 関数merge::Ord a => [a] -> [a] -> [a]は、整列されたリストを二つ取り、一つの
   整列されたリストにして返す関数である。以下に使用例を示す。

   > merge [2,5,6] [1,3,4]
   [1,2,3,4,5,6]

   関数mergeを再帰を用いて定義せよ。ただし、関数insertやisortなど、整列された
   リストを処理する関数は利用してはならない。
-}

merge::Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xr) ys@(y:yr)
  | x < y = x : merge xr ys
  | otherwise = y : merge xs yr

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (drop 1 xs)
{-
  Mergeをテストするには、QuickCheckで2つの整列したリストxs、ysを生成しなければいけない。
  関数orderedListを使用すると整列したリストを生成できるのだが、以下のように書いても、ysには
  整列したリストではなくランダムな順序のリストしか渡されない。
   prop_merge :: Property
   prop_merge = forAll orderedList $ \xs ys -> isSorted (merge (xs::[Int]) ys)

  以下のようにすると、xs、ys共に整列したリストが渡される。

   prop_merge :: Property
   prop_merge  = forAll orderdList $ \xs -> forAll orderedList -> \ys -> isSorted (merge (xs::[Int]) ys)
-}
prop_merge :: Property
prop_merge = forAll orderedList $ \xs -> forAll orderedList $ \ys -> isSorted (merge (xs::[Int]) ys)

-- QuickCheck関連の型遊び。

-- forAll                                                :: Gen a -> (a   -> prop) -> Property
-- forAll orderedList                                    ::          ([a] -> prop) -> Property
-- forAll orderedList $                                  ::          ([a] -> prop) -> Property
-- forAll orderedList $ \xs ys -> isSorted (merge xs ys) ::                           Property

-- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
-- orderedList :: (Ord a, Arbitrary a) => Gen [a]

-- type Property = Gen Prop
-- newtype Prop -- Testable Prop
-- quickCheck :: Testable prop => prop -> IO ()
-- (==>) :: Testable prop => Bool -> prop -> Property

-- class Testable prop where
--   property :: prop -> Property
--   exhaustive :: prop -> Bool

-- quickCheck $ \xs ys -> isSorted xs && isSorted ys ==> prop_merge xs ys

-- \xs ys -> isSorted xs && isSorted ys :: [()] -> [()] -> Bool
-- \xs ys -> isSorted xs && isSorted ys ==> :: [()] -> [()] -> prop
