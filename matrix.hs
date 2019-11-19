{-# LANGUAGE FlexibleInstances #-}

{-
Copyright (c) 2019 Jeremy Adam Hart

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{---------------------
  This is intended to be a very minimal matrix/vector library little consideration has been put into efficiency

  The operators:
        *.
        .*
        .*.
        .+.
        .-.
  Have been added for operations between matrices and scalars.

  General usage is that there is always a '.' on a side of the operator that accepts a matrix
------------------------}

--Column major matrix
data Matrix a = Mat [[a]] deriving(Eq, Show)

bimap::(a->b->c)->[a]->[b]->[c]
bimap f (a:b) (c:d) = (f a c):(bimap f b d)
bimap f [] [] = []

--Vector 'constructor'
vec::[Float]->(Matrix Float)
vec l = Mat [l]

class Addable a where
        (.+.)::a->a->a
        (.-.)::a->a->a

class Scalable a where
        (*.)::Float->a->a
        (.*)::a->Float->a

instance Addable ([] Float) where
        a .+. b = bimap (+) a b
        a .-. b = bimap (-) a b

instance Addable (Matrix Float) where
        (Mat a) .+. (Mat b) = Mat (bimap (.+.) a b)
        (Mat a) .-. (Mat b) = Mat (bimap (.-.) a b)

instance Scalable ([] Float) where
        a .* s = map (s*) a
        s *. a = a .* s

instance Scalable (Matrix Float) where
        (Mat a) .* s = Mat (map (s*.) a)
        s *. (Mat a) = (Mat a) .* s 

--Frobenius norm of matrix/Regular norm of vector
norm::(Matrix Float)->Float
norm (Mat m) = sqrt (foldr (\u v-> (foldr (\x y-> x*x+y) 0 u) + v) 0 m)

dot::(Matrix Float)->(Matrix Float)->Float
dot (Mat a) (Mat b) = foldr (\x y-> x + y) 

colMult::[[Float]]->[Float]->[[Float]]->[[Float]]
colMult m c t = (foldl1 (.+.) (bimap (.*) m c)):t

--Matrix multiplication
(.*.)::(Matrix Float)->(Matrix Float)->(Matrix Float)
(Mat a) .*. (Mat b) = Mat (foldr (colMult a) [] b)
