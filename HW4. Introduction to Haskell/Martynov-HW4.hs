module Martynov_HW4
where

-- Задача 1.
-- Реализуйте функцию, описывающую плотность равномерного распределения на промежутке от 3 до 4.
udistribution34 x = udistribution x 3 4

udistribution x a b | x >= a && x <= b = 1 / (b - a)
                    | otherwise        = 0
-- см. http://www.haskell.org/haskellwiki/Pattern_guard

-- Задача 2.
-- Реализуйте функцию, находящую наибольший общий делитель двух целых чисел с помощью алгоритма Евклида
_gcd a b = _gcd' (abs a) (abs b)
_gcd' a b | b == 0    = a
          | otherwise = _gcd' b (a `rem` b)
-- см. http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:rem

-- Задача 3.
-- Реализуйте функции, находящие сумму и количество цифр заданного целого числа
digs :: Integral x => x -> [x]
digs x | x == 0     = []
       | otherwise  = digs (x `div` 10) ++ [x `mod` 10]

-- http://www.haskell.org/tutorial/arrays.html

digs_sum x    = sum    $ digs $ abs x

digs_length x = length $ digs $ abs x

-- Задача 4.
-- Реализуйте функцию, находящую элементы следующей рекурентной последовательности
-- a_0 = 1; a_1 = 2; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} − 2a_k .
-- Попытайтесь найти эффективное решение.

task4 x | x< 0             = error "Incorrect value"
        | x == 0           = 1
        | x == 1           = 2
        | x == 2           = 3
        | otherwise        = p1
          where (p1,_,_,_) = loop_t4 (task4 2, task4 1, task4 0, x - 3)

loop_t4 (p1,p2,p3,p4) | p4 == 0    =         (p1 + p2 - 2 * p3, p1, p2, 0)
                      | otherwise  = loop_t4 (p1 + p2 - 2 * p3, p1, p2, p4 - 1)


-- см. http://www.haskell.org/haskellwiki/The_Fibonacci_sequence

-- Задача 5.
-- Реализуйте функцию, находящую значение определённого интеграла от заданной функции на заданном интервале по методу трапеций.

--7. Реализовать функцию, находящую значение определенного интеграла на отрезке методом трапеций.
integral f (a,b) = integral' f (a,b) 0.1 0

integral' f (a,b) s r | a + s >= b  = r + (trap f a $ b - a)
                      | otherwise   = integral' f (a + s, b) s (r + (trap f a s))

trap f a s = ((f a) + (f $ a + s)) * s / 2

-- см. http://en.wikipedia.org/wiki/Trapezoidal_rule#C.2B.2B

-- Задача 6.
-- Реализуйте функцию, находящую значение квадратного корня методом Ньютона

sqrt_newton n | n >= 0    = loop_t6 1 n 0.000001
              | otherwise = error "Incorrect value"

loop_t6 x n eps | abs (x - nx) > eps  = loop_t6 nx n eps
                | otherwise           = nx
                  where nx            = 0.5 * (x + n / x)

-- см. http://e-maxx.ru/algo/roots_newton
