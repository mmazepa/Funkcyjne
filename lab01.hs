-- ZADANIE 2 (gcd nie działa...(?!))

let gcd n 0 = n
let gcd n m = gcd m (mod n m)

--let lcm n m = ...

-- ZADANIE 3

let smaller x y = x < y
let greater x y = x > y
let equal x y = not(smaller x y) && not(greater x y)

let smaller_equal x y = not(greater x y)
let greater_equal x y = not(smaller x y)
let not_equal x y = not(equal x y)

-- ZADANIE 4 (coś tu jest nie tak...(?!))

let even n = if n >= 0 then True else odd(n-1)
let odd n = if n == 1 || n == -1 then True else even(n-1)

-- ZADANIE 5

let times x y = x * y
let plus x y = x + y

let same_values p1 p2 x y = p1 x y == p2 x y
