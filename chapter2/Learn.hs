module Learn where


exchangeArgumentPlaces :: (a -> b -> c) -> (b -> a -> c)
exchangeArgumentPlaces f x y = f y x

area x = 3.14 * (x * x)
double x = x * 2

x = 7
y = 10
f = x + y
