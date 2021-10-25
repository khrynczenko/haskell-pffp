data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultR'Us | TakeYourCahncesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Int deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. What is the type of myCar
-- Its type is Vehicle
--
-- 2. Implement following functions

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> Bool
areCars = foldl (flip ((&&) . isCar)) True

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4. What will happes if we use Plane as argument?
-- The function will fail with an error. It is a partia function to be exact
-- and it is bottom.

