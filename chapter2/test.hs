sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Int -> Int
triple x = x * x * x

square x = x * x

half x = x / 2

piTimes x = pi * x

perimeter x y = x * 2 + y * 2
