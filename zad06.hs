kwad :: (Float, Float, Float) -> (Float, Float)
kwad (a, b, c) = (x1, x2)
	where
		x1 = (-b / (2 * a)) + sqrt (b * b - 4 * a * c) / (2 * a)
		x2 = (-b / (2 * a)) - sqrt (b * b - 4 * a * c) / (2 * a)

main = do
   putStrLn "===> kwad 3 -11 -4"
   print(kwad (3, -11, -4))
