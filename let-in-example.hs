-- main :: IO()

geometry :: Floating t => t -> (t,t)
geometry radius =
  let diameter = 2 * radius
      circumference = pi * diameter
  in (diameter, circumference)


f :: () -> String
f () =
  let x = Just Nothing
      in
        case x of
          (Just _) -> "something"
          Nothing -> "nothing"
