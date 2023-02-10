{-

data Direction = North | East | South | West
                 deriving (Show)

data Robot = Rover Direction Integer
           | Survey Integer [(Integer, String)]
             deriving (Show)
             
artoo, hal :: Robot
artoo = Survey 7 [(5,"dune"), (18,"swamp"), (25, "plans")]
hal = Survey 0 [(3,"pod"), (-6,"bay")]

pool, group :: [Robot]
pool = [Rover East 10, Rover South 4, Survey 8 [(1,"")], Rover North 5]
group = [Rover North 5, Rover West 17]

-}

data Protein = Chicken | Tofu | Beef
             deriving (Show)

data Topping = Lettuce | Salsa | Onions | Cheese
             deriving (Show)

data Size = Small | Large
          deriving (Show)

data MenuItem = Nachos Size
              | Taco Protein [Topping]
              deriving (Show)

price :: MenuItem -> Float
price (Nachos Small) = 8.95
price (Nachos Large) = 11.50
price (Taco p tp) = 10.25 + fromIntegral(length tp) * 0.5


supersize :: MenuItem -> MenuItem
supersize (Nachos p) = Nachos Large
supersize (Taco p pt) = Taco p (addCheese pt)
        where
           addCheese :: [Topping] -> [Topping]
           addCheese (Cheese: pts) = Cheese: Cheese: addCheese pts
           addCheese (pt: pts) = pt: addCheese pts
           addCheese [] = []
               

