import Robots

-- Name: Carlo Pisacane
-- Email: cfpisaca@syr.edu

-- SOURCES 
-- This work was done all by myself and I have consulted no soruces other than slides and other class materials


----- 1 -----
-- PURPOSE
-- move (x,y) rbt has the effect of moving rbt to the location that is x units east and y units north of its current location.

-- DEFINITION
move :: (Integer,Integer) -> Robot -> Robot
move (x,y) (Drone (a,b) c d) = Drone ((a+x),(y+b)) c d
move (x,y) (Rover (a,b) c d) = Rover ((a+x),(y+b)) c d 

-- TESTS
-- move (2,3) (Drone (0,0) (Just Game) [])
-- move ((-1),9) (Drone (8,4) (Just Book) ["End Book"])
-- move (0,0) (Drone (1,1) Nothing [])
-- move (0,0) (Rover (1,1) [] [])
-- move (0,0) (Rover (1,1) [] [])


----- 2 -----
-- PURPOSE
-- reset rbt has the effect of erasing the log of rbt and removing any items held by rbt.

-- DEFINITION
reset :: Robot -> Robot
reset (Drone a b c) = Drone a Nothing []
reset (Rover a b c) = Rover a [] []

-- TESTS
-- reset (Drone (2,1) (Just Book) ["Pickup Novel","End Book"])
-- reset (Rover (3,5) [Book] [("Page",(2,1))])
-- reset (Drone (0,(-1)) Nothing [])
-- reset (Rover (1,1) [] [])


----- 3 -----
-- PURPOSE
-- addEntry str rbt returns the robot state obtained by adding str to the front of rbt’s log.

-- DEFINITION
addEntry :: String -> Robot -> Robot
addEntry str (Drone a b c) = Drone a b (str:c)
addEntry str (Rover a b c) = Rover a b ((str,a):c)

-- TESTS
-- addEntry "sleep" (Drone (1,43) (Just Toy) ["Audio","Stop"])
-- addEntry "sleep" (Rover (83,2) [Book] [("Page",(2,1))])
-- addEntry "" (Rover (0,0) [Game] [("Gamemode",(9,3))])
-- addEntry "end" (Drone (1,2) (Just Book) [])


----- 4 -----
-- PURPOSE
-- redact str rbt removes all occurrences of the string str from its log.

-- DEFINITION
redact :: String -> Robot -> Robot
redact str (Drone a b c) = Drone a b (filter (\x -> x /= str) c)
redact str (Rover a b c) = Rover a b (filter (\(x,y) -> x /= str) c)

-- TESTS
-- redact "Game" (Drone (5,4) (Just Game) ["Pickup Game","wake"])
-- redact "Pickup Game" (Drone (5,4) (Just Game) ["Pickup Game","wake"])
-- redact "Pickup Book" (Rover (0,0) [Game] [("Gamemode",(9,3))])
-- redact "Pickup Game" (Rover (2,10) [Book,Device,Toy,Book] [("Pickup Book",(0,5))])
-- redact "" (Rover (-1,-4) [Book] [("Pickup Book",(2,1)),("DropAll Book",(3,2)),("Pickup Book",(0,5))])


----- 5 -----
-- PURPOSE
-- pickup thing rbt returns the robot state obtained when rbt picks up thing:
   -- • When rbt is a drone, thing becomes the single item held by rbt.
 sa   -- • When rbt is a rover, thing gets added to the collection of items held by rbt.
-- An entry is added to the log to indicate that thing was picked up.

-- DEFINITION
pickup :: Item -> Robot -> Robot
pickup item (Drone a b c) = Drone a (Just item) (("Pickup " ++ show item):c)
pickup item (Rover a b c) = Rover a (item:b) ((("Pickup " ++ show item), a):c)

-- TESTS
-- pickup Book (Rover (1,1) [] [])
-- pickup Game (Rover (0,0) [Game] [("Gamemode",(9,3))])
-- pickup Device (Drone (2,3) (Just Game) ["Pickup iPhone","drop"])
-- pickup Toy (Drone (9,8) (Just Game) ["Pickup Dinosaur","hold"])


----- 6 -----
-- PURPOSE
-- dropAll thing rbt returns the robot state obtained when rbt drops all copies of thing that it may be holding. An entry is added to the robot’s log.

-- DEFINITION
dropAll :: Item -> Robot -> Robot
dropAll item (Drone a b c)
        | (Just item) == b = Drone a Nothing (("DropAll " ++ show item):c)
        | otherwise = Drone a b (("DropAll " ++ show item):c)
dropAll item (Rover a [] c) = Rover a [] ((("DropAll" ++ show item), a):c)
dropAll item (Rover a b c) = Rover a (filter (\x -> x /= item) b) ((("DropAll " ++ show item), a):c)

-- TESTS
-- dropAll Game sampleDrone
-- dropAll Toy sampleRover1
-- dropAll Device (Drone (2,3) (Just Device) ["Pickup iPhone","drop"])
-- dropAll Book (Rover (-1,-4) [Book] [("Pickup Book",(2,1)),("DropAll Book",(3,2)),("Pickup Book",(0,5))])
 

----- 7 -----
-- PURPOSE
-- perform i rbt returns the resulting state when rbt performs the instruction i. Note: This function should simply be a six-equation function that calls your previously defined functions.

-- DEFINITION
perform :: Instr -> Robot -> Robot
perform (Move (a,b)) rbt = move (a,b) rbt
perform (Reset) rbt = reset rbt
perform (Log str) rbt = addEntry str rbt
perform (Redact str) rbt = redact str rbt
perform (Pickup itm) rbt = pickup itm rbt
perform (DropAll itm) rbt = dropAll itm rbt

-- TESTS
-- perform (Pickup Game) (Rover (0,0) [Game] [("Gamemode",(9,3))])
-- perform (DropAll Device) (Rover (83,2) [Book] [("Page",(2,1))])
-- perform (Reset) sampleRover1
-- perform (Move (10,20)) (Drone (5,4) (Just Game) ["Pickup Game","wake"])
-- perform (Log "Pickup Game") (Drone (2,3) (Just Game) ["Pickup iPhone","log"])
-- perform (Redact "Pickup Book") sampleRover2
-- perform (DropAll Book) sampleDrone

