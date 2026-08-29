
-- utilties

clear :: IO()
clear = putStr "\ESC[2J\ESC[H"

info :: IO()
info = putStrLn $ unlines
  [ "n = north"
  , "s = south"
  , "e = east"
  , "w = west"
  , "q = quit"
  ]

-- rooms

template_room = row1 ++ row2 ++ row3 ++ row4 ++ row5

row1 = "█████\n"
row2 = "█   █\n"
row3 = "█   █\n"
row4 = "█   █\n"
row5 = "█████\n\n"

-- really, I should be passing this as a tuple



init_room = snd ( rooms !! 2 )
init_roomName = "ne"

allowedSpaces = [7, 8, 9,
                 13,14,15,
                 19,20,21]

doors = [  2,
         12, 16,
           26]

placeDoors :: String -> [Int] -> String
placeDoors room [] = room
placeDoors room (door:doors) =
  let room' = take door room ++ [' '] ++ drop (door + 1) room
  in placeDoors room' doors

rooms = [("nw", nw), ("nc", nc), ("ne", ne),
         ("cw", cw), ("cc", cc), ("ce", ce),
         ("sw", sw), ("sc", sc), ("se", se)]

nw = placeDoors template_room [16, 26]
nc = placeDoors template_room [12, 16, 26]
ne = placeDoors template_room [12, 26]

cw = placeDoors template_room [2, 16, 26]
cc = placeDoors template_room [2, 12, 16, 26]
ce = placeDoors template_room [2, 12, 26]

sw = placeDoors template_room [2, 16]
sc = placeDoors template_room [2, 12, 16]
se = placeDoors template_room [2, 12]

-- hero init

init_heroPosition = 20

-- items
-- each item needs three things: a room, a location within the room, a name
-- the name can be the same as its screen character
-- allowed spaces for items, for now, are 7, 9, 14, 19, 21

item1 = ("nw", 7, "h")
item2 = ("sw", 21, "s")
item3 = ("cc", 14, "a")

getItemRoom (x, _, _) = x
getItemPosition (_, y, _) = y
getItemCharacter (_, _, z) = z


-- update functions

updateRoom :: Int -> Char -> String -> String
updateRoom index newChar room =


  take index room ++ [newChar] ++ drop (index + 1) room

updateHeroPosition :: Int -> String -> Int
updateHeroPosition index cmd
  | cmd=="n"         = index - 6
  | cmd=="s"         = index + 6
  | cmd=="e"         = index + 1
  | cmd=="w"         = index - 1
  | otherwise        = index

updateHeroDoor :: Int -> Int
updateHeroDoor door
  | door==2          = 20
  | door==12         = 15
  | door==16         = 13
  | door==26         = 8

checkPosition :: Int -> Int -> Int
checkPosition index' index =
  if index' `elem` allowedSpaces
  then index'
  else index

-- Here's where I stopped
-- what I want here is to check to see if there's an active door at index, and if so,
-- redraw map to reflect new room with hero in appropriate position

checkDoors :: Int -> String -> String
checkDoors position room
  | position==2 && room `elem` ["cw", "cc", "ce", "sw", "sc", "se"]   = "Door"
  | position==12 && room `elem` ["nc", "ne", "cc", "ce", "sc", "se"]  = "Door"
  | position==16 && room `elem` ["nw", "nc", "cw", "cc", "sw", "sc"]  = "Door"
  | position==26 && room `elem` ["nw", "nc", "ne", "cw", "cc", "ce"]  = "Door"
  | otherwise                                                         = "No Door"

newRoom :: String -> Int -> String
newRoom oldRoom door
  | oldRoom=="nw" && door==16    = "nc"
  | oldRoom=="nw" && door==26    = "cw"

  | oldRoom=="nc" && door==12    = "nw"
  | oldRoom=="nc" && door==16    = "ne"
  | oldRoom=="nc" && door==26    = "cc"

  | oldRoom=="ne" && door==12    = "nc"
  | oldRoom=="ne" && door==26    = "ce"

  | oldRoom=="cw" && door==2     = "nw"
  | oldRoom=="cw" && door==16    = "cc"
  | oldRoom=="cw" && door==26    = "sw"

  | oldRoom=="cc" && door==2     = "nc"
  | oldRoom=="cc" && door==12    = "cw"
  | oldRoom=="cc" && door==16    = "ce"
  | oldRoom=="cc" && door==26    = "sc"

  | oldRoom=="ce" && door==2     = "ne"
  | oldRoom=="ce" && door==12    = "cc"
  | oldRoom=="ce" && door==26    = "se"

  | oldRoom=="sw" && door==2     = "cw"
  | oldRoom=="sw" && door==16    = "sc"

  | oldRoom=="sc" && door==2     = "cc"
  | oldRoom=="sc" && door==12    = "sw"
  | oldRoom=="sc" && door==16    = "se"

  | oldRoom=="se" && door==2     = "ce"
  | oldRoom=="se" && door==12    = "sc"



-- game loop

runGameLoop :: String -> String -> Int -> IO()
runGameLoop room roomName position  = do
  putStrLn "Command?"
  userInput <- getLine
  if userInput=="q"
  then do putStrLn "Goodbye!"
  else if userInput=="info"
  then do
    info
    runGameLoop room roomName position
  else do
    clear
    let position' = updateHeroPosition position userInput
    let position'' = checkPosition position' position
    let doorCheck = checkDoors position' roomName
    if doorCheck=="Door"
    then do
      let roomName' = newRoom roomName position'
      let room' = maybe "not found" id (lookup roomName' rooms)
      let doorPosition = updateHeroDoor position'
      -- this putStrLn will eventually be where update messages go
      putStrLn ""
      putStrLn ( updateRoom doorPosition '@' room')
      info
      runGameLoop room' roomName' doorPosition
    else do
      putStrLn ""
      putStrLn ( updateRoom position'' '@' room)
      info
      runGameLoop room roomName position''

-- game init

main :: IO()
main = do
  clear
  putStrLn "Welcome, traveller!"
  putStrLn (updateRoom init_heroPosition '@' init_room)
  info
  runGameLoop init_room init_roomName init_heroPosition

