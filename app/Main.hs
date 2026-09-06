
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
init_room = rooms !! 2

-- items
-- each item needs four things: a room, a location within the room, a character, a name
-- allowed spaces for items, for now, are 7, 9, 14, 19, 21

item1 = ("nw", 7, 'h', "the Magic Helmet")
item2 = ("sw", 21, 's', "the Sword")
item3 = ("cc", 14, 'a', "the Armor")

getItemRoom (w, _, _, _) = w
getItemPosition (_, x, _, _) = x
getItemChar (_, _, y, _) = y
getItemName (_, _, _, z) = z

itemRoomCheck :: String -> (String, Int, Char, String)
itemRoomCheck room
  | room==getItemRoom item1   = item1
  | room==getItemRoom item2   = item2
  | room==getItemRoom item3   = item3
  | otherwise                 = ("NO_ITEM", 0, ' ', "")

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

checkDoors :: Int -> String -> String
checkDoors position room
  | position==2 && room `elem` ["cw", "cc", "ce", "sw", "sc", "se"]   = "Door"
  | position==12 && room `elem` ["nc", "ne", "cc", "ce", "sc", "se"]  = "Door"
  | position==16 && room `elem` ["nw", "nc", "cw", "cc", "sw", "sc"]  = "Door"
  | position==26 && room `elem` ["nw", "nc", "ne", "cw", "cc", "ce"]  = "Door"
  | otherwise                                                         = "No Door"

newRoom :: String -> Int -> ( String, String )
newRoom oldRoom door
  | oldRoom=="nw" && door==16    = ("nc", nc)
  | oldRoom=="nw" && door==26    = ("cw", cw)

  | oldRoom=="nc" && door==12    = ("nw", nw)
  | oldRoom=="nc" && door==16    = ("ne", ne)
  | oldRoom=="nc" && door==26    = ("cc", cc)

  | oldRoom=="ne" && door==12    = ("nc", nc)
  | oldRoom=="ne" && door==26    = ("ce", ce)

  | oldRoom=="cw" && door==2     = ("nw", nw)
  | oldRoom=="cw" && door==16    = ("cc", cc)
  | oldRoom=="cw" && door==26    = ("sw", sw)

  | oldRoom=="cc" && door==2     = ("nc", nc)
  | oldRoom=="cc" && door==12    = ("cw", cw)
  | oldRoom=="cc" && door==16    = ("ce", ce)
  | oldRoom=="cc" && door==26    = ("sc", sc)

  | oldRoom=="ce" && door==2     = ("ne", ne)
  | oldRoom=="ce" && door==12    = ("cc", cc)
  | oldRoom=="ce" && door==26    = ("se", se)

  | oldRoom=="sw" && door==2     = ("cw", cw)
  | oldRoom=="sw" && door==16    = ("sc", sc)

  | oldRoom=="sc" && door==2     = ("cc", cc)
  | oldRoom=="sc" && door==12    = ("sw", sw)
  | oldRoom=="sc" && door==16    = ("se", se)

  | oldRoom=="se" && door==2     = ("ce", ce)
  | oldRoom=="se" && door==12    = ("sc", sc)



-- game loop

runGameLoop :: ( String, String ) -> Int -> IO()
runGameLoop room position item dragon = do
  putStrLn "Command?"
  userInput <- getLine
  if userInput=="q"
  then do putStrLn "Goodbye!"
  else if userInput=="info"
  then do
    info
    runGameLoop room position item dragon
  else do
    clear
    let position' = updateHeroPosition position userInput
    let position'' = checkPosition position' position
    let doorCheck = checkDoors position' (fst room)
    if doorCheck=="Door"
    then do
      let room' = newRoom (fst room) position'
      let doorPosition = updateHeroDoor position'
      -- this putStrLn will eventually be where update messages go
      putStrLn ""
      let heroInRoom = updateRoom doorPosition '@' (snd room')
      let item = itemRoomCheck (fst room')
      if getItemRoom item /= "NO_ITEM"
      then do
        putStrLn ( updateRoom (getItemPosition item) (getItemChar item) heroInRoom)
      else putStrLn heroInRoom
      -- putStrLn ( updateRoom doorPosition '@' (snd room'))
      info
      runGameLoop room' doorPosition item dragon
    else do
      putStrLn ""
      putStrLn ( updateRoom position'' '@' (snd room))
      info
      runGameLoop room position'' item dragon

-- game init

main :: IO()
main = do
  clear
  putStrLn "Welcome, traveller!"
  putStrLn (updateRoom init_heroPosition '@' ( snd init_room ) )
  info
  runGameLoop init_room init_heroPosition "NO_ITEM" "NO_DRAGON"

