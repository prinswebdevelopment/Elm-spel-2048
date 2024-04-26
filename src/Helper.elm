module Helper exposing
  ( gameOutput
  , split
  , splitCol
  , getCol
  , getRow
  , moveLeft
  , moveRight
  , moveDown
  , moveUp
  , combine
  , clearZeros
  , grow
  )

-- zet list om naar string
gameOutput : List Int -> String
gameOutput g = 
    case g of
        [] -> ""
        x :: xs -> String.fromInt x ++ gameOutput xs

-- zet list om naar 2 dimensies (groepjes van 4)
split : List Int -> List (List Int)
split list =
  case list of
    [] -> []
    a -> List.take 4 list :: split (List.drop 4 list)

-- zet list om naar 2 dimensies (transpose, groepjes van 4)
splitCol : List Int -> List (List Int)
splitCol list = [getCol 0 list, getCol 1 list, getCol 2 list, getCol 3 list]

-- haalt een kolom uit een 1 dimensionale list (waarbij verondersteld dat deze een 2d dataset is)
getCol : Int -> List Int -> List Int
getCol no list =
  case list of
    i1 :: i2 :: i3 :: i4 :: l -> List.take 1 (List.drop no [i1, i2, i3, i4]) ++ (getCol no l)
    other -> []

-- haalt een rij uit een 1 dimensionale list (waarbij verondersteld dat deze een 2d dataset is)
getRow : List Int -> Int -> List Int
getRow list no =
  List.take 4 (List.drop (no * 4) list)

-- transpose 1d list (uitgaande dat deze 2d representeerd)
transpose : List Int -> List Int
transpose list = getCol 0 list ++ getCol 1 list ++ getCol 2 list ++ getCol 3 list 

-- 2048 helper enkele rij (of kolom) nullen verplaatsen en optellen naar links
moveLeftHelper : List Int -> List Int
moveLeftHelper list = 
  clearZeros list
  |> combine
  |> grow 1

-- 2048 helper enkele rij (of kolom) nullen verplaatsen en optellen naar rechts
moveRightHelper : List Int -> List Int
moveRightHelper list = 
  clearZeros list
  |> List.reverse
  |> combine
  |> List.reverse
  |> grow 2

-- 2048 left event
moveLeft : List Int -> List Int
moveLeft list = 
  let 
    newList = List.map moveLeftHelper (split list)
  in
    case newList of
      x1 :: x2 :: x3 :: x4 :: xs -> x1 ++ x2 ++ x3 ++ x4
      other -> []

-- 2048 right event
moveRight : List Int -> List Int
moveRight list = 
  let 
    newList = List.map moveRightHelper (split list)
  in
    case newList of
      x1 :: x2 :: x3 :: x4 :: xs -> x1 ++ x2 ++ x3 ++ x4
      other -> []

-- 2048 down event
moveDown : List Int -> List Int
moveDown list = 
  let 
    newList = List.map moveRightHelper (splitCol list)
  in
    case newList of
      x1 :: x2 :: x3 :: x4 :: xs -> 
        x1 ++ x2 ++ x3 ++ x4
        |> transpose
      other -> []

-- 2048 up event
moveUp : List Int -> List Int
moveUp list = 
  let 
    newList = List.map moveLeftHelper (splitCol list)
  in
    case newList of
      x1 :: x2 :: x3 :: x4 :: xs -> 
        x1 ++ x2 ++ x3 ++ x4
        |> transpose
      other -> []

-- wis nullen in lijst
clearZeros : List Int -> List Int
clearZeros list = 
  case list of
    [] -> []
    x1 :: xs -> 
      if x1 == 0 then 
        clearZeros xs
      else 
        [x1] ++ clearZeros xs

-- tel velden op indien gelijk en naast elkaar
combine : List Int -> List Int
combine list = 
  case list of
    [] -> []
    [x1] -> [x1]
    x1 :: x2 :: xs -> 
      if x1 == x2 then 
        [x1 + x2] ++ combine xs
      else 
        [x1] ++ combine ([x2] ++ xs)

-- laat een lijst weer tot 4 groeien door 0-en toe te voegen
grow : Int -> List Int -> List Int
grow pos list = 
  if pos == 1 then
    list ++ List.repeat (4 - List.length list) 0
  else
    List.repeat (4 - List.length list) 0 ++ list