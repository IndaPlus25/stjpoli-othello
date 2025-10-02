module Othello
  ( new,
    makeMove,
    getPossibleMoves,
    Game (..),
  )
where

import Data.Bits
import Data.Word

-- You can change this to better represent the states
data GameState = InProgress | GameOver deriving (Eq, Show)

data Color = White | Black deriving (Eq, Show)

data Direction = E | W | N | S | NE | NW | SE | SW deriving (Bounded, Enum, Eq, Show)

-- The files (A-H) are reversed so the bitboard representation makes more sense
data Tile = H1 | G1 | F1 | E1 | D1 | C1 | B1 | A1 | H2 | G2 | F2 | E2 | D2 | C2 | B2 | A2 | H3 | G3 | F3 | E3 | D3 | C3 | B3 | A3 | H4 | G4 | F4 | E4 | D4 | C4 | B4 | A4 | H5 | G5 | F5 | E5 | D5 | C5 | B5 | A5 | H6 | G6 | F6 | E6 | D6 | C6 | B6 | A6 | H7 | G7 | F7 | E7 | D7 | C7 | B7 | A7 | H8 | G8 | F8 | E8 | D8 | C8 | B8 | A8
  deriving (Bounded, Enum, Eq, Show)

data Game = Game
  { state :: GameState,
    black :: Word64,
    white :: Word64,
    turn :: Color
  }

-- Converts a tile to a place on a bitboard
tileToBin :: Tile -> Word64
tileToBin t = 1 .<<. fromEnum t

-- Converts a number to the tile it represents
binToTile :: Word64 -> Tile
binToTile = toEnum . countTrailingZeros

-- | Initializes the board, ...
new :: Game
new =
  Game
    { state = InProgress,
      black = tileToBin E4 .|. tileToBin D5,
      white = tileToBin D4 .|. tileToBin E5,
      turn = Black
    }

switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White

changeTurn :: Game -> Game
changeTurn game = game{turn = switchColor (turn game)}

removeBlack :: Game -> Tile -> Game
removeBlack game tile = game {black = black game .&. complement (tileToBin tile)}

removeWhite :: Game -> Tile -> Game
removeWhite game tile = game {white = white game .&. complement (tileToBin tile)}

addBlack :: Game -> Tile -> Game
addBlack game tile = game {black = black game .|. tileToBin tile}

addWhite :: Game -> Tile -> Game
addWhite game tile = game {white = white game .|. tileToBin tile}

--the following ..Tile return the tile adjacent to the input tile in the stated direction N,S..
-- move North
nTile :: Tile -> Tile
nTile tile = binToTile (shiftL (tileToBin tile) 8)

-- move South
sTile :: Tile -> Tile
sTile tile = binToTile (shiftR (tileToBin tile) 8)

-- move East
eTile :: Tile -> Tile
eTile tile = binToTile (shiftR (tileToBin tile) 1)

-- move West
wTile :: Tile -> Tile
wTile tile = binToTile (shiftL (tileToBin tile) 1)

-- move NorthEast
neTile :: Tile -> Tile
neTile tile = binToTile (shiftL (tileToBin tile) 7)

-- move NorthWest
nwTile :: Tile -> Tile
nwTile tile = binToTile (shiftL (tileToBin tile) 9)

-- move SouthEast
seTile :: Tile -> Tile
seTile tile = binToTile (shiftR (tileToBin tile) 9)

-- move SouthWest
swTile :: Tile -> Tile
swTile tile = binToTile (shiftR (tileToBin tile) 7)

getNeighbours :: Tile -> [Tile]
-- left edge of the board
getNeighbours A1 = [A2, B1, B2]
getNeighbours A2 = [A1, B1, B2, A3, B3]
getNeighbours A3 = [A2, B2, B3, A4, B4]
getNeighbours A4 = [A3, B3, B4, A5, B5]
getNeighbours A5 = [A4, B4, B5, A6, B6]
getNeighbours A6 = [A5, B5, B6, A7, B7]
getNeighbours A7 = [A6, B6, B7, A8, B8]
getNeighbours A8 = [A7, B7, B8]
-- right edge
getNeighbours H1 = [G1, G2, H2]
getNeighbours H2 = [G1, H1, G2, G3, H3]
getNeighbours H3 = [G2, H2, G3, G4, H4]
getNeighbours H4 = [G3, H3, G4, G5, H5]
getNeighbours H5 = [G4, H4, G5, G6, H6]
getNeighbours H6 = [G5, H5, G6, G7, H7]
getNeighbours H7 = [G6, H6, G7, G8, H8]
getNeighbours H8 = [G7, H7, G8]
-- bottom edge (no corners)
getNeighbours B1 = [A1, A2, B2, C1, C2]
getNeighbours C1 = [B1, B2, C2, D1, D2]
getNeighbours D1 = [C1, C2, D2, E1, E2]
getNeighbours E1 = [D1, D2, E2, F1, F2]
getNeighbours F1 = [E1, E2, F2, G1, G2]
getNeighbours G1 = [F1, F2, G2, H1, H2]
-- top edge (no corners)
getNeighbours B8 = [A8, A7, B7, C8, C7]
getNeighbours C8 = [B8, B7, C7, D8, D7]
getNeighbours D8 = [C8, C7, D7, E8, E7]
getNeighbours E8 = [D8, D7, E7, F8, F7]
getNeighbours F8 = [E8, E7, F7, G8, G7]
getNeighbours G8 = [F8, F7, G7, H8, H7]
-- all the others
getNeighbours tile =
  [ nTile  tile -- N
  , sTile  tile -- S
  , eTile  tile -- E
  , wTile  tile -- W
  , neTile tile -- NE
  , nwTile tile -- NW
  , seTile tile -- SE
  , swTile tile -- SW
  ]

possibleNorth :: Word64 -> Tile -> Bool
possibleNorth mask tile
  | tile `elem` [A8, B8, C8, D8, E8, F8, G8, H8] = False
  | otherwise = 
      if tileToBin (nTile tile) .&. mask == tileToBin (nTile tile)
        then possibleNorth mask (nTile tile)
        else True

possibleSouth :: Word64 -> Tile -> Bool
possibleSouth mask tile
  | elem tile [A1, B1, C1, D1, E1, F1, G1, H1] = False
  | otherwise = 
      if tileToBin (sTile tile) .&. mask == tileToBin (sTile tile)
        then possibleSouth mask (sTile tile)
        else True

possibleEast :: Word64 -> Tile -> Bool
possibleEast mask tile
  | elem tile [H1, H2, H3, H4, H5, H6, H7, H8] = False
  | otherwise =
      if tileToBin (eTile tile) .&. mask == tileToBin (eTile tile)
        then possibleEast mask (eTile tile)
        else True

possibleWest :: Word64 -> Tile -> Bool
possibleWest mask tile
  | elem tile [A1, A2, A3, A4, A5, A6, A7, A8] = False
  | otherwise =
      if tileToBin (wTile tile) .&. mask == tileToBin (wTile tile)
        then possibleWest mask (wTile tile)
        else True

possibleNE :: Word64 -> Tile -> Bool
possibleNE mask tile
  | elem tile [A8, B8, C8, D8, E8, F8, G8, H8] || elem tile [H1, H2, H3, H4, H5, H6, H7, H8] = False
  | otherwise =
      if tileToBin (neTile tile) .&. mask == tileToBin (neTile tile)
        then possibleNE mask (neTile tile)
        else True

possibleNW :: Word64 -> Tile -> Bool
possibleNW mask tile
  | elem tile [A8, B8, C8, D8, E8, F8, G8, H8] || elem tile [A1, A2, A3, A4, A5, A6, A7, A8] = False
  | otherwise =
      if tileToBin (nwTile tile) .&. mask == tileToBin (nwTile tile)
        then possibleNW mask (nwTile tile)
        else True

possibleSE :: Word64 -> Tile -> Bool
possibleSE mask tile
  | elem tile [A1, B1, C1, D1, E1, F1, G1, H1] || elem tile [H1, H2, H3, H4, H5, H6, H7, H8] = False
  | otherwise =
      if tileToBin (seTile tile) .&. mask == tileToBin (seTile tile)
        then possibleSE mask (seTile tile)
        else True

possibleSW :: Word64 -> Tile -> Bool
possibleSW mask tile
  | elem tile [A1, B1, C1, D1, E1, F1, G1, H1] || elem tile [A1, A2, A3, A4, A5, A6, A7, A8] = False
  | otherwise =
      if tileToBin (swTile tile) .&. mask == tileToBin (swTile tile)
        then possibleSW mask (swTile tile)
        else True

possibleDirection :: Game -> Tile -> Direction -> Bool
possibleDirection game tile direction =
  if turn game == White
    then
      if direction == N
        then possibleNorth (white game) tile
        else if direction == S
          then possibleSouth (white game) tile
          else if direction == E
            then possibleEast (white game) tile
            else if direction == W
              then possibleWest (white game) tile
              else if direction == NE
                then possibleNE (white game) tile
                else if direction == NW
                  then possibleNW (white game) tile
                  else if direction == SE
                    then possibleSE (white game) tile
                    else if direction == SW
                      then possibleSW (white game) tile
                      else False
    else
      if direction == N
        then possibleNorth (black game) tile
        else if direction == S
          then possibleSouth (black game) tile
          else if direction == E
            then possibleEast (black game) tile
            else if direction == W
              then possibleWest (black game) tile
              else if direction == NE
                then possibleNE (black game) tile
                else if direction == NW
                  then possibleNW (black game) tile
                  else if direction == SE
                    then possibleSE (black game) tile
                    else if direction == SW
                      then possibleSW (black game) tile
                      else False

getHitList :: Game -> Tile -> Direction -> Maybe [Tile]
getHitList game tile direction = Nothing

makeMove :: Game -> Tile -> Game -- contained Maybe monad
makeMove game tile = 
  if turn game == White
    then changeTurn (addWhite game tile)
    else changeTurn (addBlack game tile)

getPossibleMoves :: Game -> Maybe [Tile]
getPossibleMoves game =
  if turn game == White
    then map possibleDirection game 
    else Nothing

instance Show Game where
  -- build board representation string
  show game = ""