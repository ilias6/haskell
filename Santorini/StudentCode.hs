module StudentCode where


-- data types that specify a pawn's place on the board
type RowPos = Int
type ColumnPos = Int
type Position = (RowPos, ColumnPos)

-- positions for players' pawns
type PlayerPositions = (Position, Position)
type BluePlayerPositions = PlayerPositions
type RedPlayerPositions = PlayerPositions

-- types that specify a building
type Height = Int
type Building = (Height, Position)
type BuildingsList = [Building]

type Turn = Char
type Depth = Int

-- unfortunetely these types do not help for the implementation of undo-redo
-- undo works fine but redo can be done only once
type History = [Game]
type Future = [Game]
type Goal = Bool

-- a complete specification of the game's state
-- Goal : Bool (if a player has won)
-- the next 2 are the players' positions
-- History-Futre : [Game] (its more like a representation for a stack, but as said above, only undo works fine and redo only for one step
-- BuildingsList : a list of the buildings
-- Turn : Char ('R' : red, 'B' : blue
data Game = Game Goal BluePlayerPositions RedPlayerPositions History Future BuildingsList Turn

-- given the positions, start with specified pawns on board and Blue plays first
initializeGame :: BluePlayerPositions -> RedPlayerPositions -> Game
initializeGame b r = if (pr1 /= pr2)&&
                        (pr1 /= pb1)&&
                        (pb1 /= pb2)&&
                        (pr1 /= pb2)&&
                        (pr2 /= pb2)&&
                        (pr2 /= pb1)&&
                        (rx1 >= 0 && rx2 >= 0 && bx1 >= 0 && bx2 >= 0)&&
                        (ry1 <= 4 && ry2 <= 4 && by1 <= 4 && by2 <= 4) then game1 else game2
                     where (rx2, ry2) = pr2
                           (rx1, ry1) = pr1
                           (bx2, by2) = pb2
                           (bx1, by1) = pb1
                           (pr1, pr2) = r
                           (pb1, pb2) = b
                           game1 = Game False b r [] [] [] 'B'
                           game2 = Game True b r [] [] [] 'N'


--returns true if the position is empty, otherwise false
is_empty :: Position -> (Position, Position, Position) -> Bool
is_empty pos1 (pos2, pos3, pos4) = if (pos1 /= pos2)&&(pos1 /= pos3)&&(pos1 /= pos4) then True
                                   else False
--returns the level of building on a position, if there is no buildings returns 0
find_level :: Position -> BuildingsList -> Height
find_level _ [] = 0
find_level p1 ((level, p2):rest) = if (p1 == p2) then level
                        else find_level p1 rest

--returns true if a position is active (if the level < 4), otherwise false
is_active :: Position -> BuildingsList -> Bool
is_active p1 list = if (find_level p1 list) == 4 then False
                    else True


--returns true if 2 positions are neighbors, otherwise false
is_neighbor :: Position -> Position -> Bool
is_neighbor (px1, py1) (px2, py2) = if (abs (px1-px2) <= 1)&&
                                       (abs (py1-py2) <= 1) then True else False

--returns true if a move from a start position is legal according to the rules, otherwise false
is_legal :: Position -> (Position, Position, Position) -> Bool
is_legal pp (from, to, build_on) = if (from == pp)&&
                                      (from /= to)&&
                                      (to_x <= 4 && to_y <= 4 && to_x >= 0 && to_y >= 0)&&
                                      (on_x <= 4 && on_y <= 4 && on_x >= 0 && on_y >= 0)&&
                                      (is_neighbor from to)&&
                                      (to /= build_on)&&
                                      (is_neighbor to build_on) then True else False
                                      where (to_x, to_y) = to
                                            (on_x, on_y) = build_on


--returns true if 2 positions have level difference < 2, otherwise false
can_climb :: Position -> Position -> BuildingsList -> Bool
can_climb from to list = if ((find_level to list)-(find_level from list) <= 1) then True
                         else False

--returns true if the move can be done, otherwise false
--The arguments are: Which -> Others -> Move -> Buildings
parse_move :: Position -> (Position, Position, Position) -> (Position, Position, Position) -> BuildingsList -> Bool
parse_move pp others (from, to, on) buildings = if (is_legal pp (from, to, on))&&
                                                   (can_climb from to buildings)&&
                                                   (is_empty to others)&&
                                                   (is_active to buildings)&&
                                                   (is_empty on others)&&
                                                   (is_active on buildings) then True else False

--adds the a new building or update an old
build :: Position -> BuildingsList -> BuildingsList
build pos [] = [(1, pos)]
build pos1 ((level, pos2):other) = if (pos1 == pos2) then (((level+1), pos2):other)
                                   else [(level, pos2)]++(build pos1 other)


--returns true if a player has won, otherwise false
is_goal_state :: Game -> Bool
--the blue player has won
is_goal_state (Game g (b1, b2) (r1, r2) h f buildings 'R') = if ((find_level b1 buildings) == 3)||
                                                                ((find_level b2 buildings) == 3)||
                                                                (possibleMoves (Game g (b1, b2) (r1, r2) h f buildings 'R') == [])
                                                             then True else False
--the red player has won
is_goal_state (Game g (b1, b2) (r1, r2) h f buildings 'B') = if ((find_level r1 buildings) == 3)||
                                                                ((find_level r2 buildings) == 3)||
                                                                (possibleMoves (Game g (b1, b2) (r1, r2) h f buildings 'B') == [])
                                                             then True else False


--do the move and update game
makeMove :: Game -> (Position, Position, Position) -> Game
makeMove (Game goal (bp1, bp2) rpp h f buildings 'B') (from, to, on) = if (from == bp1) then newgame1 else newgame2
                        where newgame1 = (Game False (to, bp2) rpp ([old1]++h) [] (build on buildings) 'R')
                              old1 = (Game goal (bp1, bp2) rpp h (f++[newgame1]) buildings 'B')
                              newgame2 = (Game False (bp1, to) rpp ([old2]++h) [] (build on buildings) 'R')
                              old2 = (Game goal (bp1, bp2) rpp h (f++[newgame2]) buildings 'B')

makeMove (Game goal bpp (rp1, rp2) h f buildings 'R') (from, to, on) = if (from == rp1) then newgame1 else newgame2
                             where newgame1 = (Game False bpp (to, rp2) ([old1]++h) [] (build on buildings) 'B')
                                   old1 = (Game goal bpp (rp1, rp2) h (f++[newgame1]) buildings 'R')
                                   newgame2 = (Game False bpp (rp1, to) ([old2]++h) [] (build on buildings) 'B')
                                   old2 = (Game goal bpp (rp1, rp2) h (f++[newgame2]) buildings 'R')

-- apply specified move (pawn initial position, pawn final position, pawn build position) if and only if it is legal 
tryMove :: Game -> (Position, Position, Position) -> Game
tryMove (Game goal bpp rpp history future buildings turn) move
                                                   | (turn == 'B') && (cond1 || cond2) && (is_goal_state newgame) = blue_winner
                                                   | (turn == 'B') && (cond1 || cond2)  = newgame
                                                   | (turn == 'R') && (cond3 || cond4) && (is_goal_state newgame) = red_winner
                                                   | (turn == 'R') && (cond3 || cond4) = newgame
                                                   | otherwise = game
                                                   where cond1 = parse_move bpp1 (bpp2, rpp1, rpp2) move buildings
                                                         cond2 = parse_move bpp2 (bpp1, rpp1, rpp2) move buildings
                                                         cond3 = parse_move rpp1 (bpp1, bpp2, rpp2) move buildings
                                                         cond4 = parse_move rpp2 (bpp1, bpp2, rpp1) move buildings
                                                         (bpp1, bpp2) = bpp
                                                         (rpp1, rpp2) = rpp
                                                         blue_winner = (Game True b r h f bu 'B')
                                                         red_winner = (Game True b r h f bu 'R')
                                                         (Game g b r h f bu tu) = newgame
                                                         newgame = makeMove game move
                                                         game = (Game goal bpp rpp history future buildings turn)


-- returns (game has ended, player character, blue positions, red positions, list with buildings)
screenshotGame :: Game -> (Bool, Turn, BluePlayerPositions, RedPlayerPositions, BuildingsList)
screenshotGame (Game goal bpp rpp _ _ buildings turn) = (goal, turn, bpp, rpp, buildings)
 

-- rewind last move executed on input game
undoMove :: Game -> Game
undoMove (Game g b r [] f bu tu) = (Game g b r [] f bu tu)
undoMove (Game _ _ _ (last:history) _ _ _) = last

-- redo last move undone on input game  
redoMove :: Game -> Game
redoMove (Game g b r h [] bu tu) = (Game g b r h [] bu tu)
redoMove (Game _ _ _ _ (next:future) _ _) = next 

--returns all neihbor positions to a position
all_neighbors :: Position -> [Position]
all_neighbors (0, 0) = [(0, 1), (1, 1), (1, 0)]
all_neighbors (0, y) = [(1, y), (1, (y+1)), (1, (y-1)), (0, (y-1)), (0, (y+1))]
all_neighbors (y, 0) = [(y, 1), ((y+1), 1), ((y-1), 1), ((y-1), 0), ((y+1), 0)]
all_neighbors (4, 4) = [(4, 3), (3, 3), (3, 4)]
all_neighbors (4, y) = [(3, y), (3, (y+1)), (3, (y-1)), (4, (y-1)), (4, (y+1))]
all_neighbors (y, 4) = [(y, 3), ((y+1), 3), ((y-1), 3), ((y-1), 4), ((y+1), 4)]
all_neighbors (x, y) = [(x, (y-1)), (x, (y+1)), ((x-1), y), ((x+1), y), ((x-1), (y-1)), ((x-1), (y+1)), ((x+1), (y-1)), ((x+1), (y+1))]

--returns a list with tuples: ("from", "to" positions (fixed), neibor position (to "to" position))
make_comb :: Position -> Position -> [Position] -> [(Position, Position, Position)]
make_comb _ _ [] = []
make_comb from to (on:on_list) = [(from, to, on)]++(make_comb from to on_list)

find_buildings :: Position -> [Position] -> [(Position, Position, Position)]
find_buildings _ [] = []
find_buildings from (to:to_list) = (make_comb from to (all_neighbors to))++(find_buildings from to_list)

--returns a list with all possible moves including the illegal ones
all_moves :: Position -> [(Position, Position, Position)]
all_moves from = find_buildings from to_list
                 where to_list = all_neighbors from

--keeps only the legal moves
filter_moves :: Position -> [(Position, Position, Position)] -> (Position, Position, Position) -> BuildingsList -> [(Position, Position, Position)]
filter_moves _ [] _ _ = []
filter_moves from (move:rest) others buildings = if (parse_move from others move buildings) then [move]++(filter_moves from rest others buildings)
                                                else filter_moves from rest others buildings

--makes the list of all legal moves for a start position
possible :: Position -> (Position, Position, Position) -> BuildingsList -> [(Position, Position, Position)]
possible from others buildings = filter_moves from all others buildings
                                where all = all_moves from

-- list all possible legal moves the current player can make in the game
possibleMoves :: Game -> [(Position, Position, Position)]
possibleMoves (Game _ (b1, b2) (r1, r2) _ _ buildings 'B') = (possible b1 (b2, r1, r2) buildings)++(possible b2 (b1, r1, r2) buildings)
possibleMoves (Game _ (b1, b2) (r1, r2) _ _ buildings 'R') = (possible r1 (b1, b2, r2) buildings)++(possible r2 (b1, b2, r2) buildings)


-- estimate winning probability for given player and game
evaluateState :: Turn -> Game -> Int
evaluateState = undefined
