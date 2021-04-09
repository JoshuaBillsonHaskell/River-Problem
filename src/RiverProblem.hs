module RiverProblem
    ( solve
    ) where
      
import Data.Maybe (isJust, fromJust)

-- Encodes The Side Of The River On Which A Participant May Reside
data Side = LeftSide | RightSide deriving (Eq, Show)

-- Representation Of The Current State Of The World
data Node = Node { nodeHen :: Side
                 , nodeFox :: Side
                 , nodeGrain :: Side
                 , nodeFarmer :: Side
                 } deriving (Eq)

instance Show Node where
    show (Node hen fox grain farmer) =
        "{Hen: " ++ show hen ++ ", Fox: " ++ show fox ++ ", Grain: " ++
        show grain ++ ", Farmer: " ++ show farmer ++ "}"

-- Initial State Where All Participants Are On The Left Side Of The River
initialState :: Node
initialState = Node LeftSide LeftSide LeftSide LeftSide

-- Given A Side Of The River, Returns The Opposite Side
switchSides :: Side -> Side
switchSides LeftSide = RightSide
switchSides RightSide = LeftSide

-- Move The Farmer To The Other Side Of The River
moveFarmer :: Node -> Node
moveFarmer node@(Node _ _ _ farmer) = node {nodeFarmer = switchSides farmer}

-- Move Hen & Farmer To The Other Side Of The River
moveHen :: Node -> Maybe Node
moveHen node@(Node hen _ _ farmer) 
  | hen == farmer = Just (moveFarmer node) {nodeHen = switchSides hen}
  | otherwise = Nothing
  
-- Move Fox & Farmer To The Other Side Of The River
moveFox :: Node -> Maybe Node
moveFox node@(Node _ fox _ farmer) 
  | fox == farmer = Just (moveFarmer node) {nodeFox = switchSides fox}
  | otherwise = Nothing
  
-- Move Grain & Farmer To The Other Side Of The River
moveGrain :: Node -> Maybe Node
moveGrain node@(Node _ _ grain farmer) 
  | grain == farmer = Just (moveFarmer node) {nodeGrain = switchSides grain}
  | otherwise = Nothing

-- Returns True If The Goal Is Reached (All Participants Are On The Right Side), False Otherwise
isGoal :: Node -> Bool
isGoal (Node hn fx grn _) = (hn==RightSide) && (fx==RightSide) && (grn==RightSide)

-- Given A State, Returns An Array Of All Neighbouring States
neighbors :: Node -> [Node]
neighbors node = filter legalState [fromJust x | x <- newStates, isJust x]
    where legalState (Node hn fx grn frmr) = not $ ((fx == hn) && (fx /= frmr)) || ((hn == grn) && (hn /= frmr))
          newStates = [Just (moveFarmer node), moveHen node, moveFox node, moveGrain node]

-- Perform A Breadth-First-Search On A Given Frontier
breadthFirstSearch :: [[Node]] -> [Node]
breadthFirstSearch [] = []
breadthFirstSearch (path:frontier)
    | isGoal . last $ path = path
    | otherwise = breadthFirstSearch $ frontier ++ map (\x -> path ++ [x]) neighbors'
    where neighbors' = neighbors $ last path

-- Convenience Function For Conducting Breadth First Search Starting At A Given Node
search :: Node -> [Node]
search start = breadthFirstSearch [[start]]

-- Given A Path (Array Of Nodes), Print The Actions And Intermediate States From The First To The Last Node
showPath :: [Node] -> IO ()
showPath [] = putStrLn "No Solution Could Be Found!"
showPath path = mapM_ putStrLn qualifiedActions
    where arcs = zip path $ tail path
          qualifiedActions = zipWith (\a s -> a ++ " " ++ s) (["--START-->"] ++ map action arcs) (map show path)

-- Given A Pair Of Nodes, Returns A String Encoding The Action Taken To Get From The First Node To The Second
action :: (Node, Node) -> String
action (Node oldHen oldFox oldGrain _, Node newHen newFox newGrain _)
  | oldHen /= newHen = "--MOVE-HEN-->"
  | oldFox /= newFox = "--MOVE-FOX-->"
  | oldGrain /= newGrain = "--MOVE-GRAIN-->"
  | otherwise = "--MOVE-FARMER-->"

-- Solve The River Problem And Print The Results
solve :: IO ()
solve = showPath $ search initialState