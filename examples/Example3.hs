main = interactionOf initial step event draw

data World = World {
    ttl :: Number,
    board :: [[Bool]]
    }

atSpace :: World -> Number -> Number -> Bool
atSpace w row col = board w !! row !! col

toSpace :: World -> Number -> Number -> (Bool -> Bool) -> World
toSpace w row col f = w { board = go (board w) row col f }
  where go (r:rs) 0 col f = goCol r col f : rs
        go (r:rs) n col f = r : go rs (n-1) col f
        go []     _ _   _ = []

        goCol (c:cs) 0 f  = f c : cs
        goCol (c:cs) n f  = c : goCol cs (n-1) f
        goCol []     _ _  = []

initial rs = World 1 (makeRows 10 rs)
  where makeRows :: Number -> [Number] -> [[Bool]]
        makeRows 0 _      = []
        makeRows n rs     = let (row, rs') = makeCols 10 rs
                            in  row : makeRows (n-1) rs'

        makeCols :: Number -> [Number] -> ([Bool], [Number])
        makeCols 0 rs     = ([], rs)
        makeCols n (r:rs) = let (rest, rs') = makeCols (n-1) rs
                            in  ((r >= 0.5) : rest, rs')

step dt w | dt < ttl w = w { ttl = ttl w - dt }
          | otherwise  = w { ttl = 1, board = next (board w) }

next = id

event (MousePress _ (x,y)) w = let row = floor ((250 - y) `div` 50)
                                   col = floor ((x + 250) `div` 50)
                               in  toSpace w row col not
event _                    w = w

draw w = color (spotColor (ttl w)) (spots (board w))

spotColor t = gray (cos (2*pi*t) / 4)

spots rs = pictures [ translate x y (spot p)
                    | (ps,y) <- zip rs [ 225,  175 ..]
                    , (p, x) <- zip ps [-225, -175 ..] ]

spot True  = solidCircle 25
spot False = circle 25
