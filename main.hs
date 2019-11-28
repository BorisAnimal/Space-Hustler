{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import CodeWorld
import Prelude hiding (Left,Right)
import Data.Text.Internal
import qualified Data.Text as T

---------- media
enemyEmoji :: Text
enemyEmoji = "👾" 

playerEmoji :: Text
playerEmoji = "🚀"

bulletEmoji :: Text
bulletEmoji = "💲"  {- "💰" "☄️" -}

liveEmoji :: Text
liveEmoji = "💰"

---------- params
-- | how much score decrease with single shot
bulletCost :: Double
bulletCost = 100.0
-- | how much cost one live (bag of money) of player - init param of game
liveCost :: Double
liveCost = 1000.0

---------- Types
-- | In future there could be Polyline 
data Border = StraightBorder Double Double Double Double -- xd, yd, xt, yt

-- | main character
data Player = Player  {
                        radius :: Double,
                        speed :: Vector,
                        coords :: Point,
                        limits :: Restrictions,
                        movingDir :: MovingDirection
                      }

-- | Direction where moving performs. Player moves each step
data MovingDirection = MvLeft | MvRight
          
-- | Box of possible coordinates for object (Player, Bullet, Enemy)
-- | Should be the same as Border data (~ using type)
data Restrictions = Restrictions Double Double Double Double


-- | Single warship of enemies
data Enemy = Enemy  {
                      coords :: Point,
                      speed :: Vector,
                      radius :: Double
                    } 

-- | Moving object possible to collide and destroy somebody
data Bullet = Bullet {
                        coords :: Point,
                        speed :: Vector,
                        radius :: Double
                     }
                 
-- | aka world from documentation of codeworld
data State = State { 
                      player :: Player, 
                      border :: Border, 
                      enemies :: [Enemy],
                      playerBullets :: [Bullet],
                      score :: Double,
                      status :: GameStatus
                   }
                   
-- | enum of game states                
data GameStatus = Playing | Win | Lose

-- | mapping to action buttons
data Button = Left | Right | Space



----------  Drawings

-- | we need to multiply coef by 2 due to it is radius
scaledByRad :: Double -> Picture -> Picture
scaledByRad r = scaled (2*r) (2*r) 

drawPlayer  :: Player -> Picture
drawPlayer player = translated x (y + r/2)
  $ scaledByRad r 
  $ rotated (pi/4) -- to make original emoji direct straight up
  $ lettering playerEmoji
  where
    f = coords :: Player -> Point
    (x,y) = f player
    r = (radius :: Player -> Double) player 

-- | 1 dp padding added on left, right and bottom
drawBorder :: Border -> Picture
drawBorder (StraightBorder xd yd xt yt) = 
  translated ((xt - xd) / 2) ((yt - yd) / 2 - dp) 
  $ colored (dark (dark blue))
  $ solidRectangle (xt - xd + 2*dp) (yt - yd + dp)
    where
      dp = 1 -- specify it as param but not argument

-- | Draw all enemies using only provided information. 
-- | No modifications of fields, paddings, sizes, etc. 
drawEnemies :: [Enemy] -> Picture
drawEnemies [] = blank
drawEnemies (e:es) = unit <> drawEnemies es
  where
    (x, y) = (coords :: Enemy -> Point) e
    r = (radius :: Enemy -> Double) e
    unit =  translated x y 
            (circle r <> (scaledByRad r (lettering enemyEmoji)))


drawBullets :: [Bullet] -> Picture
drawBullets [] = blank
drawBullets ((Bullet (x,y) _ r):xs) = drawBullets xs 
  <> translated x y (scaledByRad r (lettering bulletEmoji))

drawScore :: Double -> Picture
drawScore score  = lettering ((T.pack . show) score)

drawLives :: Int -> Picture 
drawLives n 
  | n <= 0    = blank
  | otherwise = lettering liveEmoji <> translated 1 0 (drawLives (n-1))

drawWorld :: State -> Picture
drawWorld (State player border es bs score Playing) = translated dx dy world
  where 
    (StraightBorder _ _ xx yy) = border
    dx = (-0.5) * xx
    dy = (-0.5) * yy
    world = translated 0 yt stats
      <> drawBullets bs
      <> drawPlayer player
      <> drawEnemies es
      <> drawBorder border 
    (StraightBorder _ _ xt yt) = border
    sc = drawScore score
    lv = drawLives (ceiling (score / liveCost))
    stats = translated 2.5 0.5 (sc <> translated (xt / 2) 0 lv)
drawWorld (State _ _ _ _ score Win) = 
  lettering "You win!" <> translated 0 (-3) (drawScore score)
drawWorld (State _ _ _ _ _ Lose) = 
  lettering "You lose."

  
  
  
------------ Interaction by keys
-- | Mapping of keyboard actions to 'Button' actions.
handleKeypress :: Event -> Maybe Button
handleKeypress (KeyPress "Left")   = Just Left
handleKeypress (KeyPress "Right") = Just Right
handleKeypress (KeyPress " ")    = Just Space
handleKeypress _                 = Nothing

-- | Actions from maybe clicked button
handleButton :: Maybe Button -> State -> State
handleButton Nothing state = state
handleButton (Just Space) (State _ _ _ _ _ Win) = initState
handleButton (Just Space) (State _ _ _ _ _ Lose) = initState
handleButton (Just Space) state = newState
  where 
    p = player state
    newBullet = spawnBullet ((coords :: Player -> Point) p) (0, 4.0) 0.3
    newState = state { 
                      playerBullets = newBullet : (playerBullets state),
                      score = (score state) - bulletCost
                     }
handleButton (Just btn) (State player border es bs score Playing) = newState
  where 
    newState = State (handlePlayerDir player btn) border es bs score Playing
handleButton (Just btn) (State player border es bs score _) = initState

-- | Add element to field of State
spawnBullet :: Point  -- where to start
            -> Vector -- movement speed
            -> Double -- radius
            -> Bullet
spawnBullet p vec rad = Bullet p vec rad

-- | Change dir if button pressed
handlePlayerDir :: Player -> Button -> Player
handlePlayerDir player btn = 
  player {movingDir = dir}
  where
    dir = case btn of
      Left -> MvLeft
      Right -> MvRight
      _ -> movingDir player

-- | In case if will back to "clear Rick" controll by buttons
{-
movePlayer :: Player -> Button -> Player
movePlayer player btn = 
  player {coords = applyRestrictions newCoords restr}
  where
    f = coords :: Player -> Point
    s = speed :: Player -> Vector
    restr = limits player
    newCoords = movePoint (f player) (s player) coef
    coef = case btn of
      Left -> -1
      Right -> 1 
      otherwise -> 0
-}
    
------------ | Time dependent interaction

-- | Entry point of dt changes
moveWorld :: Double -> State -> State
moveWorld dt state = checkWorld newState
  where
    movedPlayerBullets = map (moveBullet dt) (playerBullets state) -- TODO: add restrictions, if bullet leave - delete it
    newState = state { 
                        playerBullets = movedPlayerBullets,
                        player = movePlayer dt (player state)
                     }

-- | Update coordinates of player 
movePlayer :: Double -> Player -> Player
movePlayer dt player = player {coords = applyRestrictions newCoords (limits player)}
  where
    xy = (coords :: Player -> Point) player
    dxdy = (speed :: Player -> Vector) player
    -- In case of 2D movement, change it to angle
    coef = case movingDir player of
      MvLeft -> -1 -- to the left
      MvRight -> 1 -- to the right
    newCoords = movePoint xy dxdy (coef*dt)

-- | If point over the border, it will be returned to the border
applyRestrictions :: Point 
                  -> Restrictions
                  -> Point
applyRestrictions (x,y) (Restrictions xmin ymin xmax ymax) = p
  where
    f = \a amin amax -> max amin (min amax a)
    xx = f x xmin xmax :: Double
    yy = f y ymin ymax :: Double
    p = (xx, yy) 

-- | Update coordinates of single bullet
moveBullet  :: Double -- dt
            -> Bullet
            -> Bullet
moveBullet dt (Bullet coords speed rad) = newBullet
  where
    newBullet = Bullet (movePoint coords speed dt) speed rad


movePoint :: Point
          -> Vector
          -> Double -- coef (dt or direction)
          -> Point
movePoint (x, y) vec coef = (xx, yy)
  where
    xx = coef * (fst vec) + x
    yy = coef * (snd vec) + y
    
-- | Check world state, win conditions, filtration
checkWorld :: State -> State
checkWorld state = state  {
                            playerBullets = bs, 
                            enemies = es,
                            status = newStatus
                          }
  where
    (bs, es) = checkBullets (playerBullets state) (enemies state)
    newStatus = checkStatus state
                  
checkStatus :: State -> GameStatus
checkStatus (State _ _ es _ sc st)
  | null es = Win  -- no enemies
  | sc <= 0 = Lose -- no money
  | otherwise = st -- return current state
    
-- | Filter and remove collided (bullet, enemy)
checkBullets :: [Bullet] ->[Enemy] -> ([Bullet], [Enemy])
checkBullets [] es = ([], es)
checkBullets bs [] = (bs, [])
checkBullets (b:bs) es = (curB, fEs)
  where
    --(curB, curEs) =  case (bulletKillEnemy b es) of (Nothing, newEs) -> ([], newEs)
    --                                (Just jB, newEs) -> newB = [jB]
    (newB, newEs) = bulletKillEnemy b es
    curB = case newB of 
      Nothing -> fB
      Just _ -> b : fB
    (fB, fEs) = checkBullets bs newEs

-- | Simulate if single bullet shut one of enemies
bulletKillEnemy :: Bullet -> [Enemy] -> (Maybe Bullet, [Enemy])
bulletKillEnemy b [] = (Just b, [])
bulletKillEnemy (Bullet c1 s1 r1) ((Enemy c2 s2 r2):es) = 
    if intersect c1 r1 c2 r2 
      then (Nothing, es)
      else (futureB, (Enemy c2 s2 r2) : futureEs)
    where
      (futureB, futureEs) =  bulletKillEnemy (Bullet c1 s1 r1) es
  

intersect :: Point -> Double -> Point -> Double -> Bool
intersect (x1, y1) r1 (x2, y2) r2 = 
  sqrt ((x2 - x1)^2 + (y2 - y1)^2) < r1 + r2


-- enemyKillPlayer -- | TODO

------------ Initialization
initState :: State
initState = State player border enemies [] score Playing
  where
    player = Player pSize pSpeed pCoords pLimits MvRight
    pSize = 0.5
    pSpeed = (10.0, 0.0)
    pCoords = (bWidth/2, 0)
    pLimits = Restrictions  0 0 bWidth 0
                        
    border = StraightBorder 0 0 bWidth bHeight
    bHeight = 10
    bWidth = 18
    
    enemies =  lineOfEnemies 6.0 1 7.0 4 0.5
            ++ lineOfEnemies 4.0 1 5.0 8 0.3 
            ++ lineOfEnemies 6.0 1 3.0 4 0.5
    score = 3000.0 
    
-- | Line of enemies. Same size, padding, etc. 
lineOfEnemies :: Double -- left padding
              -> Double -- padding between units
              -> Double -- height
              -> Int    -- units
              -> Double -- radius
              -> [Enemy]              
lineOfEnemies leftPadding padding height units r = army
  where
    xs = take units [leftPadding + r, leftPadding + 3 * r + padding ..]
    army = map (\x -> Enemy (x, height) (0,0) r) xs
    
    
spaceHastler :: IO ()
spaceHastler = interactionOf initState updateWorld handleWorld drawWorld 
  where
    updateWorld = moveWorld
    handleWorld = handleButton . handleKeypress

-- TODO: add out of borders filtering
-- TODO: add moving enemies
-- TODO: make constraints better

main :: IO ()
main = spaceHastler
