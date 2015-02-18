import Time (fps,Time)
import Signal
import Graphics.Element (..)
import Graphics.Collage (..)
import Color (red,green,yellow,black,blue,orange,purple,grey,white,lightRed)
import List
import Keyboard
import Debug
import Random

hitDistance = 10.0
playerHealth = 10
playerSpeed = 0.5
screenWidth = 1200
screenHeight = 800
scrollSpeed = 0.1

type alias Vector = (Float,Float)

type alias Input = {time:Time,dirs:Vector,press:Bool}

{-
type alias AgentRec =
  { update:State->Agent
  , action:State->State
  , draw:Form
  , collide:Int->Agent
  , checkCollision:Agent->Bool
  , position:Vector
  , health:Int
  }
-}

type Agent = Alive { update:State->Agent
                   , action:State->State
                   , draw:Form
                   , collide:Int->Agent
                   , checkCollision:Agent->Bool
                   , position:Vector
                   , health:Int
                   }
           | Dead

type alias Weapon =
  { coolDown: Float
  , wait: Float
  , action: Vector->Float->State->State
  }

type alias PlayerWeapon =
  { coolDown : Time
  , wait : Time
  , action: Vector->Float->State->State
  }

coolDown : Time -> Weapon -> Weapon
coolDown deltas weapon =
  if   weapon.wait > 0
  then {weapon | wait <- weapon.wait - deltas}
  else {weapon | wait <- 0}

fireIfReady : Weapon -> Vector -> Float -> State -> State
fireIfReady weapon pos ang state =
  if weapon.wait <= 0
  then weapon.action pos ang state
  else state

overHeat : Weapon -> Weapon
overHeat weapon =
  {weapon | wait <- weapon.coolDown}

updateWeapon : Time -> Weapon -> Weapon
updateWeapon time weapon =
  if weapon.wait <= 0
  then overHeat weapon
  else coolDown time weapon

type alias PlayerWeapons =
  { bullets  : Weapon
  , rockets  : Weapon
  , missiles : Weapon
  , lasers   : Weapon
  , mines    : Weapon
  }

playerWeapons =
  {
    bullets =
      {
        coolDown = 300,
        wait = 0,
        action pos angle state = {state | goodBullets <- (goodBullet pos (vectorPolar 0.6 angle))::state.goodBullets}
      },
    rockets = 
      {
        coolDown = 1000,
        wait = 0,
        action pos angle state = {state | goodBullets <- (rocket pos (vectorPolar 0.5 angle))::state.goodBullets}
      },
    missiles =
      {
        coolDown = 700,
        wait = 0,
        action pos angle state = {state | goodBullets <- (missile 0.4 pos angle)::state.goodBullets}
      },
    lasers =
      {
        coolDown = 2050,
        wait = 0,
        action pos angle state = {state | goodBullets <- (goodLaser 500 pos)::state.goodBullets}
      },
    mines =
      {
        coolDown = 3000,
        wait = 0,
        action pos angle state = {state | goodBullets <- (mine 150 pos (0,-0.3))::state.goodBullets}
      }
  }

coolDownPlayerWeapons: Time->PlayerWeapons->PlayerWeapons
coolDownPlayerWeapons deltas weapons =
  {playerWeapons | bullets <- coolDown deltas weapons.bullets
                 , rockets <- coolDown deltas weapons.rockets
                 , missiles <- coolDown deltas weapons.missiles
                 , lasers <- coolDown deltas weapons.lasers
                 , mines <- coolDown deltas weapons.mines
                 }

firePlayerWeapons : PlayerWeapons -> Vector -> Float -> State -> State
firePlayerWeapons playerWeapons position angle state =
  fireIfReady playerWeapons.bullets position angle state
  |> fireIfReady playerWeapons.rockets position angle
  |> fireIfReady playerWeapons.missiles position angle
  |> fireIfReady playerWeapons.lasers position angle
  |> fireIfReady playerWeapons.mines position angle

updatePlayerWeapons : Time -> PlayerWeapons -> PlayerWeapons
updatePlayerWeapons deltas playerWeapons =
  {playerWeapons | bullets <- updateWeapon deltas playerWeapons.bullets
                 , rockets <- updateWeapon deltas playerWeapons.rockets
                 , missiles <- updateWeapon deltas playerWeapons.missiles
                 , lasers <- updateWeapon deltas playerWeapons.lasers
                 , mines <- updateWeapon deltas playerWeapons.mines
                 }

type alias State = 
  { player:Agent
  , goodBullets:List Agent
  , badAgents:List Agent
  , badBullets:List Agent
  , playerMove:Vector
  , playerFire:Bool
  , deltas:Time
  , events:List (Time,Agent)
  , age:Time
  , background:Form
  }

{- functions for vectors -}

vectorAdd : Vector -> Vector -> Vector
vectorAdd (x1,y1) (x2,y2) =
  (x1 + x2, y1 + y2)

vectorDiff : Vector -> Vector -> Vector
vectorDiff (x1,y1) (x2,y2) =
  (x1-x2,y1-y2)

vectorPolar : Float -> Float -> Vector
vectorPolar mag angle =
  (mag * (cos angle)
  ,mag * (sin angle)
  )

angle : Vector -> Vector -> Float
angle v1 v2 =
  let diff = ((vectorAngle v1) - (vectorAngle v2))
  in
    if | diff > 2*pi -> diff - 2*pi
       | diff < 0 -> diff + 2*pi
       | otherwise -> diff

vectorAddPolar : Vector -> Float -> Float -> Vector
vectorAddPolar vect mag angle =
  vectorAdd vect (vectorPolar mag angle)


vectorDist : Vector -> Vector -> Float
vectorDist (x1,y1) (x2,y2) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2)

vectorMag : Vector->Float
vectorMag (x,y) =
  sqrt (x^2 + y^2)

vectorScale : Float -> Vector -> Vector
vectorScale scalar (x,y) =
  (scalar * x, scalar * y)

vectorAngle : Vector -> Float
vectorAngle (x,y) =
  atan2 y x

rotateToVector : Vector -> Form -> Form
rotateToVector vector form =
  rotate (vectorAngle vector) form

{- functions used for the Agent type -}

applyCollision : Int->Int->(Int->Agent)->Agent->Agent
applyCollision health damage survives dies =
  let newHealth = health - damage
  in
    if newHealth <= 0
    then dies
    else survives newHealth

nullDraw : Form
nullDraw = filled black (circle 0)

nullAction : State -> State
nullAction state = state

setDraw : Form -> Agent -> Agent
setDraw form agent =
  case agent of
    Alive a -> Alive {a | draw <- form}
    Dead -> Dead

position : Agent->Vector
position agent =
  case agent of
    Alive a -> a.position
    Dead -> (0.0,-10000)

update : State -> Agent -> Agent
update state agent = case agent of
  Alive a -> a.update state
  Dead -> Dead

draw : Agent -> Form
draw agent = case agent of
               Alive a -> a.draw
               Dead -> nullDraw

action : Agent -> State -> State
action agent state = case agent of
  Alive a -> a.action state
  Dead -> state

addAction : (State->State)->Agent->Agent
addAction newAction agent =
  case agent of
    Alive a -> Alive {a | action <- newAction << a.action}
    Dead    -> Dead

addUpdate : (Agent->State->Agent)->Agent->Agent
addUpdate newUpdate agent =
  case agent of
    Alive a -> Alive {a | update <- \state -> (newUpdate (a.update state)) state}
    Dead    -> Dead

{- different shapes for entities -}
playerShip : Form
playerShip = polygon [(-20.0,-20.0),(-15.0,15.0),(0.0,30.0),(15.0,15.0),(20.0,-20.0)]
             |> filled blue

triangleShip = polygon [(-20,-10),(0,10),(20,-10)]
               |> filled yellow


{- the updates that are used -}
updateOnPress : (Agent->State->Agent)->Agent->State->Agent
updateOnPress update agent state =
  if state.playerFire
  then update agent state
  else agent

killIfOffScreen : Vector->Agent->State->Agent
killIfOffScreen (x,y) agent state =
  case agent of
    Alive a ->
      if abs(x)-300 > (toFloat screenWidth) / 2 || abs(y)-300  > (toFloat screenHeight) / 2
      then Dead
      else Alive a
    Dead -> Dead

{- the actions used -}

actionOnPress : (State->State)->State->State
actionOnPress act state =
  if state.playerFire
  then act state
  else state


shootBulletFanHelper : (Vector->Agent)->Float->Float->Float->Int ->(List Agent)->(List Agent)
shootBulletFanHelper makeBullet speed angle interval n agents =
  if n > 0
  then shootBulletFanHelper
          makeBullet
          speed 
          (angle+interval)
          interval
          (n-1)
          ((makeBullet (vectorPolar speed angle))::agents)
  else agents


shootBulletFan : (Vector->Vector->Agent)->Vector->Float->Float->Float->Int->State->State
shootBulletFan makeBullet position speed initAngle interval num state =
  let
    result = shootBulletFanHelper (makeBullet position) speed initAngle interval num state.badBullets
  in
    {state | badBullets <- result}

shootGoodBulletFan : (Vector->Vector->Agent)->Vector->Float->Float->Float->Int->State->State
shootGoodBulletFan makeBullet position speed initAngle interval num state =
  let
    result = shootBulletFanHelper (makeBullet position) speed initAngle interval num state.goodBullets
  in
    {state | goodBullets <- result}



{- collision functions -}

within : Vector -> Float -> Agent -> Bool
within pos dist agent =
  case agent of
    Alive a -> (vectorDist pos a.position) < dist
    Dead -> False

{- all of the agents -}
seeker : (Vector->Float->Agent)->Vector->Float->State->Agent
seeker makeAgent pos speed state =
  let
    playerAngle  = vectorAngle (vectorDiff (position state.player) pos)
    velocity     = (vectorPolar speed playerAngle)
    displacement = (vectorScale state.deltas velocity)
    nextPosition = vectorAdd pos displacement
  in
    makeAgent nextPosition playerAngle

closestEnemyFold : Vector->Agent->(Float,Agent)->(Float,Agent)
closestEnemyFold pos nextAgent (dist,agent) =
  let
    nextDist = vectorDist pos (position nextAgent)
  in
    if nextDist < dist
    then (nextDist,nextAgent)
    else (dist,agent)

closestEnemyPosition : Vector -> State -> Vector
closestEnemyPosition vec state =
  if List.isEmpty state.badAgents
  then (0,10000)
  else
    let
      first = List.head state.badAgents
      firstDist = vectorDist (position first) vec
      rest  = List.tail state.badAgents
      (dist,agent) = List.foldl (closestEnemyFold vec) (firstDist,first) rest
    in
      position agent

goodSeeker : (Vector->Float->Agent)->Vector->Float->State->Agent
goodSeeker makeAgent pos speed state =
  let
    closestEnemy = closestEnemyPosition pos state
    angle        = vectorAngle (vectorDiff closestEnemy pos)
    vel          = (vectorPolar speed angle)
    disp         = (vectorScale state.deltas vel)
    nextPos      = vectorAdd pos disp
  in
    makeAgent nextPos angle

goodLaser : Time->Vector -> Agent
goodLaser alive source =
  let
    nextAlive state = alive - state.deltas
    nextSource state = vectorAdd (vectorScale state.deltas state.playerMove) source
    update state = if (nextAlive state) > 0 then goodLaser (nextAlive state) (nextSource state)
                   else Dead
  in
    Alive
    { draw   = move source (filled purple (polygon [(-1,0),(-1,10000),(1,10000),(1,0)]))
    , position = source
    , health = 1
    , action state = state
    , update state = update state
    , collide dam = goodLaser alive source
    , checkCollision agent = let (x,y) = (position agent)
                                 (sx,sy) = source
                             in
                                 (y > sy) && abs(x-sx) <= 30
    }

missile : Float->Vector->Float->Agent
missile speed pos ang = Alive
  { draw   = rotate (ang-(degrees 90)) (move pos (filled blue (polygon [(-5,-10),(-5,5),(0,10),(5,5),(5,-10)])))
  , health = 2
  , action state = state
  , update state = goodSeeker (missile speed) pos speed state
  , collide dam = Dead
  , position = pos
  , checkCollision agent = False
  }

mine : Time->Vector->Vector->Agent
mine time pos vel =
  Alive { draw  = circle 10
                  |> filled green
                  |> move pos
        , health = 0
        , action state = state
        , update state = if time <= 0
                         then moveStrait (mine (time-state.deltas)) pos (0,-scrollSpeed) state.deltas
                         else moveStrait (mine (time-state.deltas)) pos vel state.deltas
        , position = pos
        , checkCollision = within pos 60
        , collide dam = goodBulletExplosion pos
        }
        

stabber : Int->Vector->Float->Agent
stabber health pos ang = Alive
  { draw   = rotate (ang-(degrees 90)) (move pos (filled orange (polygon [(-20,-20),(-5,20),(0,12),(5,20),(20,-20)])))
  , health = health
  , action state = state
  , update state = seeker (stabber health) pos 0.4 state
  , collide dam = applyCollision health dam (\h -> stabber h pos ang) (explosion pos 0 50)
  , position = pos
  , checkCollision = within pos 20
  }

bulletChaser : Vector->Agent
bulletChaser pos = chaser (basicGun badBullet 250 0.6) 6 pos 0

chaser : Weapon->Int->Vector->Float->Agent
chaser weapon health pos angle =
  let dist state = vectorDist pos (position state.player)
  in
    Alive
    { draw   = rotate (angle-(degrees 90)) (move pos (filled red (polygon [(-20,-20),(-5,20),(0,12),(5,20),(20,-20)])))
    , health = health
    , action state = fireIfReady weapon pos angle state
    , update state = seeker (chaser (updateWeapon state.deltas weapon) health) pos (0.4*((dist state)-100)/100) state
    , collide dam = applyCollision health dam (\h -> chaser weapon h pos angle) (explosion pos 0 50)
    , position = pos
    , checkCollision = within pos 20
    }

moveStrait : (Vector->Vector->Agent)->Vector->Vector->Time->Agent
moveStrait makeAgent position velocity time =
  let
    displacement = (vectorScale time velocity)
    nextPosition = (vectorAdd position displacement)
  in
  makeAgent nextPosition velocity

moveTurning : Float->(Vector->Vector->Agent)->Vector->Vector->Time->Agent
moveTurning rot makeAgent pos vel deltas =
  let
    oldAngle = vectorAngle vel
    newAngle = oldAngle+deltas*rot
    newVel = vectorPolar (vectorMag vel) newAngle
    disp = vectorScale deltas newVel
    nextPos = vectorAdd pos disp
  in
    makeAgent nextPos newVel

orbiter : Float->(Vector->Vector->Agent)-> Time -> Vector -> Vector -> Agent
orbiter goalRadius makeAgent deltas pos vel =
  let
    cenvec = vectorDiff pos (0,0)
    ang = angle pos vel
    radius = vectorMag pos
    speed = (vectorMag vel)*deltas
    strait = moveStrait makeAgent pos vel deltas
    radvel = speed*radius
  in
      if radius < goalRadius
      then strait
      else
        if | ang < pi/2   -> moveTurning radvel makeAgent pos vel deltas
           | ang > 3*pi/2 -> moveTurning radvel makeAgent pos vel deltas
           | otherwise    -> strait


stabberCarrier : Float -> Vector -> Vector->Agent
stabberCarrier rad pos vel =
  let makeStabber p v = stabber 2 p (vectorAngle v)
  in
    carrier rad (basicSpawn makeStabber 750 1) 75 pos vel
  
bulletChaserCarrier : Float -> Vector -> Vector -> Agent
bulletChaserCarrier rad pos vel =
  let makeChaser p v = bulletChaser p
  in
    carrier rad (basicSpawn makeChaser 2000 1) 75 pos vel

carrier : Float->Weapon->Int->Vector->Vector->Agent
carrier radius spawn health pos vel =
  Alive { update state = orbiter radius (carrier radius (updateWeapon state.deltas spawn) health) state.deltas pos vel
        , draw = polygon [(-100,75),(-75,100),(75,100),(100,75),(120,50),(120,-50),(100,-75),(75,-100),(-75,-100),(-100,-75)]
                 |> filled grey
                 |> move pos
                 |> rotate (vectorAngle vel)
        , action state = fireIfReady spawn pos (vectorAngle vel) state
        , checkCollision = within pos 120
        , collide dam = applyCollision health dam (\h -> carrier radius spawn h pos vel) (explosion pos 0 150)
        , position = pos
        , health = health
        }


bulletExplosion : Vector->Agent
bulletExplosion pos =
  Alive { update state = explosion pos 0 100
        , draw = nullDraw
        , action state = shootBulletFan badBullet pos 0.5 0 (degrees 30) 12 state
        , health = 0
        , position = pos
        , collide dam = bulletExplosion pos
        , checkCollision agent = False
        }

goodBulletExplosion : Vector->Agent
goodBulletExplosion pos =
  Alive { update state = explosion pos 0 100
        , draw = nullDraw
        , action state = shootGoodBulletFan goodBullet pos 0.5 0 (degrees 60) 6 state
        , health = 0
        , position = pos
        , collide dam = goodBulletExplosion pos
        , checkCollision agent = False
        }

exploder : Float->Time->Int->Vector->Vector->Agent
exploder rot time health position velocity =
  let
    makeAgent state pos vel = 
      let newTime = time - state.deltas
      in
        if newTime <= 0
        then bulletExplosion position
        else exploder rot newTime health pos vel
  in
    Alive { update state = moveTurning rot (makeAgent state) position velocity state.deltas
          , draw = filled red (circle 20)
                   |> move position
          , health = health
          , collide dam = applyCollision health dam (\h -> exploder rot time h position velocity) (bulletExplosion position)
          , position = position
          , checkCollision = within position 25
          , action state = state
          }

basicBullet : Form->Vector->Vector->Agent
basicBullet form pos velocity =
  let
    newForm = rotate (vectorAngle velocity) (move pos form)
    update state = moveStrait (basicBullet form) pos velocity state.deltas
  in
    Alive { position = pos
          , draw   = newForm
          , health = 1
          , collide dam = explosion pos 0 10.0
          , update state = update state
          , action state = state
          , checkCollision = within pos 5
          }
          |> addUpdate (killIfOffScreen pos)

goodBullet : Vector->Vector->Agent
goodBullet pos vel = basicBullet (filled green (circle 5)) pos vel

badBullet : Vector->Vector->Agent
badBullet pos vel = basicBullet (filled lightRed (circle 5)) pos vel

basicGun: (Vector->Vector->Agent) -> Float -> Float -> Weapon
basicGun makeBullet coolDown speed =
  { coolDown = coolDown
  , wait = 0
  , action pos angle state =
      {state | badBullets <- (makeBullet pos (vectorPolar speed angle))::state.badBullets}
  }

basicSpawn: (Vector->Vector->Agent) -> Float -> Float -> Weapon
basicSpawn makeAgent coolDown speed =
  { coolDown = coolDown
  , wait = 0
  , action pos angle state =
      {state | badAgents <- (makeAgent pos (vectorPolar speed angle))::state.badAgents}
  }

lineFlyer: Form->Weapon->Int->Vector->Vector->Agent
lineFlyer form gun health position velocity =
    let
      makeFlyer state pos vel = lineFlyer form (updateWeapon state.deltas gun) health pos vel
      update state = moveStrait (makeFlyer state) position velocity state.deltas
      collide dam = applyCollision health dam (\new -> lineFlyer form gun new position velocity) (explosion position 0 30)
    in
      Alive { draw = rotate ((vectorAngle velocity)-(degrees 90)) (move position triangleShip)
            , position = position
            , update state = update state
            , action state = fireIfReady gun position (vectorAngle velocity) state
            , collide dam = collide dam
            , health = health
            , checkCollision = within position 25
            }
      |> addUpdate (killIfOffScreen position)
 
rocket : Vector->Vector->Agent
rocket pos vel = Alive
  { position = pos
  , draw = polygon [(-15,-20),(-15,20),(0,25),(15,20),(15,-20)]
           |> filled yellow
           |> move pos
           |> rotate ((vectorAngle vel) - (degrees 90))
  , update state = moveStrait rocket pos vel state.deltas
  , action state = state
  , collide dam = (explosion pos 2 100)
  , health = 0
  , checkCollision = within pos 99
  }

shooter : Vector -> Vector -> Agent
shooter pos vel =
  let shape = polygon [(-15,-30),(10,20),(0,30),(-10,20),(-15,-30)]
              |> filled orange
  in
    lineFlyer shape (basicGun badBullet 1000 0.4) 2 pos vel

changeUpdate : Agent->(Agent->State->Agent)->Agent
changeUpdate agent update =
  case agent of
    Alive a -> Alive {a | update <- (update agent)}
    Dead -> Dead

dieAfter : Agent -> Agent
dieAfter agent =
  changeUpdate agent (\a s -> Dead)

explosion : Vector -> Int -> Float -> Agent
explosion pos damage rad = Alive
  { position = pos
  , draw = move pos (filled red (circle rad))
  , update state = dieAfter (explosion pos damage rad) 
  , collide dam = Dead
  , health = damage
  , checkCollision = within pos (rad+20.0)
  , action state = state
  }

sprayerGun: Weapon
sprayerGun =
  { coolDown = 700
  , wait = 0
  , action pos angle state = shootBulletFan badBullet pos 0.5 (angle - (degrees 60)) (degrees 30) 5 state
  }

sprayer : Vector -> Vector -> Agent
sprayer pos vel =
  lineFlyer triangleShip sprayerGun 5 pos vel

player : PlayerWeapons -> Int -> Vector -> Agent
player weapons health position =
  let top = (toFloat screenHeight) / 2
      bot = -top
      right = (toFloat screenWidth) / 2
      left = -right
      nextPosition state = let
                             (x,y) = (vectorAdd (vectorScale state.deltas state.playerMove) position)
                           in
                             (clamp left right x, clamp bot top y)
      nextWeapon state = if state.playerFire 
                         then updatePlayerWeapons state.deltas weapons
                         else coolDownPlayerWeapons state.deltas weapons
  in
    Alive { update state = player (Debug.watch "Weapons" (nextWeapon state)) health (nextPosition state)
          , draw   = move position playerShip
          , action state = actionOnPress (firePlayerWeapons weapons position (degrees 90)) state
          , checkCollision = within position 20
          , collide dam = applyCollision health dam (\h -> player weapons h position) (explosion position 100 800)
          , position = position
          , health = health
          }

{-everything else -}

drawState : State -> Element
drawState state =
  let
    drawpend = (\e l -> (draw e)::l)
    f1 = List.map draw state.goodBullets
    f2 = List.foldl drawpend f1 state.badAgents
    f3 = List.foldl drawpend f2 state.badBullets
    f4 = (draw state.player)::f3
  in
    collage screenWidth screenHeight (state.background::f4)

aboveScreen = (toFloat screenHeight)/2 + 200
belowScreen = -aboveScreen
rightScreen = (toFloat screenWidth)/2 + 200
leftScreen = -rightScreen

events = 
  [(500,shooter (-400,aboveScreen) (0,-0.2)),
   (600,shooter (400,aboveScreen) (0,-0.2)),
   (1000,sprayer (0,aboveScreen) (0,-0.2)),
   (1200,shooter (rightScreen,aboveScreen) (-0.1,-0.1)),
   (1200,shooter (leftScreen, aboveScreen) (0.1,-0.1)),
   (3000,exploder -0.0004 6000 3 (rightScreen,aboveScreen) (0,-0.3)),
   (0,exploder 0.0004 6000 3 (leftScreen,aboveScreen) (0,-0.3)),
   (0,exploder -0.0004 6000 3 (leftScreen,belowScreen) (0,0.3)),
   (0,exploder 0.0004 6000 3 (rightScreen,belowScreen) (0,0.3)),
   (9000,sprayer (0,aboveScreen) (0,-0.3)),
   (12000,sprayer (300,aboveScreen) (0,-0.3)),
   (0,sprayer (-300,aboveScreen) (0,-0.3)),
   (0,shooter (600,aboveScreen) (-0.1,-0.2)),
   (13000,shooter (-600,aboveScreen) (0.1,-0.2)),
   (15000,stabber 2 (300,aboveScreen) 0),
   (0,stabber 2 (150,aboveScreen) 0),
   (0,stabber 2 (0,aboveScreen) 0),
   (0,stabber 2 (-150,aboveScreen) 0),
   (0,stabber 2 (-300,aboveScreen) 0),
   (1700,sprayer (-500,aboveScreen) (-0.1,-0.3)),
   (0,sprayer (300,aboveScreen) (0,-0.3)),
   (0,exploder -0.003 5000 3 (leftScreen,-300) (0.4,0)),
   (0,shooter (-200,aboveScreen) (0,-0.3)),
   (20000,stabber 2 (leftScreen,aboveScreen) 0),
   (0,stabber 2 (-600,aboveScreen) 0),
   (0,stabber 2 (-450,aboveScreen) 0),
   (0,stabber 2 (-300,aboveScreen) 0),
   (0,stabber 2 (-150,aboveScreen) 0),
   (0,stabber 2 (0,aboveScreen) 0),
   (0,stabber 2 (150,aboveScreen) 0),
   (0,stabber 2 (300,aboveScreen) 0),
   (0,stabber 2 (450,aboveScreen) 0),
   (0,stabber 2 (600,aboveScreen) 0),
   (0,stabber 2 (leftScreen,600) 0),
   (0,stabber 2 (leftScreen,450) 0),
   (0,stabber 2 (leftScreen,300) 0),
   (0,stabber 2 (leftScreen,150) 0),
   (0,stabber 2 (leftScreen,0) 0),
   (0,stabber 2 (rightScreen,600) 0),
   (0,stabber 2 (rightScreen,450) 0),
   (0,stabber 2 (rightScreen,300) 0),
   (0,stabber 2 (rightScreen,150) 0),
   (0,stabber 2 (rightScreen,0) 0),
   (21000,sprayer (leftScreen,300) (0.3,0)),
   (0,sprayer (rightScreen,-300) (-0.3,0)),
   (22000,exploder 0.1 800 3 (leftScreen,0) (vectorPolar 0.3 0)),
   (0, exploder 0.0005 2000 3 (rightScreen,200) (-0.4,0.1)),
   (0, exploder 0.0004 2500 3 (leftScreen,-300) (0.4,-0.1)),
   (27000,stabber 2 (300,aboveScreen) 0),
   (0,stabberCarrier 350 (leftScreen,500) (0.1,0)),
   (0,bulletChaserCarrier 350 (rightScreen,-500) (-0.1,0))]


generatePoints : List Vector
generatePoints =
  let
    pointgen = Random.list 1500 (Random.pair (Random.float -600 600) (Random.float -screenHeight (20000-screenHeight)))
    (points,seed) = Random.generate pointgen (Random.initialSeed 123)
  in
    points

background : Form
background =
  let space = moveY (10000-screenHeight) (filled black (rect screenWidth 20000))
      points = generatePoints
      star = (filled white (circle 1))
      stars = List.map ((flip move) star) points
  in
    group (space::stars)

initState =
  { player = player playerWeapons playerHealth (0.0,0.0)
  , goodBullets = []
  , badAgents = []
  , badBullets = []
  , playerMove = (0,0)
  , playerFire = False
  , deltas = 0.0
  , age = 0.0
  , events = events
  , background = background
  }

progress : State -> State
progress state =
  if List.isEmpty state.events
  then state
  else 
    let (time,next) = List.head state.events
    in
      if state.age > time
      then progress {state | badAgents <- next::state.badAgents
                           , events <- List.tail state.events
                           }
      else state

setInput : State -> (Time,{x:Int,y:Int},Bool) -> State
setInput state (deltas,dirs,pressed) = 
  {state | deltas     <- deltas
         , playerMove <- vectorScale playerSpeed (toFloat dirs.x, toFloat dirs.y)
         , playerFire <- pressed
         , age <- state.age + deltas}

collide : Agent -> Agent -> (Agent,Agent)
collide agent1 agent2 =
  case agent1 of
    Alive a1 -> case agent2 of
                  Alive a2 ->
                    if a1.checkCollision agent2 || a2.checkCollision agent1
                    then
                      let
                        newA1 = a1.collide a2.health
                        newA2 = a2.collide a1.health
                      in
                        (newA1,newA2)
                    else
                      (agent1,agent2)
                  Dead -> (agent1,agent2)
    Dead -> (agent1,agent2)


collisionHelper : (Agent,List Agent)->List Agent->(Agent,List Agent)
collisionHelper (collider,collided) agents =
  if List.isEmpty agents
  then (collider,collided)
  else
    let
      next = List.head agents
      rest = List.tail agents
    in
      case collider of
        Alive c -> case next of
                     Alive n ->
                       let
                         (newCollider,newNext) = collide collider next
                       in
                         collisionHelper (newCollider,newNext::collided) rest
                     Dead ->
                       collisionHelper (collider,collided) rest
        Dead -> (Dead,List.foldl (::) collided agents)

collisionFold : Agent -> State -> State
collisionFold goodBullet state =
  let
    (newGoodBullet,newBadAgents) = collisionHelper (goodBullet,[]) state.badAgents
  in
    case newGoodBullet of
      Alive g ->
        {state | goodBullets <- newGoodBullet::state.goodBullets
               , badAgents  <- newBadAgents
               }
      Dead ->
        {state | badAgents <- newBadAgents}

doCollisions : State -> State
doCollisions state =
  let (player,badAgents) = collisionHelper (state.player,[]) state.badAgents
      (newPlayer,badBullets) = collisionHelper (player,[]) state.badBullets
      newState = {state | player <- newPlayer, badAgents <- badAgents, badBullets <- badBullets}
  in
  List.foldl collisionFold {newState|goodBullets<-[]} newState.goodBullets
  

doActions : State -> State
doActions state =
  case state.player of
    Dead -> state
    Alive a ->
      let s1 = List.foldl action state state.goodBullets
          s2 = List.foldl action s1    state.badBullets
          s3 = List.foldl action s2    state.badAgents
      in action state.player s3

doUpdates : State -> State
doUpdates state =
  {state | goodBullets <- List.map (update state) state.goodBullets
         , badBullets  <- List.map (update state) state.badBullets
         , badAgents   <- List.map (update state) state.badAgents
         , player      <- update state state.player
         }

doBackground : State -> State
doBackground state =
  {state | background <- moveY (-scrollSpeed*state.deltas) state.background}

step : (Time,{x:Int,y:Int},Bool) -> State -> State
step input state =
  setInput state input
  |> progress
  |> doCollisions
  |> doActions
  |> doUpdates
  |> doBackground
  |> Debug.watch "Step"

signal =
  let fpsSig = (fps 30)
  in
    Signal.sampleOn fpsSig (Signal.map3 (,,) fpsSig Keyboard.wasd Keyboard.space)

main : Signal Element
main = Signal.map drawState (Signal.foldp step initState signal)

