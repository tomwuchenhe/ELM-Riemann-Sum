background = [ rect 192 168 |> filled (rgb 243 243 243) ]

rectangleForm : Float -> Float -> Shape userMsg
rectangleForm width height =
    rect width height |> filled red |> move (-3, 10)



keepThreeChars : String -> String
keepThreeChars s =
    String.left 3 s


getBaseRigP : List Float -> List (Float, Float)
getBaseRigP list = 
    case list of
        [] -> []
        (x::list_sub)  -> [(10*x,0)] ++ getBaseRigP list_sub


firstElement : (Float, Float) -> Float
firstElement myTuple = 
    case myTuple of
        (x, _) -> x



eval x a1 a2 = a1 * (( (a2 *  x)  ))^2


            
buttonFlex x y= group[
  roundedRect 20 10 3
    |> filled y
    |> move (24,43)
  ,roundedRect 21 11 3
    |> outlined (solid 1) (rgb 16 14 64)
    |> move (24,43)
  ,text x
    |> filled black
    |> scale 0.6
    |> move (16.5,41)
  
  ]
getList2 : Float -> Float -> Float -> List Float
getList2 start end number= if number == -1 then [] else start :: getList2 (start+(end-start)/number) end (number-1)


getList : List Int -> Float -> Float -> Float -> List Int
getList list a b c = List.filter ( \x -> modBy (getInc a b c) x == 0) list
--- [(20,40)] ++[(10,40)] ++[(10,10)] ++[(0,10)]

mkRect : Model -> Shape userMsg
mkRect model =
     openPolygon ( (getBaseRigP (getList2 (model.rangeS) (model.rangeE) (model.r1))) ++ (getNumRigP ( reverseL (getList2 (model.rangeS) (model.rangeE) (model.r1))) model.a1 model.k1 ))
     |> filled (rgba 0 100 0 0.8) 
mkRectLeft: Model -> Shape userMsg
mkRectLeft model =
     openPolygon ( (getBaseRigP (getList2 (model.rangeS) (model.rangeE) (model.r1))) ++ (getNumLeftP ( reverseL (getList2 (model.rangeS) (model.rangeE) (model.r1))) model.a1 model.k1 model.rangeS model.rangeE model.r1) )
     |> filled (rgba 0 128 128 0.8) 

mkRectMid: Model -> Shape userMsg
mkRectMid model =
      openPolygon ( (getBaseRigP (getList2 (model.rangeS) (model.rangeE) (model.r1))) ++ (getNumMidP ( reverseL (getList2 (model.rangeS) (model.rangeE) (model.r1))) model.a1 model.k1 model.rangeS model.rangeE model.r1) )
      |> filled (rgba 30 144 255 0.8) 
      
reverseL : List Float -> List Float
reverseL list = List.reverse list

maybeToFirstFloat : Maybe Float -> Float
maybeToFirstFloat maybeTuple =
    case maybeTuple of
        Just x ->
            x
        Nothing ->
           1


getNumRigP : List Float -> Float -> Float -> List (Float, Float)
--getNumRigP (x::remain) a1 k1 = if remain == [] then [] else [(10*x, 10.5*(a1*(k1*x))^2)] ++ [(10*maybeToFirstFloat (List.head remain),10.5*(a1*(k1*(maybeToFirstFloat (List.head remain))))^2)] ++ getNumRigP remain a1 k1 asdffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
getNumRigP list a1 k1 = 
    case list of 
        [] -> []
        (x::remain) -> [(10*x, 10*a1*((k1*x)^2))] ++ [(10*maybeToFirstFloat (List.head remain),10*a1*((k1*x)^2))] ++ getNumRigP remain a1 k1

getNumLeftP : List Float -> Float -> Float -> Float -> Float -> Float -> List (Float,Float)
getNumLeftP list (a1) k1 start end number = 
    case list of 
        [] -> []
        (x::remain) -> if x /= 0 then [(10*x, 10*(a1*(k1*(x-(end-start)/number))^2))] ++ [(10*maybeToFirstFloat (List.head remain),10*(a1*(k1*(x-(end-start)/number))^2))] ++ getNumLeftP remain a1 k1 start end number else [(0,0)]
getNumMidP : List Float -> Float -> Float -> Float -> Float -> Float -> List (Float,Float)
getNumMidP list a1 k1 start end number = 
    case list of 
        [] -> []
        (x::remain) -> if x /= 0 then [(10*x, 10*(a1*(k1*(x-(end-start)/number + x)/2)^2))] ++ [(10*maybeToFirstFloat (List.head remain),10*(a1*(k1*(x-(end-start)/number + x)/2)^2))] ++ getNumMidP remain a1 k1 start end number else [(0.25,0.25)]


startList : List Float
startList = List.map toFloat <| List.range -10000 10000

getInc : Float -> Float -> Float -> Int 
getInc end start num = ceiling ((start - end) / num)

getFloat : List Int -> List Float
getFloat x  = List.map toFloat <| x


       
mkSinCurve : Model -> Shape userMsg
mkSinCurve model = 
  
  -- To draw a "curve" we could use a curve, but everything
  -- we ever draw in a computer is eventually a line or a
  -- triangle, so we might as well divide our curve into
  -- tiny lines oursevles, and use openPolygon
  openPolygon 
  -- now we need a list of points
  {-  
      In Haskell, we have list comprehensions, which look
      like set theory.
         [ f i | i <- [-3,-2,-1,1,2,3] ]
      But Elm does not support this any more, and requires
      us to use both List.range, and List.map, and if our
      numbers want to be floats we have to use toFloat, 
      because List.range only produces Ints.
  -}
    ( List.filter ( \x -> model.a1 * (( (model.k1 *  x) ))^2 < 600 ) startList
                |> List.map ( \ idx ->
                              let x : Float 
                                  x = 0.1 * idx
                                  y : Float
                                  y = model.a1 * (( (model.k1 *  x)  ))^2
                              in  
                                if y <= 6 then
                                  ( gScale * x, gScale * y )
                                else
                                  (gScale * x, gScale * 6)
                                  
                              
                            )
    )
       |> outlined (solid 1) (rgb 0 0 128)

gScale = 10.5 -- scale (x,y) into graphics coordinates

-- our example has two parts, the graph and the function controls
checkP model =
  [if model.mid_init == True then
  buttonFlex "MidP" (rgba 30 144 255 0.8)  |> move (30,34) |> scale 0.7 |> notifyTap MidC
  else if model.left_init == True then
  buttonFlex "LefP" (rgba 0 128 128 0.8)  |> move (60,34) |> scale 0.7 |> notifyTap LefC
  else if model.right_init == True then
  buttonFlex "RigP" (rgba 0 100 0 0.8)  |> move (90,34) |> scale 0.7 |> notifyTap RigC
  else
  [
  buttonFlex "MidP" white |> move (30, 34) |> scale 0.7 |> notifyTap MidC
  ,
  buttonFlex "LefP" white |> move (60,34) |> scale 0.7 |> notifyTap LefC
  ,
  buttonFlex "RigP" white |> move (90,34) |> scale 0.7 |> notifyTap RigC
  ]|> group
  ]
  
myShapes model =
  [ 
   background |> group
  , bigGraph model 
  , rect 192 0.7 |> filled black |> move (0,-20)
  , controls model
  , checkP model |> group
  ]

-- good way to control one number with arrows
upDown clr number upMsg downMsg = group
  [ triangle 3
      |> filled clr
      |> rotate (degrees 90)
      |> move (0, 15)
      |> notifyTap upMsg 
  , triangle 3
      |> filled clr 
      |> rotate (degrees -90)
      |> move (0, -7)
      |> notifyTap downMsg

  , text (keepThreeChars (String.fromFloat number))
      |> centered |> filled clr
  ] 
  
controls model = 
  group [ 
    upDown (rgb 244 101 40)  model.a1 AU AD
        |> scale 0.6
        |> move (18,22)
  , upDown (rgb 0 144 132) model.k1 KU KD
        |> scale 0.6
        |> move (29,22)
  , upDown (rgb 250 157 0) model.r1 CU CD
        |> scale 0.6
        |> move (-26,22)
  , upDown (rgb 91 91 91) model.rangeS SU SD
        |> scale 0.6
        |> move (-55,22)
  , upDown (rgb 91 91 91) model.rangeE S2U S2D
        |> scale 0.6
        |> move (-43,22)
  , text " (    x )"
      |> filled black
      |> scale 0.7
      |> move (20, 22)
  , text "2"
      |> filled black
      |> scale 0.5
      |> move (43, 27)
  , text "Num Rect"
      |> filled black
      |> scale 0.6
      |> move (-22, 22)
  , text "[     ,      ]"
      |> filled black
      |> scale 0.7
      |> move (-65, 22)
  ] |> move (-25, 30)
  

bigGraph model = group 
  -- axes
  [ line (-100,0) (100,0)
      |> outlined (solid 0.75) black
  , line (0,-100) (0,70)
      |> outlined (solid 0.75) black
  -- y lines
  , [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      |> List.map ( \ idx -> 
                    let
                      x = 10 * toFloat idx
                    in [ text (String.fromInt idx )
                              |> centered
                              |> filled black
                              |> scale 0.4
                              |> move (x ,-4)
                           , line (x, 64) (x,0)
                              |> outlined (solid 0.5) grey
                           , line (x, -7) (x,-64)
                              |> outlined (solid 0.5) grey
                       ]
                  )
      |> List.concat 
      |> group
  -- x lines
  , List.range -6 6 
      |> List.map ( \ idx -> 
                    let
                      y = 10*toFloat  idx
                    in [ text (String.fromInt idx)
                              |>  filled black
                              |> scale 0.4
                              |> move (2, y)
                           , line (-96, y) (0,y)
                              |> outlined (solid 0.5) grey
                           , line (7, y) (96,y)
                              |> outlined (solid 0.5) grey
                       ]
                  )
      |> List.concat 
      |> group
  -- the curve
  , mkSinCurve model
  ,if model.right_init == True then mkRect model
    else if model.mid_init == True then mkRectMid model
    else  mkRectLeft model
  ] |> move (0, -20)


type Msg = Tick Float GetKeyState
         | AU 
         | AD 
         | KU 
         | KD 
         | CU
         | CD
         | SU
         | SD
         | S2U
         | S2D
         | MidC
         | LefC
         | RigC
         
type State = MainMenu

type alias Model = 
    { time : Float
    , a1 : Float
    , k1 : Float
    , r1 : Float
    , rangeS : Float
    , rangeE : Float
    , right_init : Bool
    , left_init : Bool
    , mid_init : Bool
    }

update msg model = case msg of
                     Tick t _ -> { model | time = t }
                     AU -> { model | a1 = model.a1 + 0.1}
                     AD -> { model | a1 = model.a1 - 0.1}
                     KU -> { model | k1 = model.k1 + 0.1}
                     KD -> { model | k1 = model.k1 - 0.1}
                     CU -> { model | r1 = model.r1 + 1}
                     CD -> { model | r1 = model.r1 - 1}
                     SU -> { model | rangeS = model.rangeS + 1}
                     SD -> { model | rangeS = model.rangeS - 1}
                     S2U -> { model | rangeE = model.rangeE + 1}
                     S2D -> { model | rangeE = model.rangeE - 1}
                     MidC -> { model | mid_init = True}
                     LefC -> { model | left_init = True}
                     RigC -> { model | right_init = True}


init = { time = 0 
       , a1 = 1
       , k1 = 1
       , r1 = 2
       , rangeS = 0
       , rangeE = 2
       , right_init = False
       , left_init = False
       , mid_init = False
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)
