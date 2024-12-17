{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    Name,
    Workout(..),
    Intensity(..),
    WorkoutName(..),
    Meal(..),
    MealName(..),
    State(..),
    and3_0,
    parseWord,
    parseChar',
    parseQuery,
    emptyState,
    stateTransition
    ) where
import qualified Data.Char as C
import qualified Data.List as L


data Query = FitnessApp Name [Workout] [Meal]
           | AddWorkout [Workout]
           | AddMeal [Meal]
           | ListState
           | RemoveMeal
           | RemoveWorkout
           | TotalCalories
        deriving (Eq, Show)
type Name = String

data Workout = Workout WorkoutName Intensity [Workout]
             deriving (Eq, Show)

data Intensity = Low | Medium | High
               deriving (Eq, Show)

data WorkoutName = Strength | Flexibility | Cardio | Conditioning | Calisthenics | MartialArts
                deriving (Eq, Show)

data Meal = Meal MealName Integer [Meal]
          deriving (Eq, Show)

data MealName = Main | Appetizer | Dessert | Snack
              deriving (Eq, Show)


-- >>> parseQuery "RemoveWorkout & ListState"
-- Right (RemoveWorkout,"& ListState")
parseQuery :: String -> Either String (Query,String)
parseQuery str =
    case or7 parseFitnessApp parseAddWorkout parseAddMeal parseRemoveWorkout parseRemoveMeal parseListState parseCalories str of
        Right (query,rest) -> Right (query,rest)
        Left _ -> Left "Failed to parse query: Unknown command"



type Parser a = String -> Either String (a, String)

parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

parseChar :: Parser Char
parseChar [] = Left "Empty input"
parseChar (x:xs) =
    if not (C.isSpace x)
    then Right (x, xs)
    else Left "Encountered a whitespace"



parseChar' :: Char -> Parser Char
parseChar' ' ' [] = Right(' ',"")
parseChar' c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar' c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

parseString' :: Parser String
parseString' [] = Right ("", "")
parseString' (c:str) =
    case parseChar (c:str) of
        Right (ch, st) ->
            case parseString' st of
                Right (ch1, st1) -> Right (ch : ch1, st1)
                Left _ -> Right ([ch], st)
        Left e -> Left (e)


parseNumber :: Parser Integer
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseWhiteSpace :: Parser String
parseWhiteSpace [] = Left "no white spaces"
parseWhiteSpace str =
    let
        spaces = L.takeWhile C.isSpace str
        rest = drop (length spaces) str
    in
        case spaces of
            [] -> Left "not a space"
            _ -> Right ("", rest)


-- >>> parseWord "blyat\n a"
-- Right ("blyat","\n a")

parseWord :: Parser String
parseWord [] = Left "empty string"
parseWord str = 
    let
        word = parseString' str
    in  
        case word of
            Right(word', rest) ->
                Right(word',rest) 
            Left(e) -> Left(e)


parseNumber' :: Parser Integer
parseNumber'[] = Left "empty string"
parseNumber' str = 
    let
        word = parseNumber str
    in  
        case word of
            Right(word', rest) ->
                Right(word',rest) 
            Left(e) -> Left(e)

-- <workoutName> ::= "Strenght" | "Flexibility" | "Cardio" | "Conditioning" | "Calisthenics" | "Martial_arts"
parseWorkoutName :: Parser WorkoutName
parseWorkoutName [] = Left "empty string"
parseWorkoutName str =
    let 
        word = parseWord str
    in
        case word of
            Right(word', rest) -> 
                
                case word' of
                    "Strength" -> Right(Strength,rest)
                    "Flexibility" -> Right(Flexibility,rest)
                    "Cardio" -> Right(Cardio,rest)
                    "Conditioning" -> Right(Conditioning,rest)
                    "Calisthenics" -> Right(Calisthenics,rest)
                    "MartialArts" -> Right(MartialArts,rest)
                    _ -> Left "nera tokio workout"
            Left e -> Left e

-- <intensity> ::= "Low" | "Medium" | "High"
parseIntensity :: Parser Intensity
parseIntensity [] = Left "empty string"
parseIntensity str =
    let 
        word = parseWord str
    in
        case word of
            Right(word', rest) -> 
                case word' of
                    "Low" -> Right(Low,rest)
                    "Medium" -> Right(Medium,rest)
                    "High" -> Right(High,rest)
                    _ -> Left "nera tokio intensity"
            Left e -> Left e

-- <mealName> ::= "Main" | "Appetizer" | "Dessert" | "Snack"
parseMealName :: Parser MealName
parseMealName [] = Left "empty string"
parseMealName str =
    let 
        word = parseWord str
    in
        case word of
            Right(word', rest) -> 
                case word' of
                    "Main" -> Right(Main,rest)
                    "Appetizer" -> Right(Appetizer,rest)
                    "Dessert" -> Right(Dessert,rest)
                    "Snack" -> Right(Snack,rest)
                    _ -> Left "nera tokio meal"
            Left e -> Left e



and3_0 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3_0 f p1 p2 p3 str =
    case p1 str of
        Right (result1, rest1) -> 
            case p2 rest1 of
                Right (result2, rest2) ->
                    case p3 rest2 of
                        Right (result3, rest3) -> Right (f result1 result2 result3 ,rest3)
                        Left e -> Left e
                Left e -> Left e
        Left e -> Left e





-- <workout> ::= <workoutName> " " <intensity> " " "{" " " <subWorkouts> " " "}" " "
parseWorkout :: Parser Workout
parseWorkout [] = Left []
parseWorkout str =
    case and10 (\a _ b _ _ _ c _ _ _ -> Workout a b c) parseWorkoutName (parseChar' ' ') parseIntensity (parseChar' ' ') (parseChar' '{') (parseChar' ' ') parseWorkout'' (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
        Right(workout,rest'') ->      
                          Right(workout,rest'')
        Left e -> Left e
-- >>> parseWorkout' "Cardio Low { Calisthenics High {  } Cardio Medium {  }  } Cardio High {  }"
-- Right ([Workout Cardio Low [Workout Calisthenics High [],Workout Cardio Medium []],Workout Cardio High []],"")
parseWorkout' :: Parser [Workout]
parseWorkout' [] = Left ""
parseWorkout' str =
    case parseWorkout str of
        Right (workout, rest) ->
            case parseWorkout' rest of
                Right (workouts, finalRest) ->
                    Right (workout : workouts, finalRest) 
                Left _ -> Right ([workout], rest)
        Left e -> Left e

parseWorkout'' :: Parser [Workout]
parseWorkout'' [] = Left ""
parseWorkout'' str =
    case parseWorkout' str of
                Right(workouts,rest''' ) ->
                            Right(workouts,rest''')
                Left e ->
                    case and3_0 (\ _ _ _ -> ()) (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
                        Right(rest''') ->
                                Right([],str)
                        Left e -> Left e 


-- <meal> ::= <mealName> " " <calories> " " "{" " " <subMeals> " " "}" " "
parseMeal :: Parser Meal
parseMeal [] = Left []
parseMeal str =
    case and10 (\a _ b _ _ _ c _ _ _ -> Meal a b c) parseMealName (parseChar' ' ') parseNumber' (parseChar' ' ') (parseChar' '{') (parseChar' ' ') parseMeal'' (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
        Right((meal),rest'') ->      
                          Right(meal,rest'')
        Left e -> Left e

-- >>> parseWorkout' "Cardio Low {  } "
-- Right ([Workout Cardio Low []],"")
parseMeal' :: Parser [Meal]
parseMeal' [] = Left ""
parseMeal' str =
    case parseMeal str of
        Right (meal, rest) ->
            case parseMeal' rest of
                Right (meals, finalRest) ->
                    Right (meal : meals, finalRest) 
                Left _ -> Right ([meal], rest)
        Left e -> Left e

parseMeal'' :: Parser [Meal]
parseMeal'' [] = Left ""
parseMeal'' str =
    case parseMeal' str of
                Right(meals,rest''' ) ->
                            Right(meals,rest''')
                Left e ->
                    case and3_0 (\ _ _ _ -> ()) (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
                        Right(rest''') ->
                                Right([],str)
                        Left e -> Left e 



and10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i -> Parser j -> Parser k--(a, b, c, d, e, f, g, h, i, j)
and10 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 str =
    case p1 str of
        Right (result1, rest1) ->
            case p2 rest1 of
                Right (result2, rest2) ->
                    case p3 rest2 of
                        Right (result3, rest3) ->
                            case p4 rest3 of
                                Right (result4, rest4) ->
                                    case p5 rest4 of
                                        Right (result5, rest5) ->
                                            case p6 rest5 of
                                                Right (result6, rest6) ->
                                                    case p7 rest6 of
                                                        Right (result7, rest7) ->
                                                            case p8 rest7 of
                                                                Right (result8, rest8) ->
                                                                    case p9 rest8 of
                                                                        Right (result9, rest9) ->
                                                                            case p10 rest9 of
                                                                                Right (result10, rest10) ->
                                                                                    Right ((f result1 result2  result3 result4 result5 result6 result7 result8 result9 result10), rest10)
                                                                                Left e -> Left e
                                                                        Left e -> Left e
                                                                Left e -> Left e
                                                        Left e -> Left e
                                                Left e -> Left e
                                        Left e -> Left e
                                Left e -> Left e
                        Left e -> Left e
                Left e -> Left e
        Left e -> Left e



and5_3 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5_3 f p1 p2 p3 p4 p5 str =
    case p1 str of
        Right (result1, rest1) ->
            case p2 rest1 of
                Right (result2, rest2) ->
                    case p3 rest2 of
                        Right (result3, rest3) ->
                            case p4 rest3 of
                                Right (result4, rest4) ->
                                    case p5 rest4 of
                                        Right (result5, rest5) -> Right (f result1 result2 result3 result4 result5, rest5)-- 2 4 5
                                        Left e -> Left e
                                Left e -> Left e
                        Left e -> Left e
                Left e -> Left e
        Left e -> Left e
-- >>> parseFitnessApp "FitnessApp whatevz Strenght High {  } Main 200 {  } AddWorkout Cardio Low {  }"
-- Right (FitnessApp "whatevz" [Workout Strength High []] [Meal Main 200 []],"AddWorkout Cardio Low {  }")

-- <fitnessApp> ::= "FitnessApp " <Name> " " <workouts> " " <meals>
parseFitnessApp :: Parser Query
parseFitnessApp [] = Left "nothing to parse"
parseFitnessApp str = 
    case parseWord str of
        Right (word, rest) ->
            if word == "FitnessApp" 
            then case and5_3 (\_ a _ b c -> FitnessApp a b c)(parseChar' ' ') parseWord (parseChar' ' ') parseWorkout' parseMeal' rest of
                Right(app,rest'''') ->
                        Right(app,rest'''')               
                Left e -> Left e
            else Left "no such parser"
        Left e -> Left e 


and2 ::(a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 str =
    case p1 str of
        Right (result1, rest1) ->
            case p2 rest1 of
                Right (result2, rest2) -> Right (f result1 result2, rest2)
                Left e -> Left e
        Left e -> Left e

parseAddWorkout :: Parser Query
parseAddWorkout [] = Left "nothing to parse"
parseAddWorkout str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "AddWorkout"
                then case and2 (\_ a -> a) (parseChar' ' ')  parseWorkout' rest of
                    Right(workout,rest) ->
                        Right(AddWorkout workout,rest)
                    Left e -> Left e
                else Left "no such parser"
        Left e -> Left e

parseAddMeal :: Parser Query
parseAddMeal [] = Left "nothing to parse"
parseAddMeal str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "AddMeal"
                then case and2 (\_ a -> a) (parseChar' ' ') parseMeal' rest of
                    Right(meal,rest) ->
                        Right(AddMeal meal,rest)
                    Left e -> Left e
                else Left "no such parser"
        Left e -> Left e
-- >>> parseListState "ListState & AddWorkout Strenght High {  }"
-- Right (ListState,"& AddWorkout Strenght High {  }")
parseListState :: Parser Query
parseListState [] = Left "nothing to parse"
parseListState str =
    case parseWord str of
        Right(word,rest) ->
            if word == "ListState"
                then 
                    if rest == ""
                        then Right(ListState,rest)
                    else 
                        case parseChar' ' ' rest of
                            Right(_,rest') -> Right(ListState,rest')
                            Left e -> Left e 
            else Left "no such parser"
        Left e -> Left e
-- >>> parseRemoveMeal "RemoveMeal asd asd" 
-- Right (RemoveMeal,"asd asd")
parseRemoveMeal :: Parser Query
parseRemoveMeal [] = Left "nothing to parse"
parseRemoveMeal str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "RemoveMeal"
                then 
                    if rest == ""
                        then Right(RemoveMeal,rest)
                    else 
                        case parseChar' ' ' rest of
                            Right(_,rest') -> Right(RemoveMeal,rest')
                            Left e -> Left e
                else Left "no such parser"
        Left e -> Left e
parseRemoveWorkout :: Parser Query
parseRemoveWorkout [] = Left "nothing to parse"
parseRemoveWorkout str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "RemoveWorkout"
                then 
                    if rest == ""
                        then Right(RemoveWorkout,rest)
                    else 
                        case parseChar' ' ' rest of
                            Right(_,rest') -> Right(RemoveWorkout,rest')
                            Left e -> Left e
                else Left "no such parser"
        Left e -> Left e
parseCalories :: Parser Query
parseCalories [] = Left "nothing to parse"
parseCalories str =
    case parseWord str of
        Right(word,rest) ->
            if word == "TotalCalories"
                then if rest == ""
                        then Right(TotalCalories,rest)
                    else 
                        case parseChar' ' ' rest of
                            Right(_,rest') -> Right(TotalCalories,rest')
                            Left e -> Left e
            else Left "no such parser"
        Left e -> Left e

or7 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or7 p1 p2 p3 p4 p5 p6 p7 str =
    case p1 str of
        Right r1 -> Right r1
        Left e1 -> case p2 str of
            Right r2 -> Right r2
            Left e2 -> case p3 str of
                Right r3 -> Right r3
                Left e3 -> case p4 str of
                    Right r4 -> Right r4
                    Left e4 -> case p5 str of
                        Right r5 -> Right r5
                        Left e5 -> case p6 str of
                            Right r6 -> Right r6
                            Left e6 -> case p7 str of
                                Right r7 -> Right r7
                                Left e7 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5 ++ "; " ++ e6 ++ "; " ++ e7)

data State = State
  { appName  :: String
  , workouts :: [Workout]
  , meals    :: [Meal]
  } deriving (Show, Eq)

emptyState :: State
emptyState = State
    { appName  = "MyFitnessApp"
     , workouts = []
     , meals    = []
    }
-- 


sumMealCalories :: Meal -> Integer
sumMealCalories (Meal _ calories subMeals) = calories + sum (map sumMealCalories subMeals)


sumAllMealCalories :: State -> Integer
sumAllMealCalories st = sum (map sumMealCalories (meals st))



stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st (AddWorkout workout) = 
  let updatedWorkouts = workout ++ workouts st 
      newState = st { workouts = updatedWorkouts }
      
  in Right (Just "Workout added!",newState)

stateTransition st (AddMeal meal) = 
  let updatedMeals = meal ++ meals st 
      newState = st { meals = updatedMeals }
  in Right (Just "Meal added!", newState)

stateTransition st (FitnessApp name newWorkouts newMeals) = 
  let newState = st { appName = name, workouts = newWorkouts, meals = newMeals }
  in Right (Just "FitnessApp state updated!", newState)

stateTransition st RemoveMeal =
  case meals st of
    [] -> Left "No meals to remove."
    (_:remainingMeals) ->
      let newState = st { meals = remainingMeals }
      in Right (Just "Removed the first meal.", newState)

stateTransition st RemoveWorkout =
  case workouts st of
    [] -> Left "No workouts to remove."
    (_:remainingWorkouts) ->
      let newState = st { workouts = remainingWorkouts }
      in Right (Just "Removed the first workout.", newState)

stateTransition st ListState = 
  Right (Just ("Current state:\n" ++ show st), st)

stateTransition st TotalCalories = 
    let calories = sumAllMealCalories st
    in
  Right (Just ("Total Calories:\n" ++ show calories ), st)

stateTransition _ _ = Left "Query type not supported."
