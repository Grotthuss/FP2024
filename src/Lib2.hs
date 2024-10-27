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



parseQuery :: String -> Either String Query

parseQuery str =
    case or6 parseFitnessApp parseAddWorkout parseAddMeal parseRemoveWorkout parseRemoveMeal parseListState str of
        Right (query, _) -> Right query
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
                    "Strenght" -> Right(Strength,rest)
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



and3_0 :: Parser a -> Parser b -> Parser c -> String -> Either String String
and3_0 p1 p2 p3 str =
    case p1 str of
        Right (result1, rest1) -> 
            case p2 rest1 of
                Right (result2, rest2) ->
                    case p3 rest2 of
                        Right (result3, rest3) -> Right (rest3)
                        Left e -> Left e
                Left e -> Left e
        Left e -> Left e





-- <workout> ::= <workoutName> " " <intensity> " " "{" " " <subWorkouts> " " "}" " "
parseWorkout :: Parser Workout
parseWorkout [] = Left []
parseWorkout str =
    case and10 parseWorkoutName (parseChar' ' ') parseIntensity (parseChar' ' ') (parseChar' '{') (parseChar' ' ') parseWorkout'' (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
        Right((name,workoutIntensity,workouts),rest'') ->      
                          Right(Workout name workoutIntensity workouts,rest'')
        Left e -> Left e

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
                    case and3_0 (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
                        Right(rest''') ->
                                Right([],str)
                        Left e -> Left e 


-- <meal> ::= <mealName> " " <calories> " " "{" " " <subMeals> " " "}" " "
parseMeal :: Parser Meal
parseMeal [] = Left []
parseMeal str =
    case and10 parseMealName (parseChar' ' ') parseNumber' (parseChar' ' ') (parseChar' '{') (parseChar' ' ') parseMeal'' (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
        Right((meal,calories,meals),rest'') ->      
                          Right(Meal meal calories meals,rest'')
        Left e -> Left e


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
                    case and3_0 (parseChar' ' ') (parseChar' '}') (parseChar' ' ') str of
                        Right(rest''') ->
                                Right([],str)
                        Left e -> Left e 



and10 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i -> Parser j -> Parser (a, c, g)
and10 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 str =
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
                                                                                    Right ((result1, result3, result7), rest10)
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



and5_3 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (b, d, e)
and5_3 p1 p2 p3 p4 p5 str =
    case p1 str of
        Right (result1, rest1) ->
            case p2 rest1 of
                Right (result2, rest2) ->
                    case p3 rest2 of
                        Right (result3, rest3) ->
                            case p4 rest3 of
                                Right (result4, rest4) ->
                                    case p5 rest4 of
                                        Right (result5, rest5) -> Right ((result2,result4, result5), rest5)
                                        Left e -> Left e
                                Left e -> Left e
                        Left e -> Left e
                Left e -> Left e
        Left e -> Left e

-- <fitnessApp> ::= "FitnessApp " <Name> " " <workouts> " " <meals>
parseFitnessApp :: Parser Query
parseFitnessApp [] = Left "nothing to parse"
parseFitnessApp str = 
    case parseWord str of
        Right (word, rest) ->
            if word == "FitnessApp" 
            then case and5_3 (parseChar' ' ') parseWord (parseChar' ' ') parseWorkout' parseMeal' rest of
                Right((name,workout,meal),rest'''') ->
                        Right(FitnessApp name workout meal,rest'''')               
                Left e -> Left e
            else Left "no such parser"
        Left e -> Left e 


and2 :: Parser a -> Parser b -> Parser (b)
and2 p1 p2 str =
    case p1 str of
        Right (result1, rest1) ->
            case p2 rest1 of
                Right (result2, rest2) -> Right ((result2), rest2)
                Left e -> Left e
        Left e -> Left e

parseAddWorkout :: Parser Query
parseAddWorkout [] = Left "nothing to parse"
parseAddWorkout str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "AddWorkout"
                then case and2 (parseChar' ' ')  parseWorkout' rest of
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
                then case and2 (parseChar' ' ') parseMeal' rest of
                    Right(meal,rest) ->
                        Right(AddMeal meal,rest)
                    Left e -> Left e
                else Left "no such parser"
        Left e -> Left e
parseListState :: Parser Query
parseListState [] = Left "nothing to parse"
parseListState str =
    case parseWord str of
        Right(word,rest) ->
            if word == "ListState"
                then Right(ListState,rest)
            else Left "no such parser"
        Left e -> Left e
parseRemoveMeal :: Parser Query
parseRemoveMeal [] = Left "nothing to parse"
parseRemoveMeal str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "RemoveMeal"
                then 
                    Right(RemoveMeal,rest)
                else Left "no such parser"
        Left e -> Left e
parseRemoveWorkout :: Parser Query
parseRemoveWorkout [] = Left "nothing to parse"
parseRemoveWorkout str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "RemoveWorkout"
                then 
                    Right(RemoveWorkout,rest)
                else Left "no such parser"
        Left e -> Left e


or6 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6 p1 p2 p3 p4 p5 p6 str =
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
                            Left e6 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5 ++ "; " ++ e6)

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

stateTransition _ _ = Left "Query type not supported."
