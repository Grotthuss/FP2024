
{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    Name,
    Workout,
    Intensity,
    WorkoutName,
    Meal,
    MealName,
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where
import qualified Data.Char as C
import qualified Data.List as L
--import qualified Data.String as Stringas
--import qualified Distribution.Compat.CharParsing as S
--import Control.Monad.Trans.Cont (reset)

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.


data Query = FitnessApp Name [Workout] [Meal]
           | AddWorkout [Workout]
           | AddMeal [Meal]
           | ListState

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






-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show :: Query -> String
  show (FitnessApp name workouts meals) = 
    "FitnessApp: " ++ name ++ "\nWorkouts: " ++ show workouts ++ "\nMeals: " ++ show meals
  show (AddWorkout workout) = 
    "AddWorkout: " ++ show workout
  show (AddMeal meal) = 
    "AddMeal: " ++ show meal

-- | Parses user's input.
-- The function must have tests.
-- >>> parseQuery "FitnessApp whatevz Strenght High { Cardio Medium { } Calisthenics Low { } } Main 200 { Dessert 100 { } }"
-- Right FitnessApp: whatevz
-- Workouts: [Workout Strength High [Workout Cardio Medium [],Workout Calisthenics Low []]]
-- Meals: [Meal Main 200 [Meal Dessert 100 []]]



parseQuery :: String -> Either String Query
parseQuery [] = Left "Nothing to parse"
parseQuery str =
    case or' parseFitnessApp parseAddWorkout parseAddMeal parseListState str of
        Right (query, _) -> Right query
        _ -> Left "Failed to parse query: Unknown command"


type Parser a = String -> Either String (a, String)
-- >>> parseChar 'a' "aaa"
-- Right ('a',"aa")



-- >>> parseLetter "fsdf"
-- Right ('f',"sdf")
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

parseChar :: Parser Char
parseChar [] = Left "Empty input"
parseChar (x:xs) =
    if not (C.isSpace x)
    then Right (x, xs)
    else Left "Encountered a whitespace"

-- Parse a string of characters until a whitespace is encountered
-- >>> parseString' "asdas saddas da da"
-- Right ("asdas","saddas da da")
parseString' :: Parser String
parseString' [] = Right ("", "")
parseString' (c:str) =
    case parseChar (c:str) of
        Right (ch, st) ->
            case parseString' st of
                Right (ch1, st1) -> Right (ch : ch1, st1)
                Left _ -> Right ([ch], st)  -- When the rest is whitespace, end parsing
        Left e -> Left (e)

-- >>> parseNumber "123    dklj"
-- Right (123,"    dklj")
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

-- >>> parseWhiteSpace "  b                  bl"
-- Right ("","b                  bl")
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


-- >>> parseWord " >"
-- Left "Encountered a whitespace"
parseWord :: Parser String
parseWord [] = Left "empty string"
parseWord str = 
    let
        word = parseString' str
    in  
        case word of
            Right(word', rest) -> 
                case parseWhiteSpace rest of
                    Right(_,rest') -> Right(word',rest')
                    Left(msg) -> Right(word',rest)
            Left(e) -> Left(e)

-- >>> parseNumber' "123            a"            
-- Left "not a number"
parseNumber' :: Parser Integer
parseNumber'[] = Left "empty string"
parseNumber' str = 
    let
        word = parseNumber str
    in  
        case word of
            Right(word', rest) -> 
                case parseWhiteSpace rest of
                    Right(_,rest') -> Right(word',rest')
                    Left(msg) -> Right(word',rest)
            Left(e) -> Left(e)

-- <workoutName> ::= "Strenght" | "Flexibility" | "Cardio" | "Conditioning" | "Calisthenics" | "Martial_arts"
-- >>> parseWorkoutName "Strenght       " 
-- Right (Strength,"")
parseWorkoutName :: Parser WorkoutName
parseWorkoutName [] = Left "empty string"
parseWorkoutName str =
    let 
        word = parseWord str
    in
        case word of
            Right(word', rest) -> 
                
                --if(word' == "strenght" || word' == "flexibility" || word' == "cardio" || word' == "conditioning" || word' == "calisthenics" || word' == "martial_arts" ) then Right(word',rest) else Left "nera tokio workout"
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
                --if(word' == "low" || word' =="medium" || word' == "high") then Right(word',rest) else Left "nera tokio intensity"
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
                --if(word' == "main" || word' =="appetizer" || word' == "desser" || word' == "snack") then Right(word',rest) else Left "nera tokio meal"
                    "Main" -> Right(Main,rest)
                    "Appetizer" -> Right(Appetizer,rest)
                    "Dessert" -> Right(Dessert,rest)
                    "Snack" -> Right(Snack,rest)
                    _ -> Left "nera tokio meal"
            Left e -> Left e

-- <workout> ::= <workoutName> " " <intensity> " " "{ " <subWorkouts> " } "
-- >>> parseWorkout "Strenght High"
-- Right (Workout Strength High,"")
parseWorkout :: Parser Workout
parseWorkout [] = Left []
parseWorkout str =
    case parseWorkoutName str of
        Right(name, rest) ->
            case parseIntensity rest of
                Right(workoutIntensity, rest') ->
                    case parseWord rest' of
                        Right(_,rest'') ->
                            --Right(Workout name workoutIntensity,rest')
                            case parseWorkout' rest'' of
                                Right(workouts,rest''' ) ->
                                    case parseWord rest''' of 
                                        Right(_,rest'''') ->
                                            Right(Workout name workoutIntensity workouts,rest'''')
                                        Left e -> Left e
                                Left e ->
                                    case parseWord rest'' of
                                        Right(_,rest''') ->
                                            Right(Workout name workoutIntensity [],rest''')
                                        Left e -> Left e         
                        Left e -> Left e
                Left e -> Left e    
        Left e -> Left e    


-- >>> parseWorkout' "Strenght High { } "
-- Right ([Workout Strength High []],"")
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



parseMeal :: Parser Meal
parseMeal [] = Left []
parseMeal str =
    case parseMealName str of
        Right(name, rest) ->
            case parseNumber' rest of
                Right(calories, rest') ->
                    case parseWord rest' of
                        Right(_,rest'') ->
                            --Right(Workout name workoutIntensity,rest')
                            case parseMeal' rest'' of
                                Right(meals,rest''' ) ->
                                    case parseWord rest''' of 
                                        Right(_,rest'''') ->
                                            Right(Meal name calories meals,rest'''')
                                        Left e -> Left e
                                Left e ->
                                    case parseWord rest'' of
                                        Right(_,rest''') ->
                                            Right(Meal name calories [],rest''')
                                        Left e -> Left e         
                        Left e -> Left e
                Left e -> Left e    
        Left e -> Left e    

-- >>> parseMeal' "Main 300 { } Main 400 { } adsada"
-- Right ([Meal Main 300 [],Meal Main 400 []],"adsada")
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



-- >>> parseFitnessApp "FitnessApp whatevz Strenght High { } Main 200 { } sadsdsad"
-- Right (,"sadsdsad")
parseFitnessApp :: Parser Query
parseFitnessApp [] = Left "nothing to parse"
parseFitnessApp str = 
    case parseWord str of
        Right (word, rest) ->
            if word == "FitnessApp" 
            then case parseWord rest of
                Right(name,rest'') ->
                    case parseWorkout' rest'' of
                        Right(workout, rest''') ->
                            case parseMeal' rest''' of
                                Right(meal,rest'''') ->
                                    Right(FitnessApp name workout meal,rest'''')
                                Left e -> Left e
                        Left e -> Left e    
                Left e -> Left e
            else Left "no such parser"
        Left e -> Left e 

parseAddWorkout :: Parser Query
parseAddWorkout [] = Left "nothing to parse"
parseAddWorkout str = 
    case parseWord str of 
        Right(word,rest) ->
            if word == "AddWorkout"
                then case parseWorkout' rest of
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
                then case parseMeal' rest of
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


or' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or' p1 p2 p3 p4 str =
    case p1 str of
    Right r1 -> Right r1
    Left e1 -> case p2 str of
      Right r2 -> Right r2
      Left e2 -> case p3 str of
        Right r3 -> Right r3
        Left e3 -> case p4 str of
            Right r4 -> Right r4
            Left e4 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4)

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State
  { appName  :: String
  , workouts :: [Workout]
  , meals    :: [Meal]
  } deriving (Show, Eq)
-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State
    { appName  = "MyFitnessApp"
     , workouts = []
     , meals    = []
    }

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
-- >>> stateTransition emptyState (AddWorkout [Workout Cardio Low [Workout Cardio Medium []]])


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st (AddWorkout workout) = 
  let updatedWorkouts = workout ++ workouts st  -- Add the new workout to the list
      newState = st { workouts = updatedWorkouts }
  in Right (Just "Workout added!", newState)

stateTransition st (AddMeal meal) = 
  let updatedMeals = meal ++ meals st  -- Add the new meal to the list
      newState = st { meals = updatedMeals }
  in Right (Just "Meal added!", newState)

stateTransition st (FitnessApp name newWorkouts newMeals) = 
  let newState = st { appName = name, workouts = newWorkouts, meals = newMeals }
  in Right (Just "FitnessApp state updated!", newState)

stateTransition st ListState = 
  Right (Just $ "Current state:\n" ++ show st, st)

stateTransition _ _ = Left "Query type not supported."


-- >>> let initialState = emptyState
-- >>> let workoutQuery = AddWorkout (Workout Strength Medium [])
-- >>> let mealQuery = AddMeal (Meal Main 300 [])
-- >>> let Right (_, stateWithWorkout) = stateTransition initialState workoutQuery
-- >>> let Right (_, finalState) = stateTransition stateWithWorkout mealQuery
-- >>> print finalState
