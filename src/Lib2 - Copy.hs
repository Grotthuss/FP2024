{-# LANGUAGE InstanceSigs #-}
module Lib2Copy
    ( Query(..),
    Name,
    Workout(..),
    Intensity(..),
    WorkoutName(..),
    Meal(..),
    MealName(..),
    State'(..),
    and3_0,
    parseWord,
    parseChar',
    parseQuery,
    emptyState,
    stateTransition
    ) where
       
import qualified Data.Char as C
import qualified Data.List as L
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Applicative (Alternative(..))


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





type Parser a = String -> Either String (a, String)

type Parser' a = ExceptT String (State String) a






parse :: Parser' a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)


-- >>> parse (parseQuery) "FitnessApp whatevz Cardio Low {  } Main 200 {  }"
-- (Right (FitnessApp "whatevz" [Workout Cardio Low []] [Meal Main 200 []]),"")
parseQuery :: Parser' Query
parseQuery = or7 parseFitnessApp parseAddWorkout parseAddMeal parseRemoveWorkout parseRemoveMeal parseListState parseCalories
 





or7 :: Parser' a -> Parser' a -> Parser' a -> Parser' a -> Parser' a -> Parser' a -> Parser' a -> Parser' a
or7 p1 p2 p3 p4 p5 p6 p7 = do
    result1 <- attempt p1
    case result1 of
        Right val -> return val
        Left _    -> do
            result2 <- attempt p2
            case result2 of
                Right val -> return val
                Left _    -> do
                    result3 <- attempt p3
                    case result3 of
                        Right val -> return val
                        Left _    -> do
                            result4 <- attempt p4
                            case result4 of
                                Right val -> return val
                                Left _    -> do
                                    result5 <- attempt p5
                                    case result5 of
                                        Right val -> return val
                                        Left _    -> do
                                            result6 <- attempt p6
                                            case result6 of
                                                Right val -> return val
                                                Left _    -> do
                                                    result7 <- attempt p7
                                                    case result7 of
                                                        Right val -> return val
                                                        Left err  -> throwE err
  where
    -- Helper to attempt a parse and capture its result
    attempt parser = lift $ runExceptT parser


-- parseChar
parseChar :: Parser' Char
parseChar =
        do 
            input <- lift get
            case input of
                [] -> throwE "Empty input"
                (x:xs) ->
                    if not (C.isSpace x)
                        then lift $ put xs >> return x
                    else 
                        do
                            _ <- lift $ put input
                            throwE "Encountered a whitespace"




-- parseString'
-- >>> parse (parseString') "blyat "
-- (Right "blyat"," ")
parseString' :: Parser' String
parseString' =
    do
        input <- lift get
        case input of
            [] -> return ""
            (c:str) ->
                do
                    x <- parseChar
                    result <- lift (runExceptT parseString')
                    case result of
                        Right rest -> return (x : rest)       
                        Left _ -> return [x] 
                    



-- parseChar'
parseChar' :: Char -> Parser' Char
parseChar' c =
    do
        string <- lift get
        case string of
            [] -> if c == ' '
                        then return ' '
                  else
                    do 
                        _ <- lift $ put string
                        throwE "empty input"
            s@(h:t) ->
                    if c == h
                        then lift $ put t >> return h
                    else 
                        do
                            _ <- lift $ put string
                            throwE "not found" 







-- >>> parse (parseNumber') "123 ble"
-- (Right "123"," ble")
parseNumber :: Parser' String
parseNumber =
    do
        input <- lift get
        case input of
            [] -> throwE ""
            (c:str) ->
                do
                    x <- lift (runExceptT parseChar)
                    case x of
                        Right t ->
                            if C.isDigit t
                                then
                                    do 
                                        result <- lift (runExceptT parseNumber)
                                        case result of
                                            Right rest -> return (t : rest)       
                                            Left _ -> return [t] 
                            else throwE "not a digit"
                        Left err -> throwE err

-- >>> parse (parseNumber') "123 ble"
-- (Right 123," ble")
parseNumber' :: Parser' Integer
parseNumber' =
    do
        x <- parseNumber
        return (read x)




parseWord :: Parser' String
parseWord =
    do 
        input <- lift get
        case input of
            [] -> return "empty string"
            str ->
                do
                    word <- lift $ runExceptT parseString'
                    case word of
                        Left e -> throwE e
                        Right word -> return word


parseWorkoutName :: Parser' WorkoutName
parseWorkoutName = 
    do
        word <- lift $ runExceptT parseWord
        case word of
            Left e -> throwE e
            Right word' ->
                case word' of 
                    "Strength" -> return Strength
                    "Flexibility" -> return Flexibility
                    "Cardio" -> return Cardio
                    "Conditioning" -> return Conditioning
                    "Calisthenics" -> return Calisthenics
                    "MartialArts" -> return MartialArts
                    _ -> throwE "nera tokio workout"
            Left e -> throwE e

parseIntensity :: Parser' Intensity
parseIntensity = 
    do
        word <- lift $ runExceptT parseWord
        case word of
            Left e -> throwE e
            Right word' ->
                case word' of 
                    "Low" -> return Low 
                    "Medium" -> return Medium
                    "High" -> return High                    
                    _ -> throwE "nera tokio intensity"
            Left e -> throwE e



parseMealName :: Parser' MealName
parseMealName = 
    do
        word <- lift $ runExceptT parseWord
        case word of
            Left e -> throwE e
            Right word' ->
                case word' of 
                    "Main" -> return Main
                    "Appetizer" -> return Appetizer
                    "Dessert" -> return Dessert
                    "Snack" -> return Snack
                    _ -> throwE "nera tokio workout"
            Left e -> throwE e

and3_0 :: (a -> b -> c -> d) -> Parser' a -> Parser' b -> Parser' c -> Parser' d
and3_0 f p1 p2 p3 =
    do 
        a1 <-  p1
        b1 <-  p2
        c1 <-  p3
        return (f a1 b1 c1 )
and10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Parser' a -> Parser' b -> Parser' c -> Parser' d -> Parser' e -> Parser' f -> Parser' g -> Parser' h -> Parser' i -> Parser' j -> Parser' k

and10 func p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 = 
    do
        a1 <- p1
        b1 <- p2
        c1 <- p3
        d1 <- p4
        e1 <- p5
        f1 <- p6
        g1 <- p7
        h1 <- p8
        i1 <- p9
        j1 <- p10
        return (func a1 b1 c1 d1 e1 f1 g1 h1 i1 j1)

parseWorkout :: Parser' Workout

parseWorkout =
    do
        input <- lift get
        case input of
            [] -> throwE []
            str ->
                do
                    workout <- and10 (\a _ b _ _ _ c _ _ _ -> Workout a b c) parseWorkoutName (parseChar' ' ') parseIntensity (parseChar' ' ') (parseChar' '{') (parseChar' ' ') parseWorkout'' (parseChar' ' ') (parseChar' '}') (parseChar' ' ')
                    return workout

-- >>> parse (parseWorkout') "Cardio Low { Calisthenics High {  } Cardio Medium {  }  } Cardio High {  } "
-- (Right [Workout Cardio Low [Workout Calisthenics High [],Workout Cardio Medium []],Workout Cardio High []],"")




parseWorkout' :: Parser' [Workout]
parseWorkout' = 
    do
        input <- lift get
        case input of
            [] -> throwE ""
            str ->
                do
                    workout <- parseWorkout
                    input' <- lift get
                    workouts <- lift (runExceptT parseWorkout')
                    case workouts of
                        Right workouts' ->
                            return (workout : workouts')
                        Left e -> 
                            do
                                _ <- lift $ put input'
                                return ([workout])





parseWorkout'' :: Parser' [Workout]
parseWorkout'' = 
    do
        input <- lift get
        case input of
            [] -> throwE ""
            str ->
                do 
                    workouts <- lift (runExceptT parseWorkout')
                    case workouts of
                        Right workouts' -> return workouts'
                        Left e ->
                            do
                                _ <- lift $ put input
                                input' <- lift (runExceptT $ and3_0 (\ _ _ _ -> ()) (parseChar' ' ') (parseChar' '}') (parseChar' ' ') ) 
                                case input' of
                                    Right _ ->
                                        do 
                                            _ <- lift $ put input 
                                            return [] 
                                    Left e -> 
                                        do
                                            _ <- lift $ put input
                                            throwE e 



parseMeal :: Parser' Meal
parseMeal =
    do
        input <- lift get
        case input of
            [] -> throwE []
            str ->
                do
                    meal <- and10 (\a _ b _ _ _ c _ _ _ -> Meal a b c) parseMealName (parseChar' ' ') parseNumber' (parseChar' ' ') (parseChar' '{') (parseChar' ' ') parseMeal'' (parseChar' ' ') (parseChar' '}') (parseChar' ' ')
                    return meal

-- >>> parse (parseMeal') "Main 200 { Snack 0 {  } Dessert 0 {  }  } Main 0 {  }"
-- (Right [Meal Main 200 [Meal Snack 0 [],Meal Dessert 0 []],Meal Main 0 []],"k")
parseMeal' :: Parser' [Meal]
parseMeal' = 
    do
        input <- lift get
        case input of
            [] -> throwE ""
            str ->
                do
                    meal <- parseMeal
                    input' <- lift get
                    meals <- lift (runExceptT parseMeal')
                    case meals of
                        Right meals' ->
                            return (meal : meals')
                        Left e -> 
                            do
                                _ <- lift $ put input' 
                                return ([meal])





parseMeal'' :: Parser' [Meal]
parseMeal'' = 
    do
        input <- lift get
        case input of
            [] -> throwE ""
            str ->
                do 
                    meals <- lift (runExceptT parseMeal')
                    case meals of
                        Right meals' -> return meals'
                        Left e ->
                            do
                                _ <- lift $ put input
                                input' <- lift (runExceptT $ and3_0 (\ _ _ _ -> ()) (parseChar' ' ') (parseChar' '}') (parseChar' ' ') ) 
                                case input' of
                                    Right _ ->
                                        do 
                                            _ <- lift $ put input 
                                            return [] 
                                    Left e -> 
                                        do
                                            _ <- lift $ put input 
                                            throwE e 



-- >>> parse (parseFitnessApp) "FitnessApp whatevz Cardio Low {  } Main 200 {  }"
-- (Left "Encountered a whitespace"," 200 {  }")

-- >>> parse (parseMeal') "Main 100 {  }  h"
-- (Right [Meal Main 100 []]," h")


-- >>> parse (parseWorkout') "Cardio Low {  } k"
-- (Right [Workout Cardio Low []],"")
parseFitnessApp :: Parser' Query
parseFitnessApp =
    do
        input <- lift get
        word <- parseWord
        if( word == "FitnessApp" )
            then
                do
                    _ <- parseChar' ' '
                    name <- parseWord
                    _ <- parseChar' ' '
                    w <- parseWorkout'
                    m <- parseMeal'
                    return(FitnessApp name w m)
        else 
            
             (lift $ put input) >>
             throwE "no such parser"




parseAddWorkout :: Parser' Query
parseAddWorkout =
    do
        input <- lift get
        word <- parseWord
        if( word == "AddWorkout" )
            then
                do
                    _ <- parseChar' ' '
                    w <- parseWorkout'
                    return(AddWorkout w )
        else 
            
             (lift $ put input) >>
             throwE "no such parser"

parseAddMeal :: Parser' Query
parseAddMeal =
    do
        input <- lift get
        word <- parseWord
        if( word == "AddMeal" )
            then
                do
                    
                    _ <- parseChar' ' '
                    m <- parseMeal'
                    return(AddMeal m )
        else 
            
             (lift $ put input) >>
             throwE "no such parser"

parseListState :: Parser' Query
parseListState =
    do
        input <- lift get
        word <- parseWord
        if( word == "ListState" )
            then
                do
                    input' <- lift get
                    if (input' == "")
                        then 
                            return ListState
                    else
                        do
                            parseChar' ' ' >> return ListState
        else 
            
             (lift $ put input) >>
             throwE "no such parser"

parseRemoveMeal :: Parser' Query
parseRemoveMeal =
    do
        input <- lift get
        word <- parseWord
        if( word == "RemoveMeal" )
            then
                do
                    input' <- lift get
                    if (input' == "")
                        then 
                            return RemoveMeal
                    else
                        do
                            parseChar' ' ' >> return RemoveMeal
        else 
            
             (lift $ put input) >>
             throwE "no such parser"

parseRemoveWorkout :: Parser' Query
parseRemoveWorkout =
    do
        input <- lift get
        word <- parseWord
        if( word == "RemoveWorkout" )
            then
                do
                    input' <- lift get
                    if (input' == "")
                        then 
                            return RemoveWorkout
                    else
                        do
                            parseChar' ' ' >> return RemoveWorkout
        else             
             (lift $ put input) >>
             throwE "no such parser"

parseCalories :: Parser' Query
parseCalories =
    do
        input <- lift get
        word <- parseWord
        if( word == "TotalCalories" )
            then
                do
                    input' <- lift get
                    if (input' == "")
                        then 
                            return TotalCalories
                    else
                        do
                            parseChar' ' ' >> return TotalCalories
        else             
             (lift $ put input) >>
             throwE "no such parser"















data State' = State
  { appName  :: String
  , workouts :: [Workout]
  , meals    :: [Meal]
  } deriving (Show, Eq)

emptyState :: State'
emptyState = State
    { appName  = "MyFitnessApp"
     , workouts = []
     , meals    = []
    }


sumMealCalories :: Meal -> Integer
sumMealCalories (Meal _ calories subMeals) = calories + sum (map sumMealCalories subMeals)


sumAllMealCalories :: State' -> Integer
sumAllMealCalories st = sum (map sumMealCalories (meals st))


stateTransition :: State' -> Query -> Either String (Maybe String, State')
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
