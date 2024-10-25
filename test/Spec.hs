{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified
import Lib2 ( Workout(..), Meal(..),Query(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "Parsing FitnessApp query" $ 
      Lib2.parseQuery "FitnessApp whatevz Strenght High { Cardio Medium { } Calisthenics Low { } } Main 200 { Dessert 100 { } }"
      @?= Right (FitnessApp "whatevz" [Workout Lib2.Strength Lib2.High [Workout Lib2.Cardio Lib2.Medium [],Workout Lib2.Calisthenics Lib2.Low []]] [Meal Lib2.Main 200 [Meal Lib2.Dessert 100 []]]),

    testCase "Parsing AddWorkout query" $
      Lib2.parseQuery "AddWorkout Strenght High { }" 
      @?= Right (AddWorkout [Workout Lib2.Strength Lib2.High []]),

    testCase "Parsing AddMeal query" $
      Lib2.parseQuery "AddMeal Main 500 { }"
      @?= Right (Lib2.AddMeal [Lib2.Meal Lib2.Main 500 []]),

    testCase "Parsing ListState query" $
      Lib2.parseQuery "ListState"
      @?= Right Lib2.ListState,

    testCase "State transition - ListState" $
      Lib2.stateTransition Lib2.emptyState Lib2.ListState
      @?= Right (Just "Current state:\nState {appName = \"MyFitnessApp\", workouts = [], meals = []}",Lib2.State {Lib2.appName = "MyFitnessApp", Lib2.workouts = [], Lib2.meals = []}),

    testCase "State transition - Add workout to existing state" $
    case Lib2.stateTransition Lib2.emptyState (AddWorkout [Workout Lib2.Calisthenics Lib2.Low [Workout Lib2.Cardio Lib2.Medium []]]) of
          Right(_,newState) -> 
            Lib2.stateTransition newState (AddWorkout [Workout Lib2.Strength Lib2.High []])
            @?= Right (Just "Workout added!",Lib2.State {Lib2.appName = "MyFitnessApp", Lib2.workouts = [Workout Lib2.Strength Lib2.High [],Workout Lib2.Calisthenics Lib2.Low [Workout Lib2.Cardio Lib2.Medium []]], Lib2.meals = []})
          Left err -> error err,

    testCase "State transition - Add meal to existing state" $
    case Lib2.stateTransition Lib2.emptyState (AddMeal [Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]) of
          Right(_,newState) -> 
            Lib2.stateTransition newState (AddMeal [Meal Lib2.Snack 100 []])
            @?= Right (Just "Meal added!",Lib2.State {Lib2.appName = "MyFitnessApp", Lib2.workouts = [], Lib2.meals = [Meal Lib2.Snack 100 [],Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]})
          Left err -> error err,

    testCase "State transition - Add FitnessApp to state" $
    case Lib2.stateTransition Lib2.emptyState (Lib2.FitnessApp "whatevs" [Workout Lib2.Strength Lib2.High []] [Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]) of
          Right(_,newState) -> 
            Lib2.stateTransition newState (AddMeal [Meal Lib2.Snack 100 []])
            @?= Right (Just "Meal added!",Lib2.State {Lib2.appName = "whatevs", Lib2.workouts = [Workout Lib2.Strength Lib2.High []], Lib2.meals = [Meal Lib2.Snack 100 [],Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]})
          Left err -> error err
  ]