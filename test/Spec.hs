{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
   
    testCase "Parsing FitnessApp query" $ 
      Lib2.parseQuery "FitnessApp MyApp Strength High { Cardio Medium { } } Main 500 { Dessert 300 { } }"
      @?= Right (FitnessApp "MyApp" [Workout Strength High [Workout Cardio Medium []]] [Meal Main 500 [Meal Dessert 300 []]]),

    testCase "Parsing AddWorkout query" $
      Lib2.parseQuery "AddWorkout Strength High { }"
      @?= Right (AddWorkout [Workout Strength High []]),

    testCase "Parsing AddMeal query" $
      Lib2.parseQuery "AddMeal Main 500 { }"
      @?= Right (AddMeal [Meal Main 500 []]),

    testCase "Parsing ListState query" $
      Lib2.parseQuery "ListState"
      @?= Right ListState,

    testCase "Parsing invalid query" $
      Lib2.parseQuery "InvalidCommand"
      @?= Left "Failed to parse query: Unknown command",

    testCase "State transition - AddWorkout" $
      Lib2.stateTransition Lib2.emptyState (AddWorkout [Workout Strength High []])
      @?= Right (Just "Workout added!", State "MyFitnessApp" [Workout Strength High []] []),

    testCase "State transition - AddMeal" $
      Lib2.stateTransition Lib2.emptyState (AddMeal [Meal Main 500 []])
      @?= Right (Just "Meal added!", State "MyFitnessApp" [] [Meal Main 500 []]),

    testCase "State transition - FitnessApp" $
      Lib2.stateTransition Lib2.emptyState (FitnessApp "MyApp" [Workout Cardio Medium []] [Meal Snack 200 []])
      @?= Right (Just "FitnessApp state updated!", State "MyApp" [Workout Cardio Medium []] [Meal Snack 200 []]),

    testCase "State transition - ListState" $
      Lib2.stateTransition Lib2.emptyState ListState
      @?= Right (Just "Current state:\nState {appName = \"MyFitnessApp\", workouts = [], meals = []}", Lib2.emptyState)
  ]