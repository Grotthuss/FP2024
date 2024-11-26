{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC
--import Test.QuickCheck
--import Test.QuickCheck.Arbitrary (nonEmpty)
import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib2 ( Workout(..), Meal(..),Query(..))
import Lib3 qualified
--import Lib3 (Statements(..))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as QC.Arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests,propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "Parsing FitnessApp query" $ 
      Lib2.parseQuery "FitnessApp whatevz Strength High { Cardio Medium {  } Calisthenics Low {  }  } Main 200 { Dessert 100 {  }  }"
      @?= Right (FitnessApp "whatevz" [Workout Lib2.Strength Lib2.High [Workout Lib2.Cardio Lib2.Medium [],Workout Lib2.Calisthenics Lib2.Low []]] [Meal Lib2.Main 200 [Meal Lib2.Dessert 100 []]],""),

    testCase "Parsing AddWorkout query" $
      Lib2.parseQuery "AddWorkout Strength High {  }" 
      @?= Right (AddWorkout [Workout Lib2.Strength Lib2.High []],""),

    testCase "Parsing AddMeal query" $
      Lib2.parseQuery "AddMeal Main 500 {  }"
      @?= Right (Lib2.AddMeal [Lib2.Meal Lib2.Main 500 []],""),

    testCase "Parsing ListState query" $
      Lib2.parseQuery "ListState"
      @?= Right (Lib2.ListState,""),

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
          Left err -> error err,

    testCase "State transition - Remove Workout" $
    case Lib2.stateTransition Lib2.emptyState (Lib2.FitnessApp "whatevs" [Workout Lib2.Strength Lib2.High []] [Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]) of
          Right(_,newState) -> 
            Lib2.stateTransition newState (RemoveWorkout)
            @?= Right (Just "Removed the first workout.",Lib2.State {Lib2.appName = "whatevs", Lib2.workouts = [], Lib2.meals = [Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]})
          Left err -> error err,
    testCase "State transition - Remove Meal" $
    case Lib2.stateTransition Lib2.emptyState (Lib2.FitnessApp "whatevs" [Workout Lib2.Strength Lib2.High []] [Meal Lib2.Main 1000 [Meal Lib2.Dessert 300 []]]) of
          Right(_,newState) -> 
            Lib2.stateTransition newState (RemoveMeal)
            @?= Right (Just "Removed the first meal.",Lib2.State {Lib2.appName = "whatevs", Lib2.workouts = [Workout Lib2.Strength Lib2.High []], Lib2.meals = []})
          Left err -> error err
    
  ]

instance Arbitrary Lib2.Intensity where
  arbitrary = elements [Lib2.Low, Lib2.Medium, Lib2.High]

instance Arbitrary Lib2.WorkoutName where
  arbitrary = elements [Lib2.Strength, Lib2.Flexibility, Lib2.Cardio, Lib2.Conditioning, Lib2.Calisthenics, Lib2.MartialArts]

instance Arbitrary Lib2.MealName where
  arbitrary = elements [Lib2.Main, Lib2.Appetizer, Lib2.Dessert, Lib2.Snack]

instance Arbitrary Lib2.Workout where
  arbitrary = sized $ \size ->
    if size <= 0
      then Workout <$> arbitrary <*> arbitrary <*> pure []  -- No nested workouts if size is 0
      else Workout <$> arbitrary <*> arbitrary <*> resize (size `div` 2) arbitrary 

instance Arbitrary Lib2.Meal where
  arbitrary = sized $ \size -> 
    if size <= 0 
      then Meal <$> arbitrary <*> choose (0, 1000) <*> pure []  -- No nested meals if size is 0
      else Meal <$> arbitrary <*> choose (0, 1000) <*> resize (size `div` 2) arbitrary 

nonEmptyString :: Gen String
nonEmptyString = listOf1 (elements ['a'..'z'])

instance Arbitrary Lib2.Query where
  arbitrary = oneof [
      pure RemoveMeal,
      pure RemoveWorkout,
      FitnessApp <$> nonEmptyString <*> listOf1 arbitrary <*> listOf1 arbitrary
    ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof [Lib3.Single <$> arbitrary, Lib3.Batch <$> listOf1 arbitrary]











propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $ QC.withMaxSuccess 10 $
      \list -> sort (list :: [Int]) == sort (reverse list),
   
    QC.testProperty "Parsing rendered statements is identity" $ QC.withMaxSuccess 10 $
      \statements ->
        Lib3.parseStatements (Lib3.renderStatements statements) == Right (statements, "")

  ]

-- Helper functions to calculate the depth of a Meal
maxDepthMeal :: Meal -> Int
maxDepthMeal (Meal _ _ subMeals) = 
  0 + maximum (0 : map maxDepthMeal subMeals)

-- Helper functions to calculate the depth of a Workout
maxDepthWorkout :: Workout -> Int
maxDepthWorkout (Workout _ _ subWorkouts) = 
  0 + maximum (0 : map maxDepthWorkout subWorkouts)