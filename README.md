

# Fitness domain
Project is focused on making meal and workout plans

# Main Entities
1. fitnessApp: represents the whole fitness routine.(Workouts,Intensity,Meals)

    * Name of the plan
    * Meal
    * What kind of workout
    * How long will the workout be
2. Meal: will be a string
    * String
3. WorkoutName: will be a workout type
    * Strenght: for strong body
    * Flexibility : so that you will not get muscle pain or tightness
    * Cardio: for a healthy heart
    * Conditioning: so you can go longer time under pressure
    * Calisthenics: for agile body
    * Martial arts: for self defense

4. Intensity: Low, Medium, High
```
<fitnessApp> ::= "myFitness " <Name> " " <workouts> " | " <meals>

<workouts> ::= <workout> | <workout> <workouts> | " "
<workout> ::= <workoutName> " " <intensity> " " "{ " <subWorkouts> " } "
<workoutName> ::= "Strenght" | "Flexibility" | "Cardio" | "Conditioning" | "Calisthenics" | "Martial_arts"
<intensity> ::= "Low" | "Medium" | "High"
<subWorkouts> ::= <workout> | <workout> <subWorkouts> | " "

<addWorkout> ::= "addWorkout " <workout> 


<meals> ::= <meal> | <meal> <meals> | " "
<meal> ::= <mealName> " " <calories> " " "{ " <subMeals> " } "
<mealName> ::= "Main" | "Apetiser" | "Dessert" | "Snack"
<calories> ::= <integer>
<subMeals> ::= <meal> | <meal> <subMeals> | " "
<addMeal> ::= "addMeal " <meal>

<Name> ::= <string>
<string> ::= <letter> | <letter> <string>
<integer> ::= <number> | <number> <integer>
<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
<number> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

```
myFitness q Cardio Medium { Conditioning Medium { Martial arts Medium {   }  } Calisthenics Low {   }  }  Main 495 { Dessert 4 { Dessert 5 {   }   }  } 