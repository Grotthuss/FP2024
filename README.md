# fp-2024

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
<fitnessApp> ::= "myFitness" <Name> <workouts> <meals>

<workouts> ::= "workout" <workout> | "workout" <workout> <workouts> | ""
<workout> ::= <workoutName> <intensity> "{" <subWorkouts> "}"
<workoutName> ::= "Strenght" | "Flexibility" | "Cardio" | "Conditioning" | "Calisthenics" | "Martial arts"
<intensity> ::= "Low" | "Medium" | "High"
<subWorkouts> ::= <workout> | <workout> <subWorkouts> | ""

<meals> ::= "meal" <meal> | "meal" <meal> <meals> | ""
<meal> ::= <mealName> <calories> "{" <subMeals> "}"
<mealName> ::= <string>
<calories> ::= <integer>
<subMeals> ::= <meal> | <meal> <subMeals> | ""

<Name> ::= <string>
<string> ::= <letter> | <letter> <string>
<integer> ::= <number> | <number> <integer>
<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<number> ::= "0" | "1" | ... | "9"

```


pvz: myFitness RytineTreniruote workout Strenght Medium { Cardio Medium {""} } meal pusryciai 700 {uzkandis 100 {""}}