import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json


main : Program Never Model Msg
main =
    Html.beginnerProgram
     { 
         model = model,
         view  = view,
         update = update
     }


-- Model
type alias Goal =
    { 
        id : Int,
        name : String,
        value : String, 
        completed : Bool
    }

type alias Model =
    {
        score : Int,
        goals : List Goal,
        currentGoalName : String,
        currentGoalScore : String
    }

type alias RecordWithID =
    {
        id : Int
    }

model : Model
model = 
    { 
        score = 1000,
        goals = [Goal 1 "Love Kalie Forever" "100" False],
        currentGoalName = "",
        currentGoalScore = ""
    }


--Update
type Msg =
   AddGoal String String
   | CompleteGoal Int
   | AddScore Goal
   | SendGoalName String
   | SendGoalScore String


update : Msg -> Model -> Model
update msg model = 
    case msg of
        AddGoal name score ->
            { model | goals = model.goals ++ [Goal (createNewID (findMaxID model.goals)) name score False] }

        CompleteGoal id ->
            let
              newGoals =
                List.map
                    (\goal ->
                        if goal.id == id then
                            { goal | completed = True }
                        else
                            goal
                    )
                    model.goals
            in
                { model | goals = newGoals }
              

        AddScore goal ->
            if goal.completed == False then
                { model | score = model.score + Result.withDefault 0 (String.toInt goal.value) }
            else
                model

        SendGoalName name ->
            { model | currentGoalName = name }

        SendGoalScore score ->
            { model | currentGoalScore = score }


            

--view

type alias Options =
    {
        stopPropagation : Bool,
        preventDefault : Bool
    }

view : Model -> Html Msg
view model = 
    div [ class "container" ]
        [ 
            div [ class "jumbotron text-center" ] 
                [ 
                    h1 [] [ text "Achieve" ]
                ],
            div [ class "row" ]
            [ 
                div [ class "col-lg-4 col-md-4" ]
                    [ 
                        h1 [ class "text=center" ] [ text "New Goal: " ],
                        Html.form [ class "form-group" ]
                            [ 
                                label [ for "goalNameInput" ] [ text "Goal: " ],
                                input [ id "goalNameInput", class "form-control", onInput SendGoalName ] [],
                                label [ for "goalScoreInput" ] [ text "Goal Value: " ],
                                input [ id "goalScoreInput", class "form-control", onInput SendGoalScore  ] [],
                                button [class "btn btn-md btn primary", onWithOptions "click" (Options False True) (Json.succeed (AddGoal model.currentGoalName model.currentGoalScore)) ] [text "Submit"] 
                            ]
                    ],
                div [class "col-lg-4 col-md-4"]
                    [
                        h1 [ class "text-center" ] [ text "My Goals" ],
                        renderGoals model.goals
                    ],
                div [ class "col-lg-4 col-mid-4" ]
                    [
                        div [class "row"]
                            [
                                h1 [ class "text-center" ] [ text "Score" ],
                                h2 [ class "text-center" ] [ text (toString model.score) ]  
                            ]
                    ]
            ]
        ]


--searches through a list of records and finds the max id present
findMaxID : List Goal -> Int
findMaxID records = 
    records
        |> List.map (.id)
        |> List.maximum
        |> Maybe.withDefault -1

--create a new id
createNewID : Int -> Int
createNewID id = 
    id + 1


renderGoals : List Goal -> Html Msg
renderGoals goals =
    ul [class "list-unstyled text-center"]
        (List.map 
                (\goal -> 
                    li [ class "list-item" ] 
                    [ 
                        label [] 
                        [ 
                            input [class "form-control", type_ "checkbox", onClick (AddScore goal) ] [], 
                            text ((toString goal.name) ++ " - "), text goal.value 
                        ] 
                    ]) 
        goals)
        
