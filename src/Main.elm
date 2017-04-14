import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Date exposing (..)
import Task exposing (..)


--MAIN--
main : Program Never Model Msg
main =
    program
        { 
            init = init,
            view  = view,
            update = update,
            subscriptions = subscriptions
        }

-- MODEL--
type alias Goal =
    { 
        id : Int,
        name : String,
        value : String, 
        completed : Bool,
        deadline : String
    }

type alias Model =
    {
        score : Int,
        goals : List Goal,
        currentGoalName : String,
        currentGoalScore : String,
        currentDeadline : String
    }

--INIT
init : (Model, Cmd Msg)
init = 
    ({ 
        score = 1000,
        goals = [Goal 1 "Love Kalie Forever" "100" False "October 11, 2017"],
        currentGoalName = "",
        currentGoalScore = "",
        currentDeadline =  "October 11, 2017"
    }, Cmd.none)

--ACTION TYPES--
type Msg = NoOp
    | AddGoal String String String
    | ToggleGoalComplete Int Bool
    | UpdateGoalName Int String
    | ToggleScore Goal
    | ChangeCurrentGoalName String
    | ChangeCurrentGoalScore String
    | ChangeCurrentDeadline String

--UPDATE--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        AddGoal name score deadline ->
            ({ model | goals = model.goals ++ [Goal (createNewID (findMaxID model.goals)) name score False deadline] }, Cmd.none)
        ToggleGoalComplete id status ->
            let
              newGoals =
                List.map
                    (\goal ->
                        if goal.id == id then
                            { goal | completed = status }
                        else
                            goal
                    )
                    model.goals
            in
                ({ model | goals = newGoals }, Cmd.none)
        UpdateGoalName id newName ->
            let
              newGoals =
                List.map
                    (\goal ->
                        if goal.id == id then
                            { goal | name = newName }
                        else
                            goal
                    )
                    model.goals
            in
                ({ model | goals = newGoals }, Cmd.none)
              
        ToggleScore goal -> 
            if goal.completed == False then
                { model | score = model.score + Result.withDefault 0 (String.toInt goal.value) } --update model score and recursively update goal complete
                |> update (ToggleGoalComplete goal.id True)
            else
                { model | score = model.score - Result.withDefault 0 (String.toInt goal.value) }
                |> update (ToggleGoalComplete goal.id False)

        ChangeCurrentGoalName name ->
            ({ model | currentGoalName = name }, Cmd.none)
        ChangeCurrentGoalScore score ->
            ({ model | currentGoalScore = score }, Cmd.none)
        ChangeCurrentDeadline dateString ->
            ({ model | currentDeadline = dateString }, Cmd.none)

--SUBSCRIPTIONS--
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW--

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
                    h1 [ class "large-cursive-title" ] [ text "Achieve." ]
                ],
            div [ class "row" ]
            [ 
                div [ class "col-lg-4 col-md-4" ]
                    [ 
                        h1 [ class "text-center" ] [ text "New Goal: " ],
                        Html.form [ class "form-group" ]
                            [ 
                                label [ for "goalNameInput" ] [ text "Goal: " ],
                                input [ id "goalNameInput", class "form-control", onInput ChangeCurrentGoalName ] [],
                                label [ for "goalScoreInput" ] [ text "Goal Value: " ],
                                input [ id "goalScoreInput", class "form-control", onInput ChangeCurrentGoalScore  ] [],
                                label [ for "deadlineInput"] [ text "Deadline: " ],
                                input [ id "deadlineInput", class "form-control", type_ "date", onInput ChangeCurrentDeadline ] [],
                                button [class "btn btn-md btn primary", onWithOptions "click" (Options False True) (Json.succeed (AddGoal model.currentGoalName model.currentGoalScore model.currentDeadline)) ] [text "Submit"] 
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
            ],
            div [class "row" ]
            [
                div [ class "container" ]
                [
                    h1 [ class "center-text large-cursive-title" ] [ text model.currentDeadline ]
                ]
            ]
        ]


renderGoals : List Goal -> Html Msg
renderGoals goals =
    ul [class "list-unstyled text-center"]
        (List.map 
                (\goal -> 
                    li [ class "list-item" ] 
                    [ 
                        label [] 
                        [ 
                            input [class "form-control", type_ "checkbox", onClick (ToggleScore goal) ] [], 
                            text ((toString goal.name) ++ " - "), text goal.value 
                        ] 
                    ]) 
        goals)


--HELPERS--

updateGoalComplete : Goal -> Bool -> Goal
updateGoalComplete goal value =
    {goal | completed = value }


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


        
