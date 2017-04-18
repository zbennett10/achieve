--API
port module Achieve exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode
import Date exposing (..)
import DatePicker exposing (defaultSettings)
import Date.Extra.Format as DateFormat exposing (format)
import Date.Extra.Config.Config_en_us as DateConfig exposing (config)

--Elm Bootstrap
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox


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
        deadline : Date
    }

type alias Model =
    {
        score : Int,
        goals : List Goal,
        currentGoalName : String,
        currentGoalScore : String,
        currentDeadline : Date,
        datePicker: DatePicker.DatePicker
    }

--INIT
initialGoalDeadline = Date.fromString "October 11, 1991" |> Result.withDefault (Date.fromTime 0)
init : (Model, Cmd Msg)
init = 
    let
        (datePicker, datePickerFx ) =
            DatePicker.init defaultSettings
    in
        { 
            score = 1000,
            goals = [Goal 1 "Love Kalie Forever" "100" False initialGoalDeadline],
            currentGoalName = "",
            currentGoalScore = "",
            currentDeadline =  Date.fromTime 0,
            datePicker = datePicker
        }
            ! [ Cmd.map ToDatePicker datePickerFx ]

--ACTION TYPES--
type Msg = NoOp
    | AddGoal String String String
    | ToggleGoalComplete Int Bool
    | UpdateGoalName Int String
    | ToggleScore Goal
    | ChangeCurrentGoalName String
    | ChangeCurrentGoalScore String
    | ChangeCurrentDeadline String
    | ToDatePicker DatePicker.Msg

--UPDATE--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        AddGoal name score deadline ->
            ({ model | goals = model.goals ++ [Goal (createNewID (findMaxID model.goals)) name score False (stringToDate deadline)] }, Cmd.none)
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
            ({ model | currentDeadline = (stringToDate dateString) }, Cmd.none)
        ToDatePicker msg ->
            let
              ( newDatePicker, datePickerFx, maybeNewDate ) =
                DatePicker.update msg model.datePicker

              date =
                case maybeNewDate of
                    Nothing ->
                       model.currentDeadline
                    Just date ->
                       date
            in
                { model | currentDeadline = date, datePicker = newDatePicker} ! [Cmd.map ToDatePicker datePickerFx]
              

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
    Grid.container []
        [ 
            div [ class "jumbotron text-center" ] 
                [ 
                    h1 [ class "large-cursive-title" ] [ text "Achieve." ]
                ],
            Grid.row []
                [ Grid.col []
                    [
                        h1 [ class "text-center" ] [ text "Overall Score" ],
                        h2 [ class "text-center" ] [ text (toString model.score) ]  
                    ]
                ],
            Grid.row []
            [ 
                Grid.col []
                    [ 
                        h1 [ class "text-center" ] [ text "New Goal: " ],
                        Form.form []
                            [ Form.group []
                                [ 
                                    Form.label [ for "goalNameInput" ] [ text "Goal: " ],
                                    Input.text [ Input.attrs [id "goalNameInput", onInput ChangeCurrentGoalName ] ],
                                    Form.label [ for "goalScoreInput" ] [ text "Goal Value: " ],
                                    Input.text [ Input.attrs [ id "goalScoreInput", onInput ChangeCurrentGoalScore  ] ],
                                    Form.label [ for "deadlineInput"] [ text "Select a goal deadline: " ],
                                    DatePicker.view model.datePicker
                                    |> Html.map ToDatePicker,
                                    Button.button [Button.primary, Button.attrs [ onWithOptions "click" (Options False True) (Decode.succeed (AddGoal model.currentGoalName model.currentGoalScore (dateToString model.currentDeadline))) ] ] [text "Submit"] 
                                ]
                            ]
                    ],
                Grid.col []
                    [
                        h1 [ class "text-center" ] [ text "Upcoming Goals" ],
                        renderGoals model.goals
                    ]
            ],
            Grid.row []
            [
                Grid.col []
                [
                    h1 [ class "text-center" ] [ text (dateToString model.currentDeadline) ]
                ]
            ]
        ]


--PORTS
port localStorageSend : Json.Encode.Value -> Cmd msg


renderGoals : List Goal -> Html Msg
renderGoals goals =
    ListGroup.ul
        (List.map 
                (\goal -> 
                    ListGroup.li [ ListGroup.attrs [class "text-center"] ] 
                    [ 
                        Checkbox.checkbox [Checkbox.checked goal.completed, Checkbox.inline, Checkbox.success, Checkbox.attrs [class "list-checkbox", onClick (ToggleScore goal)] ] "",
                        Form.label [] 
                        [
                                h2 [] [ text goal.name ],  
                                h3 [] [ text goal.value ],
                                h4 [] [ text (dateToString goal.deadline) ] --do this!!!!!!!!! 
                        ]
                    ]
                ) 
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



stringToDate : String -> Date
stringToDate dateString = 
    Date.fromString dateString |> Result.withDefault (Date.fromTime 0)


dateToString : Date -> String
dateToString date =
    DateFormat.format DateConfig.config "%d-%b-%Y" date

--encode list of goals for local storage
encodeGoals : Model -> Json.Encode.Value
encodeGoals model =
    Json.Encode.object
        [
            ("achieve_goals", Json.Encode.list (List.map encodeGoal model.goals))
        ]

--encode an individual goal for local storage
encodeGoal : Goal -> Json.Encode.Value
encodeGoal goal =
    Json.Encode.object
        [
            ("id", Json.Encode.int goal.id),
            ("name", Json.Encode.string goal.name),
            ("value", Json.Encode.string goal.value),
            ("completed", Json.Encode.bool goal.completed),
            ("deadline", Json.Encode.string (dateToString goal.deadline))
        ]