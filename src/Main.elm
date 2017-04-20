--API
port module Achieve exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode
import Date exposing (..)
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

--expecting null when program reads from localStorage - set program to be aware of flags?


--MAIN--
main : Program (Maybe Model) Model Msg
main =
    programWithFlags
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
initialModel = Model 1000 [Goal 1 "Love Kalie Forever" "100" False "October 11, 1991"] "" "" "October 11, 1991"
      
          
init : Maybe Model -> (Model, Cmd Msg)
init model =
    case model of
        Just model ->
            (model, Cmd.none)
        Nothing ->
            (initialModel, Cmd.none)
        

--ACTION TYPES--
type Msg = NoOp
    | AddGoal String String String
    | ToggleGoalComplete Int Bool
    | UpdateGoalName Int String
    | ToggleScore Goal
    | ChangeCurrentGoalName String
    | ChangeCurrentGoalScore String
    | ChangeCurrentDeadline String
    | SetModel Model

--UPDATE--

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        AddGoal name score deadline ->
            ({ model | goals = model.goals ++ [Goal (createNewID (findMaxID model.goals)) name score False deadline] }, sendModelToStorage model)
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
                ({ model | goals = newGoals }, sendModelToStorage model)
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
                ({ model | goals = newGoals }, sendModelToStorage model)
              
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
        
        SetModel newModel ->
            ( newModel, Cmd.none )
              

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
                                    Input.date [ Input.attrs [id "deadlineInput", onInput ChangeCurrentDeadline ]],
                                    Button.button [Button.primary, Button.attrs [ onWithOptions "click" (Options False True) (Decode.succeed (AddGoal model.currentGoalName model.currentGoalScore model.currentDeadline)) ] ] [text "Submit"] 
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
                    h1 [ class "text-center" ] [ text model.currentDeadline ]
                ]
            ]
        ]


--PORTS
port localStorageSend : Json.Encode.Value -> Cmd msg
port localStorageInput : Json.Encode.Value -> Cmd msg


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
                                h3 [] [ text goal.name ],  
                                h4 [] [ text goal.value ],
                                h5 [] [ text goal.deadline ] --do this!!!!!!!!! 
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


sendModelToStorage : Model -> Cmd Msg
sendModelToStorage model =
    localStorageSend (encodeModel model)

encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [
            ("score", Json.Encode.int model.score),
            ("goals", Json.Encode.list (List.map encodeGoal model.goals)),
            ("currentGoalName", Json.Encode.string model.currentGoalName),
            ("currentGoalScore", Json.Encode.string model.currentGoalScore),
            ("currentDeadline", Json.Encode.string model.currentDeadline) 
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
            ("deadline", Json.Encode.string goal.deadline)
        ]

decodeModel : Decode.Value -> Result String Model
decodeModel modelJson = 
    Decode.decodeValue modelDecoder modelJson

modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map5 Model
        (Decode.field "score" Decode.int)
        (Decode.field "goals" (Decode.list goalDecoder))
        (Decode.field "currentGoalName" Decode.string)
        (Decode.field "currentGoalScore" Decode.string)
        (Decode.field "currentDeadline" Decode.string)

goalDecoder : Decode.Decoder Goal
goalDecoder =
    Decode.map5 Goal
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.string)
        (Decode.field "completed" Decode.bool)
        (Decode.field "deadline" Decode.string)

mapLocalStorageInput : Decode.Value -> Msg
mapLocalStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            SetModel model
        
        Err message ->
            let
                _ =
                    Debug.log message
            in
                NoOp