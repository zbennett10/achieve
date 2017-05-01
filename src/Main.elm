--API
port module Achieve exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Decode.Extra exposing (fromResult)
import Json.Encode
import Date exposing (..)
import Date.Extra.Format as DateFormat exposing (format)
import Date.Extra.Config.Config_en_us as DateConfig exposing (config)
import Time exposing (..)
import List.Extra exposing (..)
import Json.Helpers exposing (..)

--Elm Bootstrap
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Modal as Modal


--sort goals by date
--add place on app that contains completed goals





--MAIN--
main : Program Decode.Value Model Msg
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

type alias Options =
    {
        stopPropagation : Bool,
        preventDefault : Bool
    }

type alias Model =
    {
        score : Int,
        goals : List Goal,
        currentGoalName : String,
        currentGoalScore : String,
        currentDeadline : String,
        modalState: Modal.State,
        currentEditGoal : Goal
    }

--INIT
emptyGoal : Goal
emptyGoal = Goal 0 "" "" False ""

initialModel : Model
initialModel = Model 1000 [Goal 1 "Love Kalie Forever" "100" False "October 11, 1991"] "" "" "October 11, 1991" Modal.hiddenState emptyGoal
      
          
init : Decode.Value -> (Model, Cmd Msg)
init modelJson =
        mapLocalStorageInput modelJson
        

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
    | DeleteGoal Int
    | EditGoal Int
    | EditModalMsg Modal.State
    | PopulateEditModal Goal
    | SetCurrentEditGoal Goal

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

        DeleteGoal id -> 
            let
                goal =
                    findGoalByID model.goals id

                goalIndex =
                    findGoalIndex model.goals id
            in
                ({model | goals = removeAt goalIndex model.goals }, Cmd.none)

        EditGoal id ->
            let
                goal =
                    Goal id model.currentGoalName model.currentGoalScore False model.currentDeadline

                goalIndex =
                    findGoalIndex model.goals id

                newGoals =
                    updateGoalAtIndex model.goals goalIndex goal
    
            in
                {model | goals = newGoals }
                |> update (EditModalMsg Modal.hiddenState)

        EditModalMsg state->
            ( { model | modalState = state }, sendModelToStorage model )

        PopulateEditModal goal ->
            ( { model | currentEditGoal = goal }, sendModelToStorage model )

        SetCurrentEditGoal goal ->
            { model | currentEditGoal = findGoalByID model.goals goal.id }
            |> update (EditModalMsg Modal.visibleState)


        
              

--SUBSCRIPTIONS--
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW--

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
            ],
            Modal.config EditModalMsg
            |> Modal.small
            |> Modal.h1 [] [ text "Edit Goal" ]
            |> Modal.body [] 
                [ 
                  h2 [] [ text model.currentEditGoal.name],
                  Form.form []
                            [ Form.group []
                                [ 
                                    Form.label [ for "goalNameInput" ] [ text "Name: " ],
                                    Input.text [ Input.attrs [id "goalNameInput", onInput ChangeCurrentGoalName ] ],
                                    Form.label [ for "goalScoreInput" ] [ text "Goal Value: " ],
                                    Input.text [ Input.attrs [ id "goalScoreInput", onInput ChangeCurrentGoalScore  ] ],
                                    Form.label [ for "deadlineInput"] [ text "Select a goal deadline: " ],
                                    Input.date [ Input.attrs [id "deadlineInput", onInput ChangeCurrentDeadline ]],
                                    Button.button [Button.primary, Button.attrs [ onWithOptions "click" (Options False True) (Decode.succeed (EditGoal model.currentEditGoal.id)) ] ] [text "Submit"] 
                                ]
                            ]

                
                ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick (EditModalMsg Modal.hiddenState)]
                    ]
                    [ text "Close" ]
                ]
            |> Modal.view model.modalState
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
                                h5 [] [ text goal.deadline ], --do this!!!!!!!!!
                                Button.button [Button.danger, Button.attrs [onClick (DeleteGoal goal.id)] ] [ text "Remove" ],
                                Button.button [Button.success, Button.attrs [ id (toString goal.id), onClick (SetCurrentEditGoal goal) ] ] [ text "Edit" ]
                                --button that opens model here
                        ]
                    ]
                ) 
        goals)


--HELPERS--


findGoalIndex : List Goal -> Int -> Int
findGoalIndex goals id =
    case findIndex (\goal -> goal.id == id) goals of
        Nothing ->
            -1
        Just index ->
            index

updateGoalAtIndex : List Goal -> Int -> Goal -> List Goal
updateGoalAtIndex goals index newGoal =
    case updateAt index (\goal -> newGoal) goals of
        Nothing ->
            goals
        Just newGoals ->
            newGoals


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

findGoalByID : List Goal -> Int -> Goal
findGoalByID goals id =
    case find (\goal -> goal.id == id) goals of
        Nothing ->
            Goal (createNewID (findMaxID goals)) "Example Goal" "10" False (toString Time.now)
        Just goal ->
            goal
      

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
            ("currentDeadline", Json.Encode.string model.currentDeadline),
            ("modalState", encodeEditModalState model.modalState),
            ("currentEditGoal", encodeGoal model.currentEditGoal)
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


encodeEditModalState : Modal.State -> Json.Encode.Value
encodeEditModalState state =
    case state of
        visibleState -> 
            Json.Encode.bool True


decodeEditModalState : Bool -> Decode.Decoder Modal.State
decodeEditModalState bool =
    case bool of
        True ->
            Decode.succeed Modal.hiddenState
        False ->
            Decode.succeed Modal.hiddenState


modelDecoder : Decode.Decoder Model
modelDecoder =
    Decode.map7 Model
        ("score" := Decode.int)
        ("goals" := (Decode.list goalDecoder))
        ("currentGoalName" := Decode.string)
        ("currentGoalScore" := Decode.string)
        ("currentDeadline" := Decode.string)
        ("modalState" := Decode.bool |> Decode.andThen decodeEditModalState)
        ("currentEditGoal" := goalDecoder)

goalDecoder : Decode.Decoder Goal
goalDecoder =
    Decode.map5 Goal
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.string)
        (Decode.field "completed" Decode.bool)
        (Decode.field "deadline" Decode.string)

mapLocalStorageInput : Decode.Value -> (Model, Cmd Msg)
mapLocalStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            (model, Cmd.none)
        
        Err message ->
            (initialModel,  Cmd.none)