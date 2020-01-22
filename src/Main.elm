import Array exposing (set, get, toList, fromList)
import Html exposing (text, input, div, p, h1, h2, span, Html)
import Browser
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MAIN
main =
    Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
    String

init : Model
init = ""


-- UPDATE

type Msg
    = OnUpdate String

update : Msg -> Model -> Model
update msg model  =
    case msg of
       OnUpdate string ->
        string


view : Model -> Html Msg
view model =
    let
        perms = stringPermutation model
    in
    
    div []
        [ h1 [] [text "Permutation in Elm"] 
        , p [] [text "Write a text below: (limited to 7 chars)"]
        , input [type_ "text", placeholder "Palavra", value model, onInput OnUpdate, maxlength 7] []
        , p [] [text (String.concat ["Count distinct: ", (String.fromInt (List.length perms))])] 
        , p [] [text (String.join ", " perms)]
        ]


swap : List a -> Int -> Int -> List a
swap list pos1 pos2 =
    case (get pos1 (fromList list)) of
        Nothing -> []

        Just elem1 ->
            case (get pos2 (fromList list)) of
                Nothing -> list

                Just elem2 ->
                    toList(
                        set pos1 elem2 (
                            set pos2 elem1 (fromList list)
                        )
                    )


permute : Int -> List(List a) -> List a -> List(List a)
permute pos acc list =
    let
     options = List.map (swap list pos) (List.range (pos +1) (List.length list))
    in
        if pos > ((List.length list) - 1 ) then
            [list]
        else
            List.concat (List.map (permute (pos + 1) [])  options )

distinct : List a -> List a -> List a
distinct acc list =
    case list of
       [] -> acc

       x :: xs ->
        if List.member x acc then
            distinct acc xs
        else
            distinct (x :: acc) xs


stringPermutation : String -> List String
stringPermutation string =
    distinct [] (List.map (String.fromList) (permute 0 [] (String.toList string)))
