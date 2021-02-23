module Main2 exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import List exposing (..)
import Debug exposing (toString)


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


-- MODEL

type alias Model =
    { palindromeInput : String,
    listSplitIndex : String,
    removeItemAtIndex : String }


init : Model
init =
    { palindromeInput = "Please input here",
    listSplitIndex = "0",
    removeItemAtIndex = "0"}

-- UPDATE

type Msg
    = SetPalindromeInput String
    | SetListSplitIndex String
    | SetRemoveItemAtIndex String

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPalindromeInput newInput ->
            {model | palindromeInput = newInput}
        SetListSplitIndex newIndex ->
            {model | listSplitIndex = newIndex }
        SetRemoveItemAtIndex newIndex ->
            {model | removeItemAtIndex = newIndex}
        

-- VIEW

view : Model -> Html Msg
view model = 
    div
        []
        [
            node "link"
                [ rel "stylesheet"
                , href "stylesheets/main.css"
                ]
                [],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "Palindrome Checker"],
                p [class "center"] [text (String.concat ["Input - ", model.palindromeInput])],
                input [ type_ "text", class "center", onInput SetPalindromeInput] [],
                p [class "center"] [text (String.concat ["Output - ", toString (isPalindrome (String.toList model.palindromeInput))])]
            ],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "List Split"],
                p [class "center"] [text (String.concat ["List - ", toString exampleList])],
                p [class "center"] [text (String.concat ["Split at that much numbers - ", model.listSplitIndex])],
                input [ type_ "number", class "center", onInput SetListSplitIndex] [],
                p [class "center"] [text (String.concat ["Output - ", (toString (listSplit exampleList (String.toInt model.listSplitIndex |> Maybe.withDefault 0)))])]
            ],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "Remove Element at N"],
                p [class "center"] [text (String.concat ["List - ", toString exampleList])],
                p [class "center"] [text (String.concat ["Remove Item at - ", model.removeItemAtIndex])],
                input [ type_ "number", class "center", onInput SetRemoveItemAtIndex] [],
                p [class "center"] [text (String.concat ["Output - ", (toString (removeAtN exampleList (String.toInt model.removeItemAtIndex |> Maybe.withDefault 0)))])]
            ]
        ]
    

-- Change this code to try with different arrays

-- Assignment 1
-- If a list and its reverse are equal, the list is a palindrome
isPalindrome: List a -> Bool
isPalindrome listToCheck = 
    listToCheck == (List.reverse listToCheck)

-- Assignment 2
-- An example list im using for the exercises
exampleList = 
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

-- My approach is to create a list of the 2 separated lists
listSplit: List a -> Int -> List (List a)
listSplit inputList indexToSplit =
    [(take indexToSplit inputList), (drop indexToSplit inputList)]

-- Assignment 3
-- Create a list of all elements, except take 1 less at the index to remove at n
removeAtN: List a -> Int -> List a
removeAtN inputList indexToRemove =
    (take (indexToRemove - 1) inputList) ++ (drop indexToRemove inputList)