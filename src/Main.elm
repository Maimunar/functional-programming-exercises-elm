module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import List exposing (..)
import Debug exposing (toString)
import List.Extra
import List
import Html
import Array

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
    removeItemAtIndex : String,
    rleInput: String,
    grayInput: String,
    sieveInput: String}


init : Model
init =
    { palindromeInput = "Please input here",
    listSplitIndex = "0",
    removeItemAtIndex = "0",
    rleInput="",
    grayInput="",
    sieveInput="0"}

-- UPDATE

type Msg
    = SetPalindromeInput String
    | SetListSplitIndex String
    | SetRemoveItemAtIndex String
    | SetRleInput String
    | SetGrayInput String
    | SetSieveInput String

update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPalindromeInput newInput ->
            {model | palindromeInput = newInput}
        SetListSplitIndex newIndex ->
            {model | listSplitIndex = newIndex }
        SetRemoveItemAtIndex newIndex ->
            {model | removeItemAtIndex = newIndex}
        SetRleInput newInput ->
            {model | rleInput = newInput}
        SetGrayInput newInput ->
            {model | grayInput = newInput}
        SetSieveInput newInput ->
            {model | sieveInput = newInput}
        

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
            ],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "Run-Length-Encoding"],
                p [class "center"] [text (String.concat ["Input - ", 
                toString (String.toList model.rleInput )])],
                input [ type_ "text", class "center", onInput SetRleInput] [],                                                             
                p [class "center"] [text (String.concat ["Output - ", 
                toString (toListofEquals (String.toList model.rleInput))])]
            ],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "Gray Code"],
                p [class "center"] [text (String.concat ["Input - ", 
                model.grayInput])],
                input [ type_ "text", class "center", onInput SetGrayInput] [],                                                             
                p [class "center"] [text (String.concat ["Output - ", 
                toString (calculateGray (String.toInt model.grayInput |> Maybe.withDefault 0))])]
            ],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "Sieve of Eratosthenes"],
                p [class "center"] [text (String.concat ["Input - ", 
                model.sieveInput])],
                input [ type_ "number", class "center", onInput SetSieveInput] [],                                                             
                p [class "center"] [text (String.concat ["Output - ", 
                toString (sieveOfEratosthenes (String.toInt model.sieveInput |> Maybe.withDefault 2))])]
            ]            
        ]
    
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

-- Assignment 4
-- Create a list of tuples (Item, Count) from a list of single-item lists


-- METHOD NOT IN USE
-- Frontend helper function to convert the strings to a list of lists
-- Also filters out empty lists
-- Currently using the method bellow (toListofEquals)
toListofLists: String -> String -> String -> String -> String -> List (List Char)
toListofLists a b c d e =
    List.filter (\x -> List.length x > 0) [(String.toList a), (String.toList b), (String.toList c), (String.toList d), (String.toList e)]
-- Actual assignment code
-- Takes a list of lists of chars - [['a','a'], ['b','b']]
-- Returns a list of tuples - [('a',2), ('b',2)]
-- The Maybe.withDefault is used to turn a maybe Char into a definite char (needed for my type checking)
runLengthEncoding: List (List Char) -> List (Char, Int)
runLengthEncoding listToConvert =
    (List.map (\x -> (head x|> Maybe.withDefault '0', List.length x)) listToConvert)


-- Better code for Assignment 4
-- Takes as much inputs as you would like to
toListofEquals : List Char-> List (Char, Int)
toListofEquals input =
    List.map 
    convertTuple
    (List.Extra.group input)

-- List.Extra.group gives the values as ['1','1','1'] -> ('1', ['1','1'])
-- This helper function goes in the map function and does the following:
-- ('1', ['1','1']) -> ('1', 3)
convertTuple: (a, List a) -> (a, Int)
convertTuple tuple = 
    (Tuple.first tuple, (List.length (Tuple.second tuple) + 1))


-- Assignment 5
-- Recursively calculating gray's code
calculateGray: Int -> List String
calculateGray n =
    -- Edge case, mostly for errors and the start case
    if (n <= 0) then
        ["0"]
    -- Base case, that's how you get the first numbers
    else if (n == 1) then
        ["0", "1"]
    else
        let
            -- Following the recursion structure guide on elm's website
            recursionGray = calculateGray (n-1)
        in
            -- First adding all available items with a 0 in front
            (List.map (\x -> "0" ++ x) recursionGray) ++ 
            -- Secondly adding them reversed with a 1 in front
            (List.map (\x -> "1" ++ x) (List.reverse recursionGray))

-- Assignment 6
-- sieve of Eratosthenes

-- The main method being called to display the output
-- Step 1: Creates an array starting with 2 Falses and n-1 Trues 
-- (Counting 0 and 1 as false to get the right index)
-- Step 2: Converts most True values to false through the recursive functions bellow
-- Step 3: Maps the boolean array to its index when true and to 0 when false
-- Step 4: Filters all 0's from the array
-- Example outputs provided in the code for clearer explanation
sieveOfEratosthenes: Int -> List Int
sieveOfEratosthenes n =
    -- Step 4: [2,3,5]
    List.filter
        (\x -> 
            not (x == 0)
        )
        -- Step 3: [0, 0, 2, 3, 0, 5]
        (List.indexedMap
            (\index x -> 
                if (x) then
                    index
                else
                    0
            )
            -- Step 2: [False, False, True, True, False, True]
            (sieveRec 
                -- Step 1: [False, False, True, True, True, True] on n=5
                (False :: (False :: (List.repeat (n-1) True)))
                2
            )
        )

-- Method used as the outer loop looking for values to run the makeFalse check at
sieveRec: List Bool -> Int -> List Bool
sieveRec sieveArr n =
    -- Recursion is going in an ascending order here to make further logic easier
    -- An optimisation to be had here in terms of performance would be to reduce the list.length variable
    -- A good potential idea is to take the floor of the square root of it
    if (n >= List.length sieveArr) then
        sieveArr
    else
        let
            -- Note - n+1
            recursionSieve = sieveRec sieveArr (n+1)
        in
            -- If true, make every next item incrementing with n false
            if (Array.get n (Array.fromList recursionSieve) |> Maybe.withDefault True) then
                (makeFalseRec recursionSieve (2*n) n)
            else
                recursionSieve

-- Method used as the inner loop to make values false
makeFalseRec: List Bool -> Int -> Int -> List Bool
makeFalseRec sieveArr index addNum = 
    -- Again the recursion is ascending
    if (index >= List.length sieveArr) then
        sieveArr
    else
        let
            -- Incementing with a separate addNum value equaling n to make sure we don't influence it
            recArray = makeFalseRec sieveArr (index + addNum) addNum
        in
            -- Set the value at the index as false
            Array.toList (Array.set index False (Array.fromList recArray))