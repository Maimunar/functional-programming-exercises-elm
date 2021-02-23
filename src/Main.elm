module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import List
import Debug exposing (toString)


-- Change this code to try with different arrays
palindromeCheckArray = 
    [1,2,5,2,1]

-- Assignment 1
isPalindrome: List a -> Bool
isPalindrome listToCheck = 
    listToCheck == (List.reverse listToCheck)


main =
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
                p [class "center"] [text (String.concat ["Input - ", toString palindromeCheckArray])],
                p [class "center"] [text (String.concat ["Output - ", toString (isPalindrome palindromeCheckArray)])]
            ],
            div
            [ class "container"]
            [
                h3 [class "palindrome-text"] [text "Palindrome Checker"],
                p [class "center"] [text (String.concat ["Input - ", toString palindromeCheckArray])],
                p [class "center"] [text (String.concat ["Output - ", toString (isPalindrome palindromeCheckArray)])]
            ]            
        ]
    