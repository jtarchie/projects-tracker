module Main exposing (..)

import Html exposing (..)
import Http
import HttpBuilder exposing (..)
import Json.Encode
import Json.Decode exposing (nullable, andThen, oneOf, Decoder, string, succeed, field, list, at)
import Json.Decode.Extra exposing ((|:))


type alias ContentFields =
    { title : String
    , url : String
    , author : String
    , assignees : List String
    , labels : List String
    , milestone : Maybe String
    }


type Card
    = Note String
    | Content ContentFields


type alias Column =
    { name : String, cards : List Card }


type alias Project =
    { name : String, columns : List Column }


type alias Organization =
    { name : String, project : Project }


type Msg
    = Loaded Organization
    | Fail


main : Program Never (Maybe Organization) Msg
main =
    Html.program
        { init = ( Nothing, getCards )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


view : Maybe Organization -> Html Msg
view model =
    case model of
        Just org ->
            span [] [ text (toString org) ]

        Nothing ->
            span [] [ text "Nothing to show here" ]


update : Msg -> Maybe Organization -> ( Maybe Organization, Cmd Msg )
update msg model =
    case msg of
        Loaded org ->
            ( Just org, Cmd.none )

        _ ->
            ( Nothing, Cmd.none )


decodeContent : Decoder Card
decodeContent =
    at [ "content" ]
        (Json.Decode.map Content <|
            succeed ContentFields
                |: (field "title" string)
                |: (field "url" string)
                |: (at [ "author", "login" ] string)
                |: (field "labels" (at [ "edges" ] (list (at ["node", "name"] string))))
                |: (field "assignees" (at [ "edges" ] (list (at ["node", "login"] string))))
                |: (field "milestone" (nullable (at ["title"] string)))
        )


decodeNote : Decoder Card
decodeNote =
    field "note" string |> andThen (\note -> succeed (Note note))


decodeCard : Decoder Card
decodeCard =
    oneOf [ decodeNote, decodeContent ]


decodeColumn : Decoder Column
decodeColumn =
    succeed Column
        |: (field "name" string)
        |: (field "cards" (at [ "nodes" ] (list decodeCard)))


decodeProject : Decoder Project
decodeProject =
    succeed Project
        |: (field "name" string)
        |: (field "columns" (at [ "nodes" ] (list decodeColumn)))


decodeResponse : Decoder Organization
decodeResponse =
    at [ "data", "organization" ]
        (succeed Organization
            |: (field "name" string)
            |: (field "project" decodeProject)
        )


cardsQuery : String -> String
cardsQuery projectNumber =
    """
  {
    organization(login: "concourse") {
      name
      project(number: """ ++ projectNumber ++ """) {
        name
        columns(first: 10) {
          nodes {
            name
            cards(first: 10) {
              nodes {
                note
                content {
                  ... on Issue {
                    title
                    url
                    author {
                      login
                    }
                    labels(first: 10) {
                      edges {
                        node {
                          name
                        }
                      }
                    }
                    assignees(first: 2) {
                      edges {
                        node {
                          login
                        }
                      }
                    }
                    milestone {
                      title
                    }
                  }
                  ... on PullRequest {
                    title
                    url
                    author {
                      login
                    }
                    labels(first: 10) {
                      edges {
                        node {
                          name
                        }
                      }
                    }
                    assignees(first: 2) {
                      edges {
                        node {
                          login
                        }
                      }
                    }
                    milestone {
                      title
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  """


token : String
token =
    "token"


handleGetCards : Result Http.Error Organization -> Msg
handleGetCards request =
    case request of
        Ok org ->
            Loaded org

        Err error ->
            Fail


getCards : Cmd Msg
getCards =
    let
        search =
            Json.Encode.encode 0 (Json.Encode.string <| cardsQuery "22")
    in
        HttpBuilder.post "https://api.github.com/graphql"
            |> withStringBody "application/json" ("{\"query\":" ++ search ++ "}")
            |> withHeader "Authorization" ("Bearer " ++ token)
            |> withExpect (Http.expectJson decodeResponse)
            |> send handleGetCards
