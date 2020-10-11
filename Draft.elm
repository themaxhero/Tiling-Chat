module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element
    exposing
        ( Attribute
        , Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , clip
        , column
        , el
        , fill
        , height
        , image
        , padding
        , paddingXY
        , transparent
        , wrappedRow
        , pointer
        , px
        , rgb255
        , rgba255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



-- Model


type alias ID =
    String


type alias URL =
    String


type alias MessageID =
    String


type Container
    = Horizontal (List Container)
    | Vertical (List Container)
    | Node ID Chat


type alias LoginData =
    { login : Maybe String
    , password : Maybe String
    }


type alias LoggedData =
    { contacts : Dict ID Contact
    , profile : Contact
    , chats : Container
    }


type Message
    = Incoming String
    | Sending String


type MessageBuilder
    = Text String MessageBuilder
    | Emote String MessageBuilder
    | Link String MessageBuilder
    | MessageStart


type Status
    = Online
    | Away
    | Busy
    | Offline


type alias Contact =
    { nickname : String
    , realName : String
    , avatar : URL
    , status : Status
    }


type alias Chat =
    { contactId : ID
    , messages : Dict MessageID Message
    , textArea : MessageBuilder
    }


type Model
    = LoginPage LoginData
    | LoggedPage LoggedData


init : () -> UpdateResponse msg
init _ =
    ( LoggedPage loggedDataDummy, Cmd.none )


stringToMaybeString : String -> Maybe String
stringToMaybeString str =
    if str == "" then
        Nothing

    else
        Just str


statusToString : Status -> String
statusToString status =
    case status of
        Online ->
            "Online"

        Away ->
            "Away"

        Busy ->
            "Busy"

        Offline ->
            "Offline"


statusToColor : Status -> Attribute Msg
statusToColor status =
    case status of
        Online ->
            Font.color <| rgb255 36 199 71

        Away ->
            Font.color <| rgb255 255 165 0

        Busy ->
            Font.color <| rgb255 185 0 0

        Offline ->
            Font.color <| rgb255 128 128 128



-- Msg


type Msg
    = OpenChat ID
    | SendMsg ID Message
    | ReceiveMsg ID Message
    | Login String String -- Username and Password
    | ChangeStatus Status
    | ContactStatusChange ID Status
    | ContactAdd ID String -- Contact's ID and Nickname
    | ContactRemove ID -- Contact's ID
    | UpdateLoginField String
    | UpdatePasswordField String



-- Update


type alias UpdateResponse msg =
    ( Model, Cmd msg )



-- Login


contactDummy =
    { nickname = "Max Hero"
    , realName = "Marcelo Amancio"
    , avatar = "https://avatarfiles.alphacoders.com/202/202499.png"
    , status = Online
    }


contactListDummy : Dict ID Contact
contactListDummy =
    Dict.fromList
        [ ( "0", contactDummy )
        , ( "1", contactDummy )
        , ( "2", contactDummy )
        , ( "3", contactDummy )
        , ( "4", contactDummy )
        , ( "5", contactDummy )
        , ( "6", contactDummy )
        ]


messagesDummy =
    Dict.fromList
        [ ("0", Incoming "Uma monad é apenas uma monoid na categoria dos endofunctors")
        , ("1", Sending "kek")
        ]

chatDummy =
    { contactId = ""
    , messages = messagesDummy
    , textArea = MessageStart
    }


loggedDataDummy =
    { contacts = contactListDummy
    , profile = contactDummy
    , chats = Vertical [ Node "0" chatDummy, Node "1" chatDummy]
    }


doLogin : String -> String -> Model -> UpdateResponse msg
doLogin username password model =
    ( LoggedPage loggedDataDummy, Cmd.none )


doUpdateLoginField : String -> Model -> UpdateResponse msg
doUpdateLoginField username model =
    case model of
        LoginPage data ->
            ( LoginPage { data | login = stringToMaybeString username }, Cmd.none )

        _ ->
            ( model, Cmd.none )


doUpdatePasswordField : String -> Model -> UpdateResponse msg
doUpdatePasswordField password model =
    case model of
        LoginPage data ->
            ( LoginPage { data | password = stringToMaybeString password }, Cmd.none )

        _ ->
            ( model, Cmd.none )


loginPageUpdate : Msg -> LoginData -> Model -> UpdateResponse msg
loginPageUpdate msg data model =
    case msg of
        Login username password ->
            doLogin username password model

        UpdateLoginField username ->
            doUpdateLoginField username model

        UpdatePasswordField password ->
            doUpdatePasswordField password model

        _ ->
            ( model, Cmd.none )



-- Logged


doOpenChat : ID -> LoggedData -> UpdateResponse msg
doOpenChat contactId data =
    ( LoggedPage data, Cmd.none )


doSendMessage : ID -> Message -> LoggedData -> UpdateResponse msg
doSendMessage contactId message data =
    ( LoggedPage data, Cmd.none )


handleReceiveMessage : ID -> Message -> LoggedData -> UpdateResponse msg
handleReceiveMessage contactId message data =
    ( LoggedPage data, Cmd.none )


doChangeStatus : Status -> LoggedData -> UpdateResponse msg
doChangeStatus status data =
    ( LoggedPage data, Cmd.none )


handleContactStatusChange : ID -> Status -> LoggedData -> UpdateResponse msg
handleContactStatusChange contactId status data =
    ( LoggedPage data, Cmd.none )


doContactAdd : ID -> String -> LoggedData -> UpdateResponse msg
doContactAdd contactId nickname data =
    ( LoggedPage data, Cmd.none )


doContactRemove : ID -> LoggedData -> UpdateResponse msg
doContactRemove contactId data =
    ( LoggedPage data, Cmd.none )


loggedPageUpdate : Msg -> LoggedData -> Model -> UpdateResponse msg
loggedPageUpdate msg data model =
    case msg of
        OpenChat contactId ->
            doOpenChat contactId data

        SendMsg contactId message ->
            doSendMessage contactId message data

        ReceiveMsg contactId message ->
            handleReceiveMessage contactId message data

        ChangeStatus status ->
            doChangeStatus status data

        ContactStatusChange contactId status ->
            handleContactStatusChange contactId status data

        ContactAdd contactId nickname ->
            doContactAdd contactId nickname data

        ContactRemove contactId ->
            doContactRemove contactId data

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> UpdateResponse msg
update msg model =
    case model of
        LoginPage data ->
            loginPageUpdate msg data model

        LoggedPage data ->
            loggedPageUpdate msg data model



-- Login View


loginFieldInputProps onChange label content =
    { onChange = onChange
    , text = Maybe.withDefault "" content
    , placeholder = Nothing
    , label = Input.labelAbove [] (text label)
    }


loginFieldInput : Maybe String -> String -> (String -> Msg) -> Element Msg
loginFieldInput content label onChange =
    content
        |> loginFieldInputProps onChange label
        |> Input.text []


usernameInput : Maybe String -> Element Msg
usernameInput login =
    loginFieldInput login "Username" UpdateLoginField


passwordInputProps password =
    { onChange = UpdatePasswordField
    , text = Maybe.withDefault "" password
    , placeholder = Nothing
    , label = Input.labelAbove [] (text "Password")
    , show = False
    }


passwordInput : Maybe String -> Element Msg
passwordInput password =
    password
        |> passwordInputProps
        |> Input.newPassword []


loginMessage : LoginData -> Maybe Msg
loginMessage { login, password } =
    case ( login, password ) of
        ( Just actualLogin, Just actualPassword ) ->
            Just (Login actualLogin actualPassword)

        _ ->
            Nothing


shouldDisableLogin : LoginData -> Bool
shouldDisableLogin { login, password } =
    case ( login, password ) of
        ( Just actualLogin, Just actualPassword ) ->
            True

        _ ->
            False


loginButton : LoginData -> Element Msg
loginButton data =
    Input.button
        [ Background.color (rgb255 32 32 32)
        , Font.color (rgb255 200 200 200)
        , alignRight
        , paddingXY 8 8
        , Border.rounded 8
        ]
        { onPress = loginMessage data
        , label = text "Login"
        }


viewLoginStyle : List (Attribute Msg)
viewLoginStyle =
    [ centerX
    , centerY
    ]


viewLoginForm : LoginData -> Element Msg
viewLoginForm data =
    column [ spacing 8 ]
        [ usernameInput data.login
        , passwordInput data.password
        , loginButton data
        ]


viewLogin : LoginData -> Element Msg
viewLogin data =
    el viewLoginStyle (viewLoginForm data)



-- Logged View


userProfile : LoggedData -> Element Msg
userProfile data =
    row [ paddingXY 8 8
        , spacing 8
        ]
        [ el
            [ Border.rounded 24
            , clip
            , width (px 128)
            , height (px 128)
            ]
            (image
                [ width fill, height fill ]
                { src = data.profile.avatar
                , description = "User Avatar"
                }
            )
        , column [ spacing 8 ]
            [ el
                [ Font.size 24
                , Font.color (rgb255 196 196 196)
                ]
                (text data.profile.nickname)
            , el
                [ Font.size 18
                , statusToColor data.profile.status
                ]
                (text <| statusToString data.profile.status)
            ]
        ]


contactListMember : ID -> Contact -> Element Msg
contactListMember id contact =
    row
        [ paddingXY 8 8
        , spacing 8
        , Background.color (rgb255 56 56 56)
        , Border.rounded 8
        , width (px 328)
        , onClick (OpenChat id)
        , pointer
        ]
        [ el
            [ Border.rounded 8
            , clip
            , width (px 64)
            , height (px 64)
            ]
            (image
                [ width fill, height fill ]
                { src = contact.avatar, description = "Contact Avatar" }
            )
        , column
            [ spacing 4 ]
            [ el [ Font.size 18 ] (text contact.nickname)
            , el
                [ Font.size 16, statusToColor contact.status ]
                (text <| statusToString contact.status)
            ]
        ]


contactReducer : ID -> Contact -> List (Element Msg) -> List (Element Msg)
contactReducer id contact acc =
    contactListMember id contact :: acc


contactListStyles : List (Attribute Msg)
contactListStyles =
    [ paddingXY 8 8
    , Background.color (rgb255 71 71 71)
    , Border.rounded 8
    , spacing 8
    , width fill
    , height fill
    ]


contactList : LoggedData -> Element Msg
contactList data =
    data
        |> .contacts
        |> Dict.foldl contactReducer []
        |> column contactListStyles


sidebarStyles : List (Attribute Msg)
sidebarStyles =
    [ Background.color (rgb255 64 64 64)
    , width (px 360)
    , height fill
    , paddingXY 8 8
    ]


sidebar : LoggedData -> Element Msg
sidebar data =
    data
        |> contactList
        |> List.singleton
        |> (::) (userProfile data)
        |> column sidebarStyles


incomingMessage : MessageID -> String -> Element Msg
incomingMessage id str =
    wrappedRow
        [ Border.roundEach
            { topLeft = 0
            , topRight = 8
            , bottomLeft = 8
            , bottomRight = 8
            }
        , padding 8
        , alignLeft
        , Background.color (rgb255 71 71 71)
        , Font.size 16
        ]
        [text str]


sendingMessage : MessageID -> String -> Element Msg
sendingMessage id str =
    wrappedRow
        [ Border.roundEach
            { topLeft = 8
            , topRight = 0
            , bottomLeft = 8
            , bottomRight = 8
            }
        , padding 8
        , alignRight
        , Background.color (rgb255 104 104 104)
        , Font.size 16
        ]
        [text str]


messageView : MessageID -> Message -> List (Element Msg) -> List (Element Msg)
messageView id message acc =
    case message of
        Incoming str ->
            incomingMessage id str :: acc

        Sending str ->
            sendingMessage id str :: acc


chatHeader : Chat -> LoggedData -> Element Msg
chatHeader chat =
    .contacts
        >> Dict.get chat.contactId
        >> Maybe.withDefault contactDummy
        >> contactListMember chat.contactId

chatMessages : Dict ID Message -> Element Msg
chatMessages messages =
    column
        [ Background.color (rgb255 40 40 40)
        , Border.rounded 8
        , spacing 8
        , padding 8
        , width fill
        , height fill
        , Border.solid
        , Border.color (rgb255 196 196 196)
        ]
        (Dict.foldr messageView [] messages)


chatMessageBox : ID -> Chat -> LoggedData -> Element Msg
chatMessageBox id chat data =
    el
        [ padding 8
        , width fill
        , Border.rounded 8
        , Background.color (rgb255 40 40 40)
        ]
        (column
            [ width fill
            , spacing 8
            ]
            [ Input.text
                [ Font.color (rgb255 39 39 39)
                , Background.color (rgba255 0 0 0 0)
                , Border.color (rgba255 0 0 0 0)
                , Font.size 16
                ]
                { onChange = UpdatePasswordField
                , text = ""
                , placeholder = Just <| Input.placeholder [] <| text "Digite uma mensagem para enviar"
                , label = Input.labelHidden "Digite uma mensagem para enviar"
                }
            , row
                [ width fill, spacing 8 ]
                [ Input.button
                    [ Border.rounded 8
                    , width (px 32)
                    , height (px 32)
                    , Background.color (rgb255 71 71 71)
                    , alignRight
                    ]
                    { onPress = Nothing
                    , label = text ""
                    }
                , Input.button
                    [ Border.rounded 8
                    , width (px 32)
                    , height (px 32)
                    , Background.color (rgb255 71 71 71)
                    , alignRight
                    ]
                    { onPress = Nothing
                    , label = text ""
                    }
                ]
            ])


currentChatMapper : LoggedData -> Container -> Element Msg
currentChatMapper data container =
    case container of
        Horizontal chats ->
            row [ spacing 8, width fill, height fill ]
                (List.map (currentChatMapper data) chats)

        Vertical chats ->
            column [ spacing 8, width fill, height fill ]
                (List.map (currentChatMapper data) chats)

        Node id chat ->
            column
                [ Border.rounded 8
                , Background.color (rgb255 56 56 56)
                , width fill
                , height fill
                , padding 8
                , spacing 8
                ]
                [ chatHeader chat data
                , chatMessages chat.messages
                , chatMessageBox id chat data
                ]

currentChatsStyles : List (Attribute Msg)
currentChatsStyles =
    [ width fill
    , height fill
    , padding 8
    ]

currentChats : LoggedData -> Element Msg
currentChats data =
    data
        |> .chats
        |> currentChatMapper data
        |> el currentChatsStyles


viewLogged : LoggedData -> Element Msg
viewLogged data =
    row
        [ Font.color (rgb255 196 196 196)
        , Background.color (rgb255 39 39 39)
        , width fill
        , height fill
        ]
        [ sidebar data
        , currentChats data
        ]



-- View


view : Model -> Element Msg
view model =
    case model of
        LoginPage data ->
            viewLogin data

        LoggedPage data ->
            viewLogged data



-- Subs


subs : Model -> Sub msg
subs _ =
    Sub.none



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Element.layout []
        , update = update
        , subscriptions = subs
        }
