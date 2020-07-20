module Component.Chat exposing (view)
import Element exposing (
  Element,
  el,
  text,
  column,
  row,
  width,
  height,
  px,
  rgb255,
  spacing,
  padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

userStatusAttr : User -> List (Attribute Msg)
userStatusAttr user =
  []

userStatus : User -> Element Msg
userStatus user =
  user
    |> User.getStatus
    |> statusToString
    |> text
    |> el (userStatusAttr user)

nameAttr : List (Attribute Msg)
nameAttr =
  [ Font.color (rgb255 0xC4 0xC4 0xC4) ]

name : String -> Element Msg
name =
  text >> el nameAttr

status : Contact.Status -> Element Msg
status status =
  status
    |> Contact.statusToString
    |> text
    |> el (statusAttr status)

userName : User -> Element Msg
userName =
  User.getName >> name

userImage : User -> Element Msg
userImage user =
  Element.image [ width (px 72), height (px 72) ] 
    { src = User.getImage user
    , description = "User's Avatar"
    }

userInfoAttr : List (Attribute Msg)
userInfoAttr =
  [ spacing 8 ]

userInfo : User -> Element Msg
userInfo user =
  column userInfoAttr [ userName user, userStatus user ]

userAttr : List (Attribute Msg)
userAttr model =
  [ padding 8
  , spacing 8
  , Background.color <| rgb255 0x38 0x38 0x38
  ]

user : Model -> Element Msg
user model =
  row userAttr [ userImage user , userInfo user ]

contactImage : Contact -> Element Msg
contactImage contact =
  Element.image [ width (px 64), height (px 64) ]
    { src = Contact.getImage contact
    , description = (Contact.getName contact) + "'s Avatar"
    }

contactName : Contact -> Element Msg
contactName =
  Contact.getName >> name

contactStatus : Contact -> Element Msg
contactStatus =
  Contact.getStatus >> status

contactInfoAttr : Contact -> List (Attribute Msg)
contactInfoAttr =
  [ spacing 8 ]

contactInfo : Contact -> List (Element Msg)
contactInfo contact =
  flip column [ contactName contact, contactStatus contact ]

contactAttr : List (Attribute Msg)
contactAttr =
  [ Border.rounded 16 ]

contact : Contact -> Model -> Element Msg
contact contact model =
  row contactAttr
    [ contactImage user
    , contactInfo contact contactInfoAttr
    ]

friendListAttr : List (Attribute Msg)
friendListAttr =
  [ padding 8
  , spacing 8
  , Background.color <| rgb255 0x47 0x47 0x47
  , Border.rounded 16
  ]

friendList : Model -> Element Msg
friendList model = 
  model
    |> List.foldl (flip contact model) []
    |> column friendListAttr

attr : List (Attribute Msg)
attr =
  [ padding 8 ]

view : Model -> Element Msg
view model =
  column attr
    [ user model
    , friendList model
    ]