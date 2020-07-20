module Model exposing (Model, initialModel)
import Type.Contact exposing(Contact)
import Type.Chat exposing(Chat)

-- Data Types

type alias Id =
  String

type Container
  = Horizontal (List Container)
  | Vertical (List Container)
  | Node Chat

type alias Model =
  { userId: Maybe Id
  , friendList : List Contact
  , chats: Container
  }

-- Initial Model

initialModel : Model
initialModel =
  { userId : Nothing
  , friendList : []
  , chats : Horizontal []
  }

-- Model Public API

setUserId : Maybe Id -> Model -> Model
setUserId userId model =
  { model | userId = userId }

setFriendList : List Contact -> Model -> Model
setFriendList friendList model =
  { model | friendList = friendList }

setChats : Container -> Model -> Model
setChats container model =
  { model | container = container }

getUserId : Model -> Maybe Id
getUserId =
  .userId

getFriendList : Model -> List Contact
getFriendList =
  .friendList

getChats : Model -> Container
getChats =
  .chats

-- Model Private API