module Type.Chat exposing (..)
import Dict exposing (Dict)

type alias Image =
  Maybe String

type alias Id =
  String

type Type
  = Received Image
  | Sent Image

type Content
  = Emote String
  | Text String
  | URL String
  | Msg Content

type alias Response =
  { messageId : Id
  , content : Content
  }

type alias Message =
  { content : Content
  , response : Maybe Response
  , msgType : Type
  }

type alias Chat =
  { contactId : String
  , chatHistory : Dict Id Message
  }