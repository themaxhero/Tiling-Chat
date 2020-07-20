module Type.Contact exposing (Contact)

type Name =
  String

type Status
  = Online
  | Busy
  | Away
  | Offline

type alias Contact =
  { name: Name
  , status: Status
  }