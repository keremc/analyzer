type t =
  [ `Nothing
  | `Value of string
  | `Pair of t * t
  | `Triplet of t * t * t
  | `List of t list
  | `Assoc of (string * t) list
  | `Tagged of string * t ]

let rec to_yojson (r : t) : Yojson.Safe.t = match r with
  | `Nothing -> `Null
  | `Value s -> `String s
  | `Pair (a, b) -> `List [ to_yojson a; to_yojson b ]
  | `Triplet (a, b, c) -> `List [ to_yojson a; to_yojson b; to_yojson c ]
  | `List l -> `List (BatList.map to_yojson l)
  | `Assoc l -> `Assoc (BatList.map (fun (k, v) -> k, to_yojson v) l)
  | `Tagged (n, r) -> `List [ `String n; to_yojson r ]