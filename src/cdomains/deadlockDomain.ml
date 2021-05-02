open Deriving.Cil
open Pretty

type myowntypeEntry = {addr : ValueDomain.Addr.t ; loc : location} [@@deriving to_yojson]


module MyLock : Printable.S with type t = myowntypeEntry =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  type t = myowntypeEntry [@@deriving to_yojson]
  module Ad = ValueDomain.Addr
  let name () = "address with location"
  let equal x y = Ad.equal x.addr y.addr
  let hash x = Ad.hash x.addr
  let compare x y = Ad.compare x.addr y.addr
  let short w x = (Ad.short (w/2) x.addr) ^ "@" ^ (Basetype.ProgLines.short (w/2) x.loc)
  let isSimple x = true
  let pretty_f sh () x = Ad.pretty () x.addr ++ text "@" ++ Basetype.ProgLines.pretty () x.loc
  let pretty = pretty_f short
  let printXml c x = Ad.printXml c x.addr
  let represent x = Ad.represent x.addr
  let pretty_diff () (x,y) = Ad.pretty_diff () (x.addr,y.addr)
end

module Lockset = SetDomain.ToppedSet (MyLock) (struct let topname = "all locks" end)
