module Application.Session

open Model
open Model.Common
open System

type InsertError = UniquenessError of string

type IInMemoryDatabase<'Key, 'T> =
    abstract member Lookup: 'Key -> Option<'T>
    abstract member UnsafeLookup: 'Key -> 'T
    abstract member Insert: 'Key -> 'T -> Result<unit, InsertError>
    abstract member Delete: 'Key -> unit
    abstract member Update: 'Key -> 'T -> unit
    abstract member All: unit -> seq<'T>
    abstract member Filter: ('T -> bool) -> seq<'T>

type IStore =
    abstract member GetCandidates: unit -> IInMemoryDatabase<string, Candidate>
    abstract member GetSessions: unit -> IInMemoryDatabase<string * DateTime, Session>
    abstract member GetGuardians: unit -> IInMemoryDatabase<string, string * string>



