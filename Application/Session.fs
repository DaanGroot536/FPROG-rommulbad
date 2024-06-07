module Application.Session

open Model
open Model.Common

type IStore =
    abstract RetrieveAllCandidates : unit -> System.Collections.Generic.ICollection<string * System.DateTime * string * string>
    abstract GetCandidate : string -> Option<string * System.DateTime * string * string>
    abstract GetSessions : string -> (string * bool * System.DateTime * int) seq
    abstract GetTotalMinutes : string -> int
    abstract GetEligibleSessions : string * string -> List<Session>
    abstract GetTotalEligibleMinutes : string * string -> int

let retrieveAllCandidates (db: IStore) =
    db.RetrieveAllCandidates ()

let getCandidate (db: IStore) (name: string) =
    db.GetCandidate name

let getSessions (db: IStore) (name: string) =
    db.GetSessions name

let getTotalMinutes (db: IStore) (name: string) =
    db.GetTotalMinutes name

let getEligibleSessions (db: IStore) (name: string) (diploma: string) =
    db.GetEligibleSessions (name, diploma)

let getTotalEligibleMinutes (db: IStore) (name: string) (diploma: string) =
    db.GetTotalEligibleMinutes (name, diploma)
