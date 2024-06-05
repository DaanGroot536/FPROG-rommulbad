module Application.Session

open Model
open Model.Common

type IInMemoryDatabase =
    abstract RetrieveAllCandidates : unit -> List<Candidate>
    abstract GetCandidate : Name -> Option<Candidate>
    abstract AddSession : string * Session -> Result<unit, InsertError>
    abstract GetSessions : string -> List<Session>
    abstract GetTotalMinutes : string -> int
    abstract GetEligibleSessions : string * string -> List<Session>
    abstract GetTotalEligibleMinutes : string * string -> int

let retrieveAllCandidates (db: IInMemoryDatabase) =
    db.RetrieveAllCandidates ()

let getCandidate (db: IInMemoryDatabase) (name: Name) =
    db.GetCandidate name

let addSession (db: IInMemoryDatabase) (name: string) (session: Session) =
    db.AddSession (name, session)

let getSessions (db: IInMemoryDatabase) (name: string) =
    db.GetSessions name

let getTotalMinutes (db: IInMemoryDatabase) (name: string) =
    db.GetTotalMinutes name

let getEligibleSessions (db: IInMemoryDatabase) (name: string) (diploma: string) =
    db.GetEligibleSessions (name, diploma)

let getTotalEligibleMinutes (db: IInMemoryDatabase) (name: string) (diploma: string) =
    db.GetTotalEligibleMinutes (name, diploma)
