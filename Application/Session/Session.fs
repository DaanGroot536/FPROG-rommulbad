module Application.Session

open Model
open Model.Common
open Application.Errors

type ISessionDataAccess =
    abstract GetAllSessions : unit -> Session seq
    abstract StoreSession : Session -> Result<unit, StoreError>
    abstract GetSessionsByName : string -> Session seq
    abstract GetTotalMinutes : string -> Session seq
    abstract GetEligebleSessions : (Session -> bool) -> Session seq

let getAllSessions (dataAccess: ISessionDataAccess) =
    dataAccess.GetAllSessions()
    |> Seq.toList

let storeSession (dataAccess: ISessionDataAccess) session =
    dataAccess.StoreSession(session)

let getSessionsByName (dataAccess: ISessionDataAccess) (name: string) =
    dataAccess.GetSessionsByName name
    |> Seq.toList

let getTotalMinutes (dataAccess: ISessionDataAccess) (name: string) =
    dataAccess.GetTotalMinutes name
    |> Seq.map (fun session -> SessionLength.intValue session.Minutes)
    |> Seq.sum

let getEligibleSessions (dataAccess: ISessionDataAccess) (name: string) (diploma: string) =
    let shallowOk =
        match diploma with
        | "A" -> true
        | _ -> false

    let minMinutes =
        match diploma with
        | "A" -> 1
        | "B" -> 10
        | _ -> 15

    let filter (session: Session) = (Deep.boolValue session.Deep || shallowOk) && (SessionLength.intValue session.Minutes >= minMinutes) && (Name.stringValue session.Name = name)
    dataAccess.GetEligebleSessions filter

let filterSessions (dataAccess: ISessionDataAccess) filter =
    dataAccess.GetEligebleSessions filter

let getTotalEligebleSessions (dataAccess: ISessionDataAccess) (name: string) (diploma: string) =
    getEligibleSessions dataAccess name diploma
    |> Seq.map (fun session -> SessionLength.intValue session.Minutes)
    |> Seq.sum
