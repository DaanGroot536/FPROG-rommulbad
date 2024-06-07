module DataAccess.Session

open DataAccess.Database
open DataAccess.Store
open Application.Session
open Model.Common
open Model

/// Data access operations of the Stock component implemented using the simulated in-memory DB.
let databasePersistence (store: Store) = 
    { new IStore with
        member this.RetrieveAllCandidates() =
            InMemoryDatabase.all store.candidates

        member this.GetCandidate (name: string) =
            InMemoryDatabase.lookup name store.candidates

        member this.GetSessions (name: string) =
            InMemoryDatabase.filter (fun (n, _, _, _) -> n = name) store.sessions

        member _.GetTotalMinutes(candidateName: string) =
            InMemoryDatabase.all store.sessions
            |> Seq.filter (fun ((name, _), _) -> name = candidateName)
            |> Seq.sumBy (fun (_, (_, _, _, minutes)) -> minutes)

        member _.GetEligibleSessions(candidateName: string, _sessionName: string) =
            InMemoryDatabase.all store.sessions
            |> Seq.filter (fun ((name, _), (_, deep, _, _)) -> name = candidateName && deep)
            |> Seq.map (fun (_, (_, deep, date, minutes)) -> { Deep = deep; Date = date; Minutes = minutes })
            |> Seq.toList

        member _.GetTotalEligibleMinutes(candidateName: string, _sessionName: string) =
            InMemoryDatabase.all store.sessions
            |> Seq.filter (fun ((name, _), (_, deep, _, _)) -> name = candidateName && deep)
            |> Seq.sumBy (fun (_, (_, _, _, minutes)) -> minutes)
    }
