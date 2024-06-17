module DataAccess.Session

open DataAccess.Database
open DataAccess.Store
open Application.Session
open Application.Errors
open Model
open Model.Common

type SessioneDataAccess(store: Store) =
    interface ISessionDataAccess with

        member this.GetAllSessions () =
            InMemoryDatabase.all store.sessions

        member this.StoreSession (session: Session) =
            match InMemoryDatabase.insert (Name.stringValue session.Name, SessionDate.dateTimeValue session.Date) session store.sessions with
            | Ok _ -> Ok ()
            | Error _ -> Error (InsertError "Could not store session")

        member this.GetSessionsByName (name: string) =
            InMemoryDatabase.filter (fun session -> session.Name = Name name) store.sessions

        member this.GetTotalMinutes (name: string) =
            InMemoryDatabase.filter (fun session -> session.Name = Name name) store.sessions

        member this.GetEligebleSessions filter  =
            InMemoryDatabase.filter filter store.sessions

