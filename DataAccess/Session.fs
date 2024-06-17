module DataAccess.Session

open DataAccess.Database
open DataAccess.Store
open Application.Session
open Model
open Model.Common

type SessioneDataAccess(store: Store) =
    interface ISessionDataAccess with

        member this.GetAllSessions () =
            InMemoryDatabase.all store.sessions
            |> Seq.toList

        member this.StoreSession (session: Session) =
            match InMemoryDatabase.insert (Name.stringValue session.Name, SessionDate.dateTimeValue session.Date) session store.sessions with
            | Ok _ -> Ok ()
