module DataAccess.Guardian

open DataAccess.Database
open DataAccess.Store
open Application.Guardian
open Model
open Model.Common

type GuardianDataAccess(store: Store) =
    interface IGuardianDataAccess with

        member this.GetAllGuardians () =
            InMemoryDatabase.all store.guardians
            |> Seq.toList

        member this.StoreGuardian (guardian: Guardian) =
            match InMemoryDatabase.insert (Identifier.stringValue guardian.Id) guardian store.guardians with
            | Ok _ -> Ok ()
