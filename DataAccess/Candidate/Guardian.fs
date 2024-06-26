﻿module DataAccess.Guardian

open DataAccess.Database
open DataAccess.Store
open Application.Guardian
open Application.Errors
open Model
open Model.Common

type GuardianDataAccess(store: Store) =
    interface IGuardianDataAccess with

        member this.GetAllGuardians () =
            InMemoryDatabase.all store.guardians

        member this.StoreGuardian (guardian: Guardian) =
            match InMemoryDatabase.insert (Identifier.stringValue guardian.Id) guardian store.guardians with
            | Ok _ -> Ok ()
            | Error _ -> Error (InsertError "Could not store guardian")

        member this.UpdateGuardian (guardian: Guardian) =
            InMemoryDatabase.update (Identifier.stringValue guardian.Id) guardian store.guardians

        member this.GetGuardian (id: string) =
            InMemoryDatabase.lookup id store.guardians


