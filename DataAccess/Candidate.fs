module DataAccess.Candidate

open DataAccess.Database
open DataAccess.Store
open Application.Candidate
open Model
open Model.Common

type CandidateDataAccess(store: Store) =
    interface ICandidateDataAccess with

        member this.GetAllCandidates () =
            InMemoryDatabase.all store.candidates
            |> Seq.toList

        member this.StoreCandidate (candidate: Candidate) =
            match InMemoryDatabase.insert (Name.stringValue candidate.Name) candidate store.candidates with
            | Ok _ -> Ok ()
