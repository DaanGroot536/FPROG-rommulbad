module Service.Web

open DataAccess.Database
open DataAccess.Store
open Giraffe
open Model
open Model.Common
open Thoth.Json.Net
open Thoth.Json.Giraffe
open Service.Serialization
open Application.Candidate
open Application.Session
open Application.Guardian
open DataAccess.Candidate
open DataAccess.Session
open DataAccess.Guardian

//TODO: move all things that use model stuff to the application layer
let getCandidates: HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()
            let candidateAccess = CandidateDataAccess(store)
            let candidates = Application.Candidate.getAllCandidates candidateAccess

            return! ThothSerializer.RespondJsonSeq candidates encoderCandidate next ctx
        }

let getGuardians: HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let guardians =
                InMemoryDatabase.all store.guardians
                |> Seq.map id 

            return! ThothSerializer.RespondJsonSeq guardians encoderGuardian next ctx
        }

let getCandidate (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()
            let candidateAccess = CandidateDataAccess(store)

            let candidate = Application.Candidate.getCandidate candidateAccess name
            match candidate with
            | Some candidate ->
                    return! ThothSerializer.RespondJson candidate encoderCandidate next ctx
            | None -> return! RequestErrors.NOT_FOUND "Employee not found!" next ctx
        }

let addSession (name: string) : HttpHandler =
    fun next ctx ->
        task {

            let! session = ThothSerializer.ReadBody ctx decoderSession

            match session with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok session ->
                let store = ctx.GetService<Store>()
                let dataAccess = SessioneDataAccess(store)
                match Application.Session.storeSession dataAccess session with
                | Ok msg -> return! text "Session added successfully" next ctx
                | _ -> return! text "Failed to store Session" next ctx
        }

let addGuardian : HttpHandler =
    fun next ctx ->
        task {
            let! guardianResult = ThothSerializer.ReadBody ctx decoderGuardian

            match guardianResult with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok guardian ->
                let store = ctx.GetService<Store>()
                let dataAccess = GuardianDataAccess(store)
                match Application.Guardian.storeGuardian dataAccess guardian with
                | Ok msg -> return! text "Guardian added successfully" next ctx
                | _ -> return! text "Failed to store Guardian" next ctx
                
        }

let addCandidate : HttpHandler =
    fun next ctx ->
        task {
            let! candidateResult = ThothSerializer.ReadBody ctx decoderCandidate

            match candidateResult with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok candidate ->
                let store = ctx.GetService<Store>()
                let dataAccess = CandidateDataAccess(store)
                match Application.Candidate.storeCandidate dataAccess candidate with
                | Ok msg -> return! text "Candidate added successfully" next ctx
                | _ -> return! text "Failed to store Candidate" next ctx
                
        }

let assignCandidate (rawCandidateName: string, rawGuardianId: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()
            let dataAccessC = CandidateDataAccess(store)
            let dataAccessG = GuardianDataAccess(store)
            match Application.Guardian.assignCandidate dataAccessG dataAccessC rawGuardianId rawCandidateName with
            | Ok msg -> return! text "Candidate assigned successfully" next ctx
            | _ -> return! RequestErrors.NOT_FOUND "Guardian or candidate not found" next ctx
        }



let encodeSession (_, deep, date, minutes) =
    Encode.object
        [ "date", Encode.datetime date
          "deep", Encode.bool deep
          "minutes", Encode.int minutes ]


let getSessions (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()
            let dataAccess = SessioneDataAccess(store)

            let sessions = 
                Application.Session.getSessionsByName dataAccess name
                |> Seq.map id

            return! ThothSerializer.RespondJsonSeq sessions encoderSession next ctx
        }

let getTotalMinutes (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()
            let dataAccess = SessioneDataAccess(store)

            let total =
                Application.Session.getTotalMinutes dataAccess name

            return! ThothSerializer.RespondJson total Encode.int next ctx
        }


let getEligibleSessions (name: string, diploma: string) : HttpHandler =
    fun next ctx ->
        task {

            let store = ctx.GetService<Store>()
            let dataAccess = SessioneDataAccess(store)
            
            let sessions =
                Application.Session.getEligibleSessions dataAccess name diploma

            return! ThothSerializer.RespondJsonSeq sessions encoderSession next ctx
        }

let getTotalEligibleMinutes (name: string, diploma: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let dataAccess = SessioneDataAccess(store)
            
            let total =
                Application.Session.getTotalEligebleSessions dataAccess name diploma

            return! ThothSerializer.RespondJson total Encode.int next ctx
        }

let awardDiploma (rawCandidateName: string, rawDiploma: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let candidateOpt = InMemoryDatabase.lookup rawCandidateName store.candidates

            match candidateOpt with
            | Some candidate ->
                let shallowOk, minMinutes, totalRequired =
                    match rawDiploma with
                    | "A" -> true, 1, 120
                    | "B" -> false, 10, 150
                    | "C" -> false, 15, 180
                    | _ -> false, 0, 0

                if totalRequired = 0 then
                    return! RequestErrors.BAD_REQUEST "Invalid diploma type" next ctx
                else
                    let filter (session: Session) =
                        (Deep.boolValue session.Deep || shallowOk)
                        && (SessionLength.intValue session.Minutes >= minMinutes)
                        && (Name.stringValue session.Name = rawCandidateName)

                    let totalMinutes =
                        InMemoryDatabase.filter filter store.sessions
                        |> Seq.map (fun session -> SessionLength.intValue session.Minutes)
                        |> Seq.sum

                    let newDiploma =
                        match Diploma.make rawDiploma with
                        | Ok diploma -> diploma

                    let updatedCandidate =
                        { candidate with Diploma = Some (newDiploma) }

                    if totalMinutes >= totalRequired then
                        match InMemoryDatabase.update (Name.stringValue candidate.Name) updatedCandidate store.candidates with
                        | _ -> return! text $"Diploma {rawDiploma} awarded successfully" next ctx

                    else
                        return! RequestErrors.FORBIDDEN "Not enough eligible minutes for the diploma" next ctx

            | None ->
                return! RequestErrors.NOT_FOUND "Candidate not found!" next ctx
        }



let routes: HttpHandler =
    choose
        [ 
          //Candidates
          GET >=> route "/candidate" >=> getCandidates
          POST >=> route "/candidate" >=> addCandidate
          GET >=> routef "/candidate/%s" getCandidate
          POST >=> routef "/candidate/%s/award/%s" awardDiploma

          //Sessions
          POST >=> routef "/candidate/%s/session" addSession
          GET >=> routef "/candidate/%s/session" getSessions
          GET >=> routef "/candidate/%s/session/total" getTotalMinutes
          GET >=> routef "/candidate/%s/session/%s" getEligibleSessions
          GET >=> routef "/candidate/%s/session/%s/total" getTotalEligibleMinutes

          //Guardians
          GET >=> route "/guardian" >=> getGuardians
          POST >=> route "/guardian" >=> addGuardian
          GET >=> routef "/guardian/%s/assigncandidate/%s" assignCandidate
          ]
