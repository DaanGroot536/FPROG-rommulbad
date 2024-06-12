module Service.Web

open DataAccess.Database
open DataAccess.Store
open Giraffe
open Model
open Model.Common
open Thoth.Json.Net
open Thoth.Json.Giraffe
open Service.Serialization
open Application.Session

//TODO: move all things that use model stuff to the application layer
let getCandidates: HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<IStore>()

            let candidates =
                InMemoryDatabase.all store.candidates
                |> Seq.map id 

            return! ThothSerializer.RespondJsonSeq candidates encoderCandidate next ctx
        }

let getCandidate (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<IStore>()

            let candidate =
                getCandidate store name

            match candidate with
            | None -> return! RequestErrors.NOT_FOUND "Employee not found!" next ctx
            | Some(name, _, gId, dpl) ->
                let decodedName = decodeName name
                let decodedGId = decodeIdentifier gId
                let decodedDpl = decodeDiploma dpl

                match decodedName, decodedGId, decodedDpl with
                | Ok name, Ok gId, Ok dpl ->
                    let candidate = { Candidate.Name = name; GuardianId = Some gId; Diploma = Some dpl }
                    return! ThothSerializer.RespondJson candidate encoderCandidate next ctx
                | _ ->
                    return! RequestErrors.BAD_REQUEST "Invalid data for candidate!" next ctx
        }

let addSession (name: string) : HttpHandler =
    fun next ctx ->
        task {

            let! session = ThothSerializer.ReadBody ctx decoderSession

            match session with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok { Deep = deep
                   Date = date
                   Minutes = minutes } ->
                let store = ctx.GetService<Store>()

                InMemoryDatabase.insert (name, SessionDate.dateTimeValue date) (name, Deep.boolValue deep, SessionDate.dateTimeValue date, SessionLength.intValue minutes) store.sessions
                |> ignore


                return! text "OK" next ctx
        }

let encodeSession (_, deep, date, minutes) =
    Encode.object
        [ "date", Encode.datetime date
          "deep", Encode.bool deep
          "minutes", Encode.int minutes ]


let getSessions (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<IStore>()

            let sessions = 
                getSessions store
                |> Seq.map (fun (name, deep, date, length) ->
                    let decodedNameResult = decodeName name
                    let decodedDeepResult = decoderDeep deep
                    let decodedDateResult = decoderSessionDate date
                    let decodedLengthResult = decoderSessionLength length

                    match decodedNameResult, decodedDeepResult, decodedDplResult with
                    | Ok decodedName, Ok decodedGId, Ok decodedDpl ->
                        Some { Session.Name = decodedName; GuardianId = Some decodedGId; Diploma = Some decodedDpl }
                    | _ -> None
                )
                |> Seq.choose id

            return! ThothSerializer.RespondJsonSeq sessions encodeSession next ctx
        }

let getTotalMinutes (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let total =
                InMemoryDatabase.filter (fun (n, _, _, _) -> n = name) store.sessions
                |> Seq.map (fun (_, _, _, a) -> a)
                |> Seq.sum

            return! ThothSerializer.RespondJson total Encode.int next ctx
        }


let getEligibleSessions (name: string, diploma: string) : HttpHandler =
    fun next ctx ->
        task {

            let store = ctx.GetService<Store>()

            let shallowOk =
                match diploma with
                | "A" -> true
                | _ -> false

            let minMinutes =
                match diploma with
                | "A" -> 1
                | "B" -> 10
                | _ -> 15

            let filter (n, d, _, a) = (d || shallowOk) && (a >= minMinutes)


            let sessions = InMemoryDatabase.filter filter store.sessions

            return! ThothSerializer.RespondJsonSeq sessions encodeSession next ctx

        }

let getTotalEligibleMinutes (name: string, diploma: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let shallowOk =
                match diploma with
                | "A" -> true
                | _ -> false

            let minMinutes =
                match diploma with
                | "A" -> 1
                | "B" -> 10
                | _ -> 15

            let filter (n, d, _, a) = (d || shallowOk) && (a >= minMinutes)


            let total =
                InMemoryDatabase.filter filter store.sessions
                |> Seq.map (fun (_, _, _, a) -> a)
                |> Seq.sum

            return! ThothSerializer.RespondJson total Encode.int next ctx
        }



let routes: HttpHandler =
    choose
        [ GET >=> route "/candidate" >=> getCandidates
          GET >=> routef "/candidate/%s" getCandidate
          //POST >=> routef "/candidate/%s/guardian" >=> addGuardian
          //POST >=> routef "/candidate/%s/award/%s" >=> awardDiploma
          POST >=> routef "/candidate/%s/session" addSession
          GET >=> routef "/candidate/%s/session" getSessions
          GET >=> routef "/candidate/%s/session/total" getTotalMinutes
          GET >=> routef "/candidate/%s/session/%s" getEligibleSessions
          GET >=> routef "/candidate/%s/session/%s/total" getTotalEligibleMinutes ]
