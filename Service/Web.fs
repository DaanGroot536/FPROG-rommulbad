module Service.Web

open DataAccess.Database
open DataAccess.Store
open Giraffe
open Model
open Model.Common
open Thoth.Json.Net
open Thoth.Json.Giraffe
open Service.Serialization

//TODO: move all things that use model stuff to the application layer
let getCandidates: HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let candidates =
                InMemoryDatabase.all store.candidates
                |> Seq.map id 

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

            let candidate =
                InMemoryDatabase.lookup name store.candidates
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
            | Ok { Deep = deep
                   Date = date
                   Minutes = minutes } ->
                let store = ctx.GetService<Store>()

                InMemoryDatabase.insert (name, SessionDate.dateTimeValue date) {Name = Name name; Deep = deep; Date = date; Minutes = minutes} store.sessions
                |> ignore


                return! text "OK" next ctx
        }

let addGuardian : HttpHandler =
    fun next ctx ->
        task {
            let! guardianResult = ThothSerializer.ReadBody ctx decoderGuardian

            match guardianResult with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok guardian ->
                let store = ctx.GetService<Store>()

                InMemoryDatabase.insert (Identifier.stringValue guardian.Id) guardian store.guardians
                |> ignore

                return! text "Guardian added successfully" next ctx
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

            let sessions = 
                InMemoryDatabase.all store.sessions
                |> Seq.map id

            return! ThothSerializer.RespondJsonSeq sessions encoderSession next ctx
        }

let getTotalMinutes (name: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let total =
                InMemoryDatabase.filter (fun session -> session.Name = Name name) store.sessions
                |> Seq.map (fun session -> SessionLength.intValue session.Minutes)
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

            let filter (session: Session) = (Deep.boolValue session.Deep || shallowOk) && (SessionLength.intValue session.Minutes >= minMinutes) && (Name.stringValue session.Name = name)

            let sessions = InMemoryDatabase.filter filter store.sessions

            return! ThothSerializer.RespondJsonSeq sessions encoderSession next ctx
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

            let filter (session: Session) = (Deep.boolValue session.Deep || shallowOk) && (SessionLength.intValue session.Minutes >= minMinutes) && (Name.stringValue session.Name = name)


            let total =
                InMemoryDatabase.filter filter store.sessions
                |> Seq.map (fun session -> SessionLength.intValue session.Minutes)
                |> Seq.sum

            return! ThothSerializer.RespondJson total Encode.int next ctx
        }



let routes: HttpHandler =
    choose
        [ GET >=> route "/candidate" >=> getCandidates
          GET >=> routef "/candidate/%s" getCandidate
          GET >=> route "/guardian" >=> getGuardians
          POST >=> route "/guardian" >=> addGuardian
          //POST >=> routef "/candidate/%s/award/%s" >=> awardDiploma
          POST >=> routef "/candidate/%s/session" addSession
          GET >=> routef "/candidate/%s/session" getSessions
          GET >=> routef "/candidate/%s/session/total" getTotalMinutes
          GET >=> routef "/candidate/%s/session/%s" getEligibleSessions
          GET >=> routef "/candidate/%s/session/%s/total" getTotalEligibleMinutes ]
