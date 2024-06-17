module Application.Candidate

open Model
open Model.Common
open Application.Session
open Application.Errors

type ICandidateDataAccess =
    abstract GetAllCandidates : unit -> List<Candidate>
    abstract StoreCandidate : Candidate -> Result<unit, StoreError>
    abstract GetCandidate : string -> Option<Candidate>
    abstract UpdateCandidate : Candidate -> unit

let getAllCandidates (dataAccess: ICandidateDataAccess) =
    dataAccess.GetAllCandidates()
    
let storeCandidate (dataAccess: ICandidateDataAccess) candidate =
    dataAccess.StoreCandidate(candidate)

let getCandidate (dataAccess: ICandidateDataAccess) name = 
    dataAccess.GetCandidate(name)

let isEligeble (dataAccessC: ICandidateDataAccess) (dataAccessS: ISessionDataAccess) (rawCandidateName: string) (rawDiploma: string) =
    let candidateOpt = getCandidate dataAccessC rawCandidateName

    match candidateOpt with
    | Some candidate ->
        let shallowOk, minMinutes, totalRequired =
            match rawDiploma with
            | "A" -> true, 1, 120
            | "B" -> false, 10, 150
            | "C" -> false, 15, 180
            | _ -> false, 0, 0

        if totalRequired = 0 then
            Error ("Invalid diploma type")
        else
            let filter (session: Session) =
                (Deep.boolValue session.Deep || shallowOk)
                && (SessionLength.intValue session.Minutes >= minMinutes)
                && (Name.stringValue session.Name = rawCandidateName)

            let totalMinutes =
                Application.Session.filterSessions dataAccessS filter
                |> Seq.map (fun session -> SessionLength.intValue session.Minutes)
                |> Seq.sum

            let newDiploma =
                match Diploma.make rawDiploma with
                | Ok diploma -> diploma



            if totalMinutes >= totalRequired then
                Ok ("Candidate is eligeble for this diploma!")
            else
                Error ("Not enough eligible minutes for the diploma")

    | None ->
        Error ("Candidate not found!")

let getEligebleCandidatesForDiploma (dataAccessC: ICandidateDataAccess) (dataAccessS: ISessionDataAccess) (diploma: string) =
    let candidates = getAllCandidates dataAccessC
    candidates
    |> Seq.filter (fun candidate ->
        match isEligeble dataAccessC dataAccessS (Name.stringValue candidate.Name) diploma with
        | Ok _ -> true
        | Error _ -> false)
    |> Seq.toList

let awardDiploma (dataAccessC: ICandidateDataAccess) (name: string) (diploma: string) =
    let candidateOpt = getCandidate dataAccessC name
    match candidateOpt with
    | Some candidate ->
        let updatedCandidate =
            { candidate with Diploma = Some (Diploma diploma) }
        match dataAccessC.UpdateCandidate updatedCandidate with
        | _ -> Ok ($"Diploma {diploma} awarded successfully")
    | None -> Error ("Candidate not found")



    
    
