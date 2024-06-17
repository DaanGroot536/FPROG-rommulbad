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

let awardDiploma (dataAccessC: ICandidateDataAccess) (dataAccessS: ISessionDataAccess) (rawCandidateName: string) (rawDiploma: string) =
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

            let updatedCandidate =
                { candidate with Diploma = Some (newDiploma) }

            if totalMinutes >= totalRequired then
                match dataAccessC.UpdateCandidate updatedCandidate with
                | _ -> Ok ($"Diploma {rawDiploma} awarded successfully")

            else
                Error ("Not enough eligible minutes for the diploma")

    | None ->
        Error ("Candidate not found!")
    
