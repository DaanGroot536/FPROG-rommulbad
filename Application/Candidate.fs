module Application.Candidate

open Model
open Model.Common

type ICandidateDataAccess =
    abstract GetAllCandidates : unit -> List<Candidate>
    abstract StoreCandidate : Candidate -> Result<unit, 'TError>

let getAllCandidates (dataAccess: ICandidateDataAccess) =
    dataAccess.GetAllCandidates()

let storeCandidate (dataAccess: ICandidateDataAccess) candidate =
    dataAccess.StoreCandidate(candidate)
    
