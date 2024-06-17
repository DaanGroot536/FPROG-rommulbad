module Application.Guardian

open Model
open Model.Common
open Application.Candidate

type IGuardianDataAccess =
    abstract GetAllGuardians : unit -> Guardian seq
    abstract GetGuardian : string -> Option<Guardian>
    abstract StoreGuardian : Guardian -> Result<unit, 'TError>
    abstract UpdateGuardian : Guardian -> unit

let getAllGuardians (dataAccess: IGuardianDataAccess) =
    dataAccess.GetAllGuardians()
    |> Seq.toList

let getGuardian (dataAccess: IGuardianDataAccess) (id: string) =
    dataAccess.GetGuardian(id)

let storeGuardian (dataAccess: IGuardianDataAccess) guardian =
    dataAccess.StoreGuardian(guardian)

let updateGuardian (dataAccess: IGuardianDataAccess) guardian =
    dataAccess.UpdateGuardian(guardian)

let assignCandidate (dataAccess: IGuardianDataAccess) (dataAccessC: ICandidateDataAccess) (id: string) (name: string) =
    let guardian = getGuardian dataAccess id
    let candidate = getCandidate dataAccessC name
    match candidate, guardian with
    | Some candidate, Some guardian ->
        let updatedCandidates = 
            match guardian.Candidates with
            | Some candidates -> Some (candidate :: candidates)
            | None -> Some [candidate]

        let updatedGuardian =
            { guardian with Candidates = updatedCandidates }
        match updateGuardian dataAccess updatedGuardian with
        | _ -> Ok ()
    | _ -> Error ()

    
