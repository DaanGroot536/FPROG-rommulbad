module Application.Guardian

open Model
open Model.Common

type IGuardianDataAccess =
    abstract GetAllGuardians : unit -> List<Guardian>
    abstract StoreGuardian : Guardian -> Result<unit, 'TError>

let getAllGuardians (dataAccess: IGuardianDataAccess) =
    dataAccess.GetAllGuardians()

let storeGuardian (dataAccess: IGuardianDataAccess) guardian =
    dataAccess.StoreGuardian(guardian)
