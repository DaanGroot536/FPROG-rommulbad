module Application.Session

open Model
open Model.Common

type ISessionDataAccess =
    abstract GetAllSessions : unit -> List<Session>
    abstract StoreSession : Session -> Result<unit, 'TError>

let getAllSessions (dataAccess: ISessionDataAccess) =
    dataAccess.GetAllSessions()

let storeSession (dataAccess: ISessionDataAccess) session =
    dataAccess.StoreSession(session)
