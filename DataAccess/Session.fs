module DataAccess.Session

open DataAccess.Database
open Application.Session
open Model

/// Data access operations of the Stock component implemented using the simulated in-memory DB.
let databasePersistence = { new IInMemoryDatabase with
    

}
