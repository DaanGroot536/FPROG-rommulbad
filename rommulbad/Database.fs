/// ====================================
/// ==== DO NOT CHANGE THIS FILE    ====
/// ====                            ====
/// ==== You do not have to alter   ====
/// ==== this file for the          ====
/// ==== assessment                 ====
/// ====================================
namespace Rommulbad.Database

type InsertError = UniquenessError of string

type InMemoryDatabase<'Key, 'T when 'Key: comparison> = private { mutable Data: Map<'Key, 'T> }

module InMemoryDatabase =
    let ofMap (m: Map<'Key, 'T>) : InMemoryDatabase<'Key, 'T> = { InMemoryDatabase.Data = m }
    let ofSeq (s: seq<'Key * 'T>) : InMemoryDatabase<'Key, 'T> = s |> Map.ofSeq |> ofMap


    let lookup key store : Option<'T> = Map.tryFind key store.Data
    let unsafeLookup key store : 'T = Map.find key store.Data

    let insert (key: 'Key) value store : Result<unit, InsertError> =
        if Map.containsKey key store.Data then
            sprintf "Key %O taken" key |> UniquenessError |> Error
        else
            store.Data <- Map.add key value store.Data
            Ok()

    let delete key store =
        store.Data <- store.Data |> Map.remove key


    let update key value store =
        store.Data <-
            store.Data
            |> Map.change key (fun vo ->
                match vo with
                | None -> None
                | Some _ -> Some value)

    let all store = Map.values store.Data

    let filter (pred: ('T -> bool)) store = all store |> Seq.filter pred
