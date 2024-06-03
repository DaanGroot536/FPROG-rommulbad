module Rommulbad.Store

open System
open Rommulbad.Database

/// Here a store is created that contains the following tables with the following attributes
///
/// candidates (primary key is name)
/// - name (consists of words seperated by spaces)
/// - date of birth
/// - guardian id (see guardian id)
/// - highest swimming diploma (A, B, or C, with C being the highest)
///
/// sessions (primary key is compound: candidate name and date)
/// - candidate name (foreign key to employees)
/// - date
/// - minutes(int)
///
/// guardians
/// - id (3 digits followed by dash and 4 letters, e.g. 133-LEET)
/// - name (consists of words separated by spaces)
type Store() =
    member val candidates: InMemoryDatabase<string, string * DateTime * string * string> =
        [ "Eleanor", DateTime(2016, 1, 9), "123-ABCD", "A"
          "Camiel", DateTime(2015, 11, 3), "123-ABCD", "C"
          "Lore", DateTime(2018, 8, 30), "9999-ZZZ", "" ]
        |> Seq.map (fun (n, bd, gi, dpl) -> n, (n, bd, gi, dpl))
        |> InMemoryDatabase.ofSeq

    member val sessions: InMemoryDatabase<string * DateTime, string * bool * DateTime * int> =
        [ "Eleanor", false, DateTime(2024, 2, 2), 3
          "Eleanor", false, DateTime(2024, 3, 2), 5
          "Eleanor", false, DateTime(2024, 3, 2), 10
          "Eleanor", true, DateTime(2024, 4, 1), 30
          "Eleanor", true, DateTime(2024, 5, 2), 10
          "Eleanor", true, DateTime(2024, 5, 3), 15
          "Camiel", false, DateTime(2023, 4, 10), 15
          "Camiel", true, DateTime(2023, 4, 17), 10
          "Camiel", true, DateTime(2023, 5, 24), 20
          "Camiel", true, DateTime(2023, 5, 14), 10
          "Camiel", true, DateTime(2023, 6, 13), 20
          "Camiel", true, DateTime(2023, 6, 17), 10
          "Camiel", true, DateTime(2023, 7, 10), 20
          "Camiel", true, DateTime(2023, 7, 17), 10
          "Camiel", true, DateTime(2023, 8, 10), 20
          "Camiel", true, DateTime(2023, 8, 17), 10
          "Camiel", true, DateTime(2023, 9, 10), 20
          "Camiel", true, DateTime(2023, 9, 17), 10
          "Camiel", true, DateTime(2023, 10, 10), 20
          "Camiel", true, DateTime(2023, 10, 17), 10
          "Camiel", true, DateTime(2023, 11, 10), 20
          "Camiel", true, DateTime(2023, 11, 17), 10
          "Camiel", true, DateTime(2023, 12, 10), 20
          "Camiel", true, DateTime(2023, 12, 17), 10
          "Lore", false, DateTime(2024, 6, 3), 1
          "Lore", false, DateTime(2024, 6, 10), 5 ]
        |> Seq.map (fun (n, deep, date, min) -> (n, date), (n, deep, date, min))
        |> InMemoryDatabase.ofSeq

    member val guardians: InMemoryDatabase<string, string * string> =
        [ "123-ABCD", "Jan Janssen"
          "234-FDEG", "Marie Moor"
          "999-ZZZZ", "Margeet van Lankerveld" ]
        |> Seq.map (fun t -> fst t, t)
        |> InMemoryDatabase.ofSeq
