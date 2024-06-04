namespace Model

open Thoth.Json.Net
open Thoth.Json.Giraffe
open System

// Names are words separated by spaces
// GuardianId must be a valid guardian id (see below)
// Diploma is the highest diploma obtained by the candidate. It can be
// - an empty string meaning no diploma
// - or the strings "A", "B", or "C".
type Candidate =
    { Name: string
      GuardianId: string
      Diploma: string }


module Candidate =
    let encode: Encoder<Candidate> =
        fun candidate ->
            Encode.object
                [ "name", Encode.string candidate.Name
                  "guardian_id", Encode.string candidate.GuardianId
                  "diploma", Encode.string candidate.Diploma ]

    let decode: Decoder<Candidate> =
        Decode.object (fun get ->
            { Name = get.Required.Field "name" Decode.string
              GuardianId = get.Required.Field "guardian_id" Decode.string
              Diploma = get.Required.Field "diploma" Decode.string })

/// A guardian has an Id (3 digits followed by a dash and 4 letters),
/// a Name (only letters and spaces, but cannot contain two or more consecutive spaces),
/// and a list of Candidates (which may be empty)
type Guardian =
    { Id: string
      Name: string
      Candidates: List<Candidate> }
