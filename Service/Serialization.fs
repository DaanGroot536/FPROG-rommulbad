module Service.Serialization

open Thoth.Json.Net
open Model
open Common

// Serialize Common
let decoderName: Decoder<Name> =
    Decode.string
    |> Decode.andThen (fun s -> 
        match Name.make s with
        | Ok name -> Decode.succeed name
        | Error validationMessage -> Decode.fail validationMessage
    )

let decoderIdentifier: Decoder<Identifier> =
    Decode.string
    |> Decode.andThen (fun s -> 
        match Identifier.make s with
        | Ok identifier -> Decode.succeed identifier
        | Error validationMessage -> Decode.fail validationMessage
    )

let decoderDiploma: Decoder<Diploma> =
    Decode.string
    |> Decode.andThen (fun s -> 
        match Diploma.make s with
        | Ok diploma -> Decode.succeed diploma
        | Error validationMessage -> Decode.fail validationMessage
    )



// Serialize Candidate
let encoderCandidate: Encoder<Candidate> = fun candidate ->
    Encode.object [
        "name", (let (Name name) = candidate.Name in Encode.string name)
        "guardian_id", (Encode.option (fun (Identifier indentifier) -> Encode.string indentifier) candidate.GuardianId)
        "diploma", (Encode.option (fun (Diploma diploma) -> Encode.string diploma) candidate.Diploma)
    ]

let decoderCandidate: Decoder<Candidate> =
    Decode.object (fun get -> 
        let name = get.Required.Field "name" decoderName
        let guardianId = get.Optional.Field "guardianId" decoderIdentifier
        let diploma = get.Optional.Field "diploma" decoderDiploma
        { Name = name; GuardianId = guardianId; Diploma = diploma }
    )