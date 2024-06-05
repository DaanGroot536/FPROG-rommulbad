module Service.Serialization

open Thoth.Json.Net
open Model
open Common

type DecoderError =
    | Custom of string

// Serialize Common
let decodeName (name: string) : Result<Name, DecoderError> =
    match Name.make name with
    | Ok name -> Ok name
    | Error validationMessage -> Error (DecoderError.Custom validationMessage)

let encoderName: Encoder<Name> = fun (Name name) ->
    Encode.string name

let decodeIdentifier (identifier: string): Result<Identifier, DecoderError> =
    match Identifier.make identifier with
    | Ok id -> Ok id
    | Error validationMessage -> Error (DecoderError.Custom validationMessage)

let decodeDiploma (diploma: string): Result<Diploma, DecoderError> =
    match Diploma.make diploma with
    | Ok dpl -> Ok dpl
    | Error validationMessage -> Error (DecoderError.Custom validationMessage)

let decoderSessionDate: Decoder<SessionDate> =
    Decode.datetime
    |> Decode.andThen (fun d ->
        match SessionDate.make d with
        | Ok sessionDate -> Decode.succeed sessionDate
        | Error validationMessage -> Decode.fail validationMessage
    )

let encoderSessionDate: Encoder<SessionDate> = fun (SessionDate date) ->
    Encode.datetime date

let decoderSessionLength: Decoder<SessionLength> = 
    Decode.int
    |> Decode.andThen (fun i ->
        match SessionLength.make i with
        | Ok sessionLength -> Decode.succeed sessionLength
        | Error validationMessage -> Decode.fail validationMessage
    )

let encoderSessionLength: Encoder<SessionLength> = fun (SessionLength length) ->
    Encode.int length

let decoderDeep: Decoder<Deep> =
    Decode.string
    |> Decode.andThen (fun b ->
        match Deep.make b with
        | Ok deep -> Decode.succeed deep
        | Error validationMessage -> Decode.fail validationMessage
    )

let encoderDeep: Encoder<Deep> = Deep.encoderValue >> Encode.string

// Serialize Candidate

let decoderName : Decoder<Name> =
    Decode.string |> Decode.andThen (fun name ->
        match Name.make name with
        | Ok decodedName -> Decode.succeed decodedName
        | Error errorMessage -> Decode.fail errorMessage
    )

let decoderIdentifier : Decoder<Identifier> =
    Decode.string |> Decode.andThen (fun identifier ->
        match Identifier.make identifier with
        | Ok decodedId -> Decode.succeed decodedId
        | Error errorMessage -> Decode.fail errorMessage
    )

let decoderDiploma : Decoder<Diploma> =
    Decode.string |> Decode.andThen (fun diploma ->
        match Diploma.make diploma with
        | Ok decodedDiploma -> Decode.succeed decodedDiploma
        | Error errorMessage -> Decode.fail errorMessage
    )

let encoderCandidate: Encoder<Candidate> = fun candidate ->
    Encode.object [
        "name", encoderName candidate.Name
        "guardianId", (Encode.option (fun (Identifier indentifier) -> Encode.string indentifier) candidate.GuardianId)
        "diploma", (Encode.option (fun (Diploma diploma) -> Encode.string diploma) candidate.Diploma)
    ]

let decoderCandidate: Decoder<Candidate> =
    Decode.object (fun get -> 
        let name = get.Required.Field "name" decoderName
        let guardianId = get.Optional.Field "guardianId" decoderIdentifier
        let diploma = get.Optional.Field "diploma" decoderDiploma
        { Name = name; GuardianId = guardianId; Diploma = diploma }
    )

// Serialize Session
let encoderSession: Encoder<Session> = fun session ->
    Encode.object [
        "deep", encoderDeep session.Deep
        "sessionDate", encoderSessionDate session.Date
        "sessionLength", encoderSessionLength session.Minutes
    ]

let decoderSession: Decoder<Session> = 
    Decode.object (fun get ->
        let deep = get.Required.Field "deep" decoderDeep
        let date = get.Required.Field "sessionDate" decoderSessionDate
        let length = get.Required.Field "sessionLength" decoderSessionLength
        { Deep = deep; Date = date; Minutes = length }
    )