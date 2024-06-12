module Model.Common

open System
open System.Text.RegularExpressions
open Model.Validation

type Identifier = Identifier of string

let (|Identifier|) (Identifier identifier) = identifier

[<RequireQualifiedAccess>]
module Identifier =

    let private identifierPattern = Regex(@"^\d{3}-[A-Z]{4}$")

    let make rawIdentifier =
        rawIdentifier
        |> nonEmpty "Identifier may not be empty."
        |> Result.bind (matches identifierPattern "Identifier must start with three digits followed by a dash and end with four capital letters.")
        |> Result.map Identifier

    let stringValue (Identifier identifier) =
        identifier

type Name = Name of string


let (|Name|) (Name name) = name

[<RequireQualifiedAccess>]
module Name =

    let private namePattern = Regex(@"^[A-Za-z]+( [A-Za-z]+)*$")

    let make rawName =
        rawName
        |> nonEmpty "Name may not be empty."
        |> Result.bind (matches namePattern "Name must consist of words separated by a single space and not end with a space.")
        |> Result.map Name

    let stringValue (Name name) =
        name

type Diploma = Diploma of string

let (|Diploma|) (Diploma diploma) = diploma

[<RequireQualifiedAccess>]
module Diploma =

    let make rawDiploma =
        rawDiploma
        |> nonEmpty "Diploma may not be empty."
        |> Result.map Diploma

    let stringValue (Diploma dpl) =
        dpl

type SessionDate = SessionDate of DateTime

let (|SessionDate|) (SessionDate date) = date

[<RequireQualifiedAccess>]
module SessionDate =

    let make (date: DateTime) =
        Ok (SessionDate date)

    let dateTimeValue (SessionDate date) =
        date

// Length of the session
type SessionLength = SessionLength of int

let (|SessionLength|) (SessionLength length) = length

[<RequireQualifiedAccess>]
module SessionLength =

    let make length =
        if length < 0 then
            Error "Session length cannot be negative."
        elif length > 30 then
            Error "Session length cannot be larger than 30."
        else
            Ok (SessionLength length)

    let intValue (SessionLength length) =
        length

// Indicates the depth of the session
type Deep = Shallow | Deep

let (|DeepCase|) = function
    | Shallow -> "shallow"
    | Deep -> "deep"

[<RequireQualifiedAccess>]
module Deep =

    let make deep =
        match deep with
        | "shallow" -> Ok Shallow
        | "deep" -> Ok Deep
        | _ -> Error "Invalid value for Deep. Valid values are 'shallow' and 'deep'."

    
    let encoderValue: Deep -> string = function
        | Shallow -> "shallow"
        | Deep -> "deep"

    let boolValue (deep: Deep) : bool =
        match deep with
        | Shallow -> false
        | Deep -> true

