module Model.Common

open System
open System.Text.RegularExpressions
open Model.Validation

type Identifier = private | Identifier of string

let (|Identifier|) (Identifier identifier) = identifier

[<RequireQualifiedAccess>]
module Identifier =

    let private identifierPattern = Regex(@"^\d{3}-[A-Z]{4}$")

    // Constructs a correct Identifier given the correct input (string)
    let make rawIdentifier =
        rawIdentifier
        |> nonEmpty "Identifier may not be empty."
        |> Result.bind (matches identifierPattern "Identifier must start with three digits followed by a dash and end with four capital letters.")
        |> Result.map Identifier

type Name = private | Name of string

let (|Name|) (Name name) = name

[<RequireQualifiedAccess>]
module Name =

    let private namePattern = Regex(@"^[A-Za-z]+( [A-Za-z]+)*$")

    let make rawName =
        rawName
        |> nonEmpty "Name may not be empty."
        |> Result.bind (matches namePattern "Name must consist of words separated by a single space and not end with a space.")
        |> Result.map Name

type Diploma = private | Diploma of string

let (|Diploma|) (Diploma diploma) = diploma

[<RequireQualifiedAccess>]
module Diploma =

    let make rawDiploma =
        rawDiploma
        |> nonEmpty "Diploma may not be empty."
        |> Result.map Diploma

type SessionDate = private | SessionDate of DateTime

let (|SessionDate|) (SessionDate date) = date

[<RequireQualifiedAccess>]
module SessionDate =

    let make (date: DateTime) =
        Ok (SessionDate date)

type SessionLength = private | SessionLength of int

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

type Deep = private | Deep of float

let (|Deep|) (Deep deep) = deep

[<RequireQualifiedAccess>]
module Deep =

    let make deep =
        if deep < 0.0 then
            Error "Deep value cannot be negative."
        else
            Ok (Deep deep)
