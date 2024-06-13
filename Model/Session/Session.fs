namespace Model

open Thoth.Json.Net
open Thoth.Json.Giraffe
open System
open Common

/// Swimming session registered on a specific date
///
/// A Swimming session can be in the deep or shallow pool
/// Minutes cannot be negative or larger than 30
/// Only the year, month and date of Date are used.
type Session =
    { Name: Name
      Deep: Deep
      Date: SessionDate
      Minutes: SessionLength }