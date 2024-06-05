﻿namespace Model

open Thoth.Json.Net
open Common

/// A guardian has an Id (3 digits followed by a dash and 4 letters),
/// a Name (only letters and spaces, but cannot contain two or more consecutive spaces),
/// and a list of Candidates (which may be empty)
type Guardian =
    { Id: Identifier
      Name: Name
      Candidates: List<Candidate> }