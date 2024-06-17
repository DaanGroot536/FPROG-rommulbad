module Model.Validation

open System
open System.Text.RegularExpressions

let nonEmpty invalid s = if String.IsNullOrWhiteSpace s then Error invalid else Ok s

let alphaNumeric invalid s = if String.forall Char.IsLetterOrDigit s then Ok s else Error invalid

let matches (re : Regex) invalid (s: string) = if re.IsMatch s then Ok s else Error invalid
