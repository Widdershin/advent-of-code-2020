//https://adventofcode.com/2020/day/1

// Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

// For example, suppose your expense report contained the following:

// 1721
// 979
// 366
// 299
// 675
// 1456

// In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

// read some stuff from a file

open System.IO
open System.Text;

let isNotEmpty s = String.length s > 0

let lines = File.ReadAllLines "./advent1.txt" |> Array.filter isNotEmpty
let ints = Array.map int lines

let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }

let formatPair pair =
    pair |> List.map (sprintf "%i") |> String.concat ","

let pairAddsToGoal (pair: int list) =
    List.sum pair = 2020
 
let pairs : seq<int list> = combinations [] 3 (Array.toList ints)
let matchingPair = Seq.find pairAddsToGoal pairs

let pairsList : int list list = Seq.toList pairs
let pairsAsText = List.map formatPair pairsList

let x = List.reduce (fun x y -> x * y) matchingPair
printfn "%i" x