open Core

(*
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
 *)

module T = struct
  type t = {
    name:int;
    children:int;
    cats:int;
    samoyeds:int;
    pomeranians:int;
    akitas:int;
    vizslas:int;
    goldfish:int;
    trees:int;
    cars:int;
    perfumes:int;
  }
  [@@deriving sexp, compare, fields]
end

include T
include Comparable.Make(T)

let empty = {
  name=0;
  children=0;
  cats=0;
  samoyeds=0;
  pomeranians=0;
  akitas=0;
  vizslas=0;
  goldfish=0;
  trees=0;
  cars=0;
  perfumes=0;
}

let of_assoc l =
  let find_or_missing l prop =
    List.Assoc.find l ~equal:String.equal prop 
    |> Option.value ~default:(-1)
  in 
  let get = find_or_missing l in
  let children = get "children"
  and cats = get "cats"
  and samoyeds = get "samoyeds"
  and pomeranians = get "pomeranians"
  and akitas = get "akitas"
  and vizslas = get "vizslas"
  and goldfish = get "goldfish"
  and trees = get "trees"
  and cars = get "cars"
  and perfumes = get "perfumes"
  in { name=0;
       children; 
       cats; 
       samoyeds; 
       pomeranians; 
       akitas; 
       vizslas; 
       goldfish;
       trees;
       cars;
       perfumes;
     }