open Thread
open Event


(* 13.1 *)
let spawn_counter n =
  let rec count i =
    if i > n then
    ()
      (*delay 0.1*)
     else
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
    count (i+1)
  in
  create count 0


let run_counters m n = failwith "todo"

(* 
(* 13.2 *)
type blog = string list
type user = string
type pass = string
type message = Post of user * pass * string
  | Read of user * blog channel
type t = message channel

(* begin solution *)
let start_server users = failwith "todo"

let post s u p t = failwith "tood"

let read s u = failwith "todo"

(* end solution *)
let test =
  let s = start_server [("userA", "passA"); ("userB", "passB")] in
  post s "userB" "passB" "Welcome to my OCaml blog.";
  post s "userA" "passA" "My name is A and I'm starting my own blog!";
  post s "userB" "12345" "I am a hacker attacking B's blog now!";
  post s "userB" "passB" "You can have threads in OCaml!";
  read s "userB"

