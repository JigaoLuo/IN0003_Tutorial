open Thread

(* wait a line form console, then repeat it *)
(* let echo () = print_string (read_line () ^ "\n");; *)

(* main function with many ;  *)
(* creates a Thread, which take the echo function to work with *)
(* let main = let t1 = create echo ()
            in join t1; (** current thrad is blocked and t1 starts **)
          print_int (id (self ()));
          print_string ": that is our Thread id\n" *)

open Event (* creation of channels, sending and receiving *)

(* let thread_function ch = let x = sync (receive ch)
                    in print_string (x ^ "\n");
                    sync (send ch " - receving done!");;
            
let main = 
  let ch = new_channel () 
  in 
  create thread_function ch;
  print_string "main is running ...\n";
  sync (send ch "Message from main to thread_function  -  sending done!");  (** Send string to channel ch, another thread waits for this string**)
  print_string ("Message fomr thread_function to main " ^ sync (receive ch) ^ "\n");; *)

(* deadlock *)
(* let main = 
  let ch = new_channel () 
  in 
  let new_thread = create thread_function ch
  in
  print_string "main is running ...\n";
  join new_thread;
  sync (send ch "Message from main to thread_function  -  sending done!");
  print_string ("Message fomr thread_function to main " ^ sync (receive ch) ^ "\n");; *)


(* let spawn_counter n =
  let rec count i =
    if i <= n then
      let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i 
      in

      print_endline s;  (** Buffer flushing , see SOLUTION**)
    count (i+1)
  in
  create count 0;;

let run_counters m n =
  let counters = List.init m (fun _ -> spawn_counter n) in  (** a list of thread **)
  List.iter join counters; (**conmment this out *)
  print_endline "end";;

run_counters 3 10;; *)


(* version 2: main thread orchestration *)
(* let spawn_counter n ch =
  let rec count i =
    let _ = sync (receive ch) in
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i 
    in
    print_endline s;
    if i < n then
      (sync (send ch true);
      count (i+1))
    else
      (sync (send ch false))
  in
  create count 0

let run_counters m n =
  let channels = List.init m (fun _ -> new_channel ()) in
  let counters = List.map (spawn_counter n) channels in (** every thread has a channel*)
  let rec run l = 
    match l with [] -> ()
              | c::cs -> sync (send c true);  (** I still have element in list *)
                         let chans = if sync (receive c) then cs @ [c] else cs 
                         in
                         run chans
  in
  run channels;
  List.iter join counters;
  print_endline "end"

let _ = run_counters 3 10 *)


(* 13.2 *)
type blog = string list
type user = string
type pass = string
type message = 
    Post of user * pass * string
  | Read of user * blog channel
type t = message channel

(* begin solution *)
let start_server users =
  let c = new_channel () 
  in
    let rec server_fun blogs =
      let get_blog user =  (**match user name and blogs - like hash table - blogs are just string list *)
                match List.assoc_opt user blogs with
                  None -> [] 
                | Some b' -> b' 
      in
      match sync (receive c) with (**listen to channel *)
              | Post (user, pass, text) ->
                if List.assoc_opt user users = Some pass  (** matched then login *)
                then
                  server_fun ((user, get_blog user @ [text])::List.remove_assoc user blogs)  (**update the blog, no dulicates *)
                else server_fun blogs (**stay unchanged *)
              | Read (user, answer_c) ->
                sync (send answer_c (get_blog user));
                server_fun blogs
    in
    let _ = create server_fun [] 
    in
    c

let post server_c u p t =
  sync (send server_c (Post (u, p, t)))

let read server_c u =
  let answer_c = new_channel () in
  sync (send server_c (Read (u, answer_c)));  (**please send blog to this ansewer channel *)
  sync (receive answer_c)   (** which I listen to *)

(* end solution *)
let test =
  let s = start_server [("userA", "passA"); ("userB", "passB")] in
  post s "userB" "passB" "Welcome to my OCaml blog.";
  post s "userA" "passA" "My name is A and I'm starting my own blog!";
  post s "userB" "12345" "I am a hacker attacking B's blog now!";
  post s "userB" "passB" "You can have threads in OCaml!";
  read s "userB"