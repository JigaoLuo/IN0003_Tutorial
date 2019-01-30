open Thread
open Event

(* 13.1 *)
(* version 1: chaotic *)
let spawn_counter n =
  let rec count i =
    if i > n then
    ()
      (*delay 0.1*)
     else
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
(*    Printf.printf "Thread %2d: %d\n%!" (id (self())) i; *)
    count (i+1)
  in
  create count 0

let run_counters m n =
  let counters = List.init m (fun _ -> spawn_counter n) in
  List.iter join counters;
  print_newline ()

(* let _ = run_counters 10 10000 *)


(* version 2: main thread orchestration *)
let spawn_counter n c =
  let rec count i =
    let _ = sync (receive c) in
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
    if i < n then
      (sync (send c true);
      count (i+1))
    else
      (sync (send c false))
  in
  create count 0

let run_counters m n =
  let channels = List.init m (fun _ -> new_channel ()) in
  let counters = List.map (spawn_counter n) channels in
  let rec run = function [] -> ()
    | c::cs -> sync (send c true);
      let chans = if sync (receive c) then cs @ [c] else cs in
      run chans
  in
  run channels;
  List.iter join counters;
  print_newline ()

(* let _ = run_counters 10 1000 *)


(* version 3: self organization *)
type cmd = Count | Update_recv of cmd channel

let spawn_counter n rc sc =
  let rec count_solo i =
    if i <= n then
      (let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
      print_endline s;
      count_solo (i+1))
    else ()
  in
  let rec impl i rc sc =
    let rc = match sync (receive rc) with Count -> rc
      | Update_recv new_rc -> new_rc
    in
    let s = Printf.sprintf "Thread %2d: %d" (id (self ())) i in
    print_endline s;
    (* NOTE: we use == here to compare the channel references *)
    if rc == sc then count_solo (i+1)
    else if i < n then
      (sync (send sc Count);
      impl (i+1) rc sc)
    else
      sync (send sc (Update_recv rc))
  in
  Thread.create (fun () -> impl 0 rc sc) ()

let run_counters m n =
  let recv_channels = List.init 10 (fun _ -> new_channel ()) in
  let send_channels = (List.tl recv_channels) @ [List.hd recv_channels] in
  let curried = List.map2 spawn_counter (List.init m (fun _ -> n)) recv_channels in
  let threads = List.map2 (fun f sc -> f sc) curried send_channels in
  sync (send (List.hd recv_channels) Count);
  List.iter Thread.join threads;
  print_newline ()

(* let _ = run_counters 10 100 *)


(* 13.2 *)
type blog = string list
type user = string
type pass = string
type message = Post of user * pass * string
  | Read of user * blog channel
type t = message channel

(* begin solution *)
let start_server users =
  let c = new_channel () in
    let rec server_fun blogs =
      let get_blog user = match List.assoc_opt user blogs with
        None -> [] | Some b' -> b' in
      match sync (receive c) with
      | Post (user, pass, text) ->
        if List.assoc_opt user users = Some pass then
          server_fun ((user, get_blog user @ [text])
            ::List.remove_assoc user blogs)
        else server_fun blogs
      | Read (user, answer_c) ->
        sync (send answer_c (get_blog user));
        server_fun blogs
    in
    let _ = create server_fun [] in
    c

let post s u p t =
  sync (send s (Post (u, p, t)))

let read s u =
  let answer_c = new_channel () in
  sync (send s (Read (u, answer_c)));
  sync (receive answer_c)

(* end solution *)
let test =
  let s = start_server [("userA", "passA"); ("userB", "passB")] in
  post s "userB" "passB" "Welcome to my OCaml blog.";
  post s "userA" "passA" "My name is A and I'm starting my own blog!";
  post s "userB" "12345" "I am a hacker attacking B's blog now!";
  post s "userB" "passB" "You can have threads in OCaml!";
  read s "userB"

(* alternative for start_server *)
let (|?) a b = match a with Some x -> x | None -> b
let rec start_server up =
  let open List in
  let c = new_channel () in
  let rec f bs =
    match sync (receive c) with
    | Post (u, p, b) ->
        if assoc_opt u up = Some p then
          f ((u, (assoc_opt u bs |? []) @ [b])::remove_assoc u bs)
        else
          f bs
    | Read (u, rc) ->
        sync (send rc (assoc_opt u bs |? []));
        f bs
  in
  ignore (create f []);
  c


(* 13.3 *)
module Future = struct
  type 'a msg = Result of 'a | Ex of exn
  type 'a t = 'a msg channel

  let create f a =
    let c = new_channel () in
    let task () =
      let r = try Result (f a) with e -> Ex e in
      sync (send c r)
    in
    let _ = Thread.create task () in
    c

  let get c =
    match sync (receive c) with
    | Result r -> r
    | Ex e -> raise e

  let then_ f c =
    let c' = new_channel () in
    let task () =
      let r = match sync (receive c) with
      | Result r -> Result (f r)
      | Ex e -> Ex e
      in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'

  let when_any cs =
    let c' = new_channel () in
    let task () =
      let r = select (List.map receive cs) in
      sync (send c' r)
    in
    let _ = Thread.create task () in
    c'

  let when_all cs =
    let c' = new_channel () in
    let task () =
      let r = List.fold_left (fun a c -> sync (receive c)::a) [] cs |> List.rev in
      match List.find_opt (function Ex _ -> true | _ -> false) r with
      | Some (Ex e) -> sync (send c' (Ex e))
      | _ -> sync (send c' (Result (List.map (function Result r -> r | _ -> failwith "unreachable") r)))
    in
    let _ = Thread.create task () in
    c'

  (* additional stuff *)
  let memoize c =
    let c' = new_channel () in
    let task () =
      let r = sync (receive c') in
      let rec repeat () =
        sync (send c' r);
        repeat ()
      in
      repeat ()
    in
    let _ = Thread.create task () in
    c'

  let result_to receiver_c c =
    let task () =
      match sync (receive c) with
      | Result r -> sync (send receiver_c r)
      | Ex e -> raise e
    in
    let _ = Thread.create task () in
    ()

  let get_opt c = poll (receive c)
end



(* Future example *)
let read_lines filename =
  let file = open_in filename in
  let rec read_all l =
    try
      read_all (input_line file :: l)
    with End_of_file -> List.rev l
  in
  let content = read_all [] in
  close_in file;
  content

let write_file filename content =
  let file = open_out filename in
  output_string file content;
  close_out file

let print_list l =
  print_endline (String.concat "\n" l)

let main () =
  let f1 = Future.create read_lines "Pervasives.html" in
  let f1 = Future.then_ (List.filter ((<>) "")) f1 in
  let f2 = Future.create read_lines "List.html" in
  let f2 = Future.then_ (List.filter ((<>) "")) f2 in
(* let fany = Future.when_any [f1;f2] in
  print_list (Future.get fany)
  *)
  let fmerged = Future.when_all [f1;f2]
    |> Future.then_ (List.fold_left (@) [])
    |> Future.then_ (String.concat "\n")
    |> Future.then_ (write_file "merged.html") in
  try Future.get fmerged with e -> print_endline "Exception!"
