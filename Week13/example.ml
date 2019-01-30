open Thread

(* wait a line form console, then repeat it *)
let echo () = print_string (read_line () ^ "\n")  

(* mian function with many ;  *)
(* creates a Thread, which take the echo function to work with *)
(* let main = let t1 = create echo ()
            in join t1;
          print_int (id (self ()));
          print_string ": that is our Thread id\n" *)


(* create: (’a -> ’b) -> ’a -> t  *)
(* This funciton return the thread id *)
(* The created thread evaluates the function for its argument  *)
(* self : unit -> t *)
(* id: t -> int *)
(* join: t -> unit *)
(* kill: t -> unit *)
(* delay: float -> unit  *)
(* exit: unit -> unit *)

open Event (* creation of channels, sending and receiving *)

let thread_functin ch = let x = sync (receive ch)
                    in print_string (x ^ "\n");
                    sync (send ch "got it!")
            
let main = 
  let ch = new_channel () in create thread_functin ch;
  print_string "main is running ...\n";
  sync (send ch "Greetings!");
  print_string ("He " ^ sync (receive ch) ^ "\n")
  
(* main spawns a thread. Then it sends it a string and
waits for the answer. Accordingly, the new thread waits for the transfer
of a string value over the channel. As soon as the string is received, an
answer is sent on the same channel. *)

