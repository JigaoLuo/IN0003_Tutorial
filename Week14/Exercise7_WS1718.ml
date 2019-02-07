module type Iter = sig 
  type 'a t 
  type 'a s 
  val init : 'a t -> 'a s 
  val next : 'a s -> 'a option * 'a s 
end 
 








module ListIter : Iter = struct 
  type 'a t = 'a list 
  type 'a s = 'a list 
  let init l = l 
  let next s = match s with [] -> (None, []) | x :: xs -> (Some x, xs) 
End 

  

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree 

module TreeIter : Iter = struct 
  type 'a t = 'a tree 
  type 'a s = 'a list 
  let init t = 
    let rec tree_to_list tree = 
      match tree with Leaf -> [] 
                    | Node (v, l, r) -> tree_to_list l @ [v] @ tree_to_list r 
    in tree_to_list t 
  let next s = match s with [] -> (None, []) | x :: xs -> (Some x, xs) 
end



module ExtIter (I : Iter) = struct 
  include I
  let rec next_filtered f s = 
    match next s with  
      (None, state) then (None, state)  
    | (Some x, state) -> if f x then (Some x, state) else next_filtered f state 

  let next_mapped f s = 
    match next s with  
      (None, state) -> (None, state)  
    | (Some x, state) -> (Some (f x), state) 
end;; 




module type PairIterSig = sig 
  type ('a, 'b) t 
  type ('a, 'b) s 
  val init : ('a, 'b) t -> ('a, 'b) s 
  val next : ('a, 'b) s -> ('a * 'b) option * ('a, 'b) s 
end 


module PairIter (A : Iter) (B : Iter) : PairIterSig = struct 
  type ('a, 'b) t = ('a A.t * 'b B.t)  
  type ('a, 'b) s = ('a A.s * 'b B.s) 

  let init (a, b) = (A.init a, B.init b) 

  let next (a, b) = 
    let (ta, sa) = A.next a in 
    let (tb, sb) = B.next b in 
    let (!?) o = 
      match o with Some x -> x 
                 | None -> failwith "fail" in 
    if ta = None || tb = None then (None, (a, b)) 
    else (Some(!? ta, !? tb), (sa, sb))       
end 
                 