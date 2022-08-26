type 'a gt = Node of 'a*('a gt) list

let t : int gt =
    Node (33,
  [Node (12 ,[]);
    Node (77,
  [Node (37,
    [Node (14, [])]);
  Node (48, []);
    Node (103, [])])
  ])


  let mk_leaf (n:'a) : 'a gt =
   Node(n,[])

let t1 : int gt = Node(33,[])

  let rec mirror (Node(x,y)) =
    match Node(x,y) with
  |  Node(x,[]) -> Node(x,[])
  |  Node(x,y) -> Node(x, List.map mirror (List.rev y))



  let rec preorder = function
  | Node(x,[]) -> [x]
  | Node(x,y) -> x :: List.flatten(List.map preorder y)
  


  let rec inorder = function
  | Node(x,[]) -> [x]
  | Node(x,y) -> x :: List.rev(List.flatten(List.map preorder y))

  

  let rec max l o = (*list and other number *)
    match l with
    | [] -> o
    | h::t -> 
      if o>h (*other number greater than head of list*)
      then max t o (*get max of tail or other number*)
      else max t h (*max of head or tail*)



  let rec height t =
    match t with
    | Node(x,[]) -> 1
    | Node(x,y) -> max (List.map height y) 0 + 1 ;;


    
    let rec size t =
      match t with
      | Node(x,[]) -> 1
      | Node(x,y) -> List.length(inorder t) ;;



        let rec helper' l =
          match l with
          | [] -> true
          | [_] -> true
          | h::mid::t -> 
            if h=mid
            then helper' (mid::t)
          else false
          
        let is_leaf_perfect t =
          helper' (inorder t) ;;


let rec mapt f (Node(x,y)) = 
  match y with 
  | [] -> Node(f x, [])
  | _ -> Node(f x, List.map(mapt f) (y))


  let rec foldt = fun f (Node(x,y)) ->
    match y with
    | [] -> f x []
    | _ -> f x (List.map(foldt f) y)

let rec paths_to_leaves t = 
  match t with
  | Node(x, []) -> [[]]
  | Node(x,y) -> List.flatten (List.mapi (fun i n -> (List.map(fun a -> i :: a) n)) (List.map paths_to_leaves y))


    let sumt t =
      foldt (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t
     let memt t e =
      foldt (fun i rs -> i=e || List.exists (fun i -> i) rs) t



      let rec mirror' (Node(x,y)) =
        foldt (fun i rs -> Node(i, List.rev rs)) t
       
