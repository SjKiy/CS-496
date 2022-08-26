type program = int list
let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]



let rec repeat : int -> 'a -> 'a list =
  fun n l ->
  match n with
  | 0 -> []
  | n -> l :: repeat (n-1) l ;;


  
  let rec esc = fun a b ->
  match b,a with
  | 0, h::t-> h::t
  | _,[] -> []
  | b, h::t -> esc t (b-1)


let rec p = fun a b ->
  match a with 
  | [] -> 0
  | h::t -> if h = b
    then (p t b) + 1
    else 0


  let rec mirror_image : int list -> int list = fun s -> 
  match s with
  | [] -> []
  | h::t -> 
  if h = 0
  then h :: mirror_image t
  else if h = 5
  then 3 :: mirror_image t
  else if h = 4
  then 2 :: mirror_image t
  else if h = 3
  then 5 :: mirror_image t
  else if h = 1
  then h :: mirror_image t
  else if h = 2
  then 4 :: mirror_image t
  else mirror_image t




  let rec rotate_90_letter : int list  -> int list = fun s -> 
  match s with
  | [] -> []
  | h::t -> 
  if h = 0
  then 0 :: rotate_90_letter t
  else if h = 5
  then 2 :: rotate_90_letter t
  else if h = 4
  then 5 :: rotate_90_letter t
  else if h = 3
  then 4 :: rotate_90_letter t
  else if h = 1
  then 1 :: rotate_90_letter t
  else if h = 2
  then 3 :: rotate_90_letter t
  else rotate_90_letter t

  
  let rec rotate_90_word : int list list -> int list list = fun s -> 
  List.map rotate_90_letter s

    let rec pantograph_nm : int -> int list -> int list = fun m n ->
    match m,n with
    | m, [] -> []
    | 0,_ -> []
    | n, h::t -> 
      if h = 0 || h = 1
      then h :: (pantograph_nm n t)
      else if h = 2
      then (repeat n h) @ (pantograph_nm n t)
      else if h = 3
      then (repeat n h) @ (pantograph_nm n t)
      else if h = 4
      then (repeat n h) @ (pantograph_nm n t)
      else if h = 5
      then (repeat n h) @ (pantograph_nm n t)
      else pantograph_nm n t



      let rec pantograph_f : int -> int list -> int list = fun m n ->
      match m,n with
      | m, [] -> []
      | 0,_ -> []
      | n, h::t -> 
        if h = 0 || h = 1
        then h :: (pantograph_nm n t)
        else if h = 2
        then (repeat n h) @ (pantograph_nm n t)
        else if h = 3
        then (repeat n h) @ (pantograph_nm n t)
        else if h = 4
        then (repeat n h) @ (pantograph_nm n t)
        else if h = 5
        then (repeat n h) @ (pantograph_nm n t)
        else pantograph_nm n t

let rec map f m n =
      match m,n with
      | m, [] -> []
      | 0,_ -> []
      | n, h::t -> 
        if h = 0 || h = 1
        then f h :: (map f n t)
        else if h = 2
        then (repeat n h) @ (map f n t)
        else if h = 3
        then (repeat n h) @ (map f n t)
        else if h = 4
        then (repeat n h) @ (map f n t)
        else if h = 5
        then (repeat n h) @ (map f n t)
        else map f n t

let pantograph = map (fun h -> h)


let rec fold_right f a l =
  match l with 
  | [] -> a
  | (x, y)::l -> repeat y x @ (fold_right f a l)


let uncompress_f l = fold_right (fun h r -> h @ r) [] l


let optimize p = 
  let rec opt pi prev acc =
      match pi with
      | [] -> acc
      | h::t -> 
          if (prev = 1 && h = 1) || (prev = 0 && h = 0) then (opt t h acc)
          else (opt t h (h::acc))
  in List.rev (opt p 1 ([]))
;;

let tracker : int -> int*int -> int*int = fun n (a,b) ->
    match n with
    | 0 -> (a,b)
    | 1 -> (a,b)
    | 2 -> (a,b+1)
    | 3 -> (a+1, b)
    | 4 -> (a, b-1)
    | 5 -> (a-1, b)
  
let rec coverage : int*int -> int list -> (int*int) list = fun m n ->
    match n with
    | [] -> []
    | h::t -> List.rev(List.rev(tracker h m :: coverage (tracker h m) t))




let rec compress : int list -> (int*int) list = fun l ->
  match l with
  | [] -> []
  | h::t -> 
    List.rev (List.rev ((h, (p t h + 1)) :: compress (esc t (p t h))))


  let rec uncompress : (int*int) list -> int list = fun l ->
    match l with
    | [] -> []
    | (x, y)::l -> List.rev ( List.rev (repeat y x @ uncompress l))
