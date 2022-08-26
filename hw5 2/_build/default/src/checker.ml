open Ast
open ReM
open Dst


let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Pair(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    return @@ PairType(t1,t2)
  | Unpair(id1,id2,e1,e2) ->
    chk_expr e1 >>=
    pair_of_pairType "unpair: " >>= fun (t1,t2) ->
    extend_tenv id1 t1 >>+
    extend_tenv id2 t2 >>+
    chk_expr e2  
  | Letrec(id,param,tParam,tRes,body,target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")
  (* EXPLICIT-REFS *)
  | Unit -> return UnitType
  | BeginEnd([]) -> return UnitType
  | BeginEnd(es) ->
    List.fold_left (fun r e -> r >>= fun _ -> chk_expr e) (return UnitType) es
  | NewRef(e) ->
  chk_expr e >>= fun a -> return (RefType a)
  | DeRef(e) ->
    chk_expr e >>= arg_of_refType "reftype " >>= fun a ->
    return a
  | SetRef(e1,e2) ->
    chk_expr e1 >>= arg_of_refType "reftype " >>= fun a ->
    chk_expr e2 >>= fun b -> if a = b then return UnitType
    else error "setref: Expected a reference type"

 
  (* list *)
  | EmptyList(t) ->
    return (ListType t)
  | Cons(h, t) ->
    chk_expr h >>= fun z ->
    chk_expr t >>= arg_of_listType "listtype " >>= fun x ->
    if z = x then return @@ ListType x
    else error "cons: type of head and tail do not match"
  | IsNullL(e) ->
    chk_expr e >>=
    arg_of_listType "listtype " >>= fun _ ->
    return BoolType
  | Hd(e) ->
    chk_expr e >>=
    arg_of_listType "listtype " >>= fun a ->
    return a
  | Tl(e) ->
    chk_expr e >>=
    arg_of_listType "listtype " >>= fun a ->
    return @@ ListType a

  (* tree *)
  | EmptyTree(t) ->
  return (TreeType t)
  | Node(de, le, re) ->
    chk_expr de >>= fun a ->
    chk_expr le >>= arg_of_treeType "treetype " >>= fun b ->
    chk_expr re >>= arg_of_treeType "treetype " >>= fun c ->
    if (a = b && b = c) then return @@ TreeType a
    else error "error"
  | IsNullT(t) ->
    chk_expr t >>=
    arg_of_treeType "treetype " >>= fun _ ->
    return @@ BoolType
  | GetData(t) ->
    chk_expr t >>=
    arg_of_treeType "treetype " >>= fun a ->
    return a
  | GetLST(t) ->
    chk_expr t >>= arg_of_treeType "treetype " >>= fun a ->
    return @@ TreeType a
  | GetRST(t) ->
    chk_expr t >>=
    arg_of_treeType "treetype " >>= fun a ->
    return @@ TreeType a

  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> error "chk_expr: implement"    



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_expr
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_expr
  in run_teac (c >>= fun t -> return @@ Ast.string_of_texpr t)



