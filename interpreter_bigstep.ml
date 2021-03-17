(*
Zacharia Sabri OCAML project
3/17/2021 check-in

*)



type exp = 
  | True
  | False
  | If of exp*exp*exp 
  | Num of int 
  | IsZero of exp 
  | Plus of exp*exp 
  | Mult of exp*exp 


exception ExpressionError of string

let rec string_of_expression (e : exp)  = 
    match e with 
    | True -> "true"
    | False -> "false"
    | If(exp1,exp2,exp3) ->  "if " ^ string_of_expression(exp1) ^" then " ^ string_of_expression(exp2) ^ " else " ^ string_of_expression(exp3) 
    | Num(e) -> string_of_int(e)
    | IsZero(e) -> "(isZero " ^string_of_expression(e)^")"
    | Plus(exp1,exp2) ->  (
      match string_of_expression(exp1) with
            | tmp1-> 
                          (
                         match string_of_expression(exp2) with                         
                               | tmp2 -> "(" ^ tmp1 ^ " + " ^ tmp2 ^ ")"
                               |_-> raise(Failure "error" )
                          )
            | _-> raise(Failure "error" )
                          )  
    | Mult(exp1,exp2) -> (
          match string_of_expression(exp1) with
                |tmp1 ->(
                    match string_of_expression(exp2) with
                          |tmp2 -> "(" ^ tmp1 ^ " * " ^ tmp2 ^ ")"
                          |_-> raise(Failure "error")
                )
    |_-> raise(Failure "error")
    ) 
                       
       
      let () = print_endline (string_of_expression (Num 3))
      let () = print_endline (string_of_expression True)
      let () = print_endline (string_of_expression False)
      let () = print_endline (string_of_expression(Plus(Num 3,Num 2)))
      let () = print_endline (string_of_expression(Mult(Num 3,Num 2)))
      let () = print_endline (string_of_expression(Plus((Num 3),Plus((Num 3,Mult(Num 2,Plus(Num 3, Num 2)))))))
      let () = print_endline (string_of_expression(If(True,Num 3, Num 5)))
      let () = print_endline (string_of_expression(If(False,Plus(Num 3, Num 2), Plus(Num 5, Num 1))))
      let () = print_endline (string_of_expression(If(Plus(False, True),Plus(Num 3,False), Mult(Num 3, Num 1))))
      let () = print_endline (string_of_expression(If(IsZero(Num 1),Plus(Num 3, Num 2), Plus(Num 5, Num 1))))
      let () = print_endline (string_of_expression (IsZero(Mult(Num 3, Num 5))))
      let () = print_endline (string_of_expression (IsZero(If(IsZero(Num 1),Plus(Num 3 , Num 2),Plus(Num 5, Num 1)))))
      let () = print_endline (string_of_expression (Plus(Num 3, If(IsZero(Num 1),Plus(Num 3, Num 2),Plus(Num 5 , Num 1)))))
      let () = print_endline (string_of_expression (Plus(Num 3, If(IsZero(Num 1),Plus(Num 3, Num 2),Mult(Plus(Num 5 , Num 1),IsZero(True))))))
      let () = print_endline (string_of_expression(If(If(True,True,False),Plus(Num 3, Num 2), Plus(Num 5, Num 1))))
      let () = print_endline (string_of_expression(If(True,    If(IsZero(Mult(Num 3, Num 5)),Plus(Num 3, Num 2),Plus(Num 5, Num 1) )   ,   If(True  ,Mult(Num 3 , Num 2)  ,  Mult(Num 2,Plus(Num 3, Num 2))))))