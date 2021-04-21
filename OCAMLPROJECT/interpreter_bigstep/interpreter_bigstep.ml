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


exception Eval_error





let rec string_of_exp (e : exp)  = 
    match e with 
    | True -> "true"
    | False -> "false"
    | If(exp1,exp2,exp3) ->  "if " ^ string_of_exp(exp1) ^" then " ^ string_of_exp(exp2) ^ " else " ^ string_of_exp(exp3) 
    | Num(e) -> string_of_int(e)
    | IsZero(e) -> "(isZero " ^string_of_exp(e)^")"
    | Plus(exp1,exp2) ->  (
      match string_of_exp(exp1) with
            | tmp1-> 
                          (
                         match string_of_exp(exp2) with                         
                               | tmp2 -> "(" ^ tmp1 ^ " + " ^ tmp2 ^ ")"
                               |_->  raise(Failure "error")
                          )
            | _->  raise(Failure "error")
                          )  
    | Mult(exp1,exp2) -> (
          match string_of_exp(exp1) with
                |tmp1 ->(
                    match string_of_exp(exp2) with
                          |tmp2 -> "(" ^ tmp1 ^ " * " ^ tmp2 ^ ")"
                          |_-> raise(Failure "error")
                )
    |_-> raise(Failure "error")
    ) 



   


    let rec eval (e : exp)  = 
      match e with 
      | True -> True
      | False -> False
      | Num(e) -> Num(e)
      | IsZero(e) ->   (
                       match eval(e) with 
                        | Num(e) ->
                         (
                         
                        if e == 0 then True else False

                         )
                         |_-> raise (Eval_error )
                        )




      | If(exp1,exp2,exp3) -> if eval(exp1) = True then eval(exp2) else eval(exp3)
      | Plus(exp1,exp2) ->
          (
             match eval(exp1) with 
                | Num(e1) ->
                    (
                       match eval(exp2) with

                       | Num(e2) -> Num(e1+e2)
                       | _-> raise (Eval_error )

                    )
                |_-> raise (Eval_error )

          )
          | Mult(exp1,exp2) ->
          (
             match eval(exp1) with 
                | Num(e1) ->
                    (
                       match eval(exp2) with

                       | Num(e2) -> Num(e1*e2)
                       | _-> raise (Eval_error )

                    )
                |_-> raise (Eval_error ) 

          )
      










       
      let () = print_endline (string_of_exp (Num 3))
      let () = print_endline (string_of_exp True)
      let () = print_endline (string_of_exp False)
      let () = print_endline (string_of_exp(Plus(Num 3,Num 2)))
      let () = print_endline (string_of_exp(Mult(Num 3,Num 2)))
      let () = print_endline (string_of_exp(Plus((Num 3),Plus((Num 3,Mult(Num 2,Plus(Num 3, Num 2)))))))
      let () = print_endline (string_of_exp(If(True,Num 3, Num 5)))
      let () = print_endline (string_of_exp(If(False,Plus(Num 3, Num 2), Plus(Num 5, Num 1))))
      let () = print_endline (string_of_exp(If(Plus(False, True),Plus(Num 3,False), Mult(Num 3, Num 1))))
      let () = print_endline (string_of_exp(If(IsZero(Num 1),Plus(Num 3, Num 2), Plus(Num 5, Num 1))))
      let () = print_endline (string_of_exp (IsZero(Mult(Num 3, Num 5))))
      let () = print_endline (string_of_exp (IsZero(If(IsZero(Num 1),Plus(Num 3 , Num 2),Plus(Num 5, Num 1)))))
      let () = print_endline (string_of_exp (Plus(Num 3, If(IsZero(Num 1),Plus(Num 3, Num 2),Plus(Num 5 , Num 1)))))
      let () = print_endline (string_of_exp (Plus(Num 3, If(IsZero(Num 1),Plus(Num 3, Num 2),Mult(Plus(Num 5 , Num 1),IsZero(True))))))
      let () = print_endline (string_of_exp(If(If(True,True,False),Plus(Num 3, Num 2), Plus(Num 5, Num 1))))
      let () = print_endline (string_of_exp(If(True,    If(IsZero(Mult(Num 3, Num 5)),Plus(Num 3, Num 2),Plus(Num 5, Num 1) )   ,   If(True  ,Mult(Num 3 , Num 2)  ,  Mult(Num 2,Plus(Num 3, Num 2))))))



    let() = print_endline("---------------------------------------------------------------------------------")

      
    let () = print_endline(string_of_exp(eval(True)))
    let () = print_endline(string_of_exp(eval(False))) 
    let () = print_endline(string_of_exp(eval(Num 0)))      
    let () = print_endline(string_of_exp(eval(IsZero(Num 0))))         
    let () = print_endline(string_of_exp(eval(IsZero(Plus(Num 1 , Num 1)))))   
    let () = print_endline(string_of_exp(eval (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)))))
    let () = print_endline(string_of_exp(eval (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))))
    let () = print_endline(string_of_exp(eval (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)))))
    let () = print_endline(string_of_exp(eval (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1)))))



    
   
    let testExp exp = 
                let rslt = 
                    try string_of_exp(eval exp)
                    with Eval_error _ -> "Eval_error" 
                    in print_endline(rslt)
  
        
    let () = testExp (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1))
    let () = testExp (IsZero (If (IsZero (Num 0), True, Num 0)))
    let () = testExp(IsZero(If( IsZero (Mult (Num 5, Num 0)), If (False, Num 0, IsZero (Plus (Num (-1), Num 0))), Num 0 )))
   
   
    let () = print_endline(string_of_exp(eval (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True))))
    let () = print_endline(string_of_exp(eval(If( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True), Mult (Num 1, Num 2), True ))))
    let () = print_endline(string_of_exp(eval(If( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0), Mult (Num 2, Mult (Num 1, Num 1)), Plus( Plus( Plus( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1)), Num 1 ), Num (-1) ), Num 1 ) ))))
    let () = print_endline(string_of_exp(eval(If( True, If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5), Plus (Mult (Num 4, Num 1), Num 1) ))))
    
    let () = print_endline(string_of_exp(eval(If( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1)), If( True, If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1)), Num 5 ), Num 5 ))))
    let () = testExp(If( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1)))), IsZero True, Num 1 ))
    let () = print_endline(string_of_exp(eval(Plus( Num 1, Plus( Num (-1), If( IsZero (Plus (Num 1, If (True, Num 1, Num 2))), Plus (Num 1, Num 2), Mult (Num 2, Num 2) ) ) ))))
    
    let () = testExp ((Plus( Num (-1), If( IsZero (Plus (Num 5, Num (-4))), Mult (Num 123, Plus (Num 5, Num (-4))), IsZero (Num 0) ) )))