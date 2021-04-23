(*
Zacharia Sabri OCAML project

*)
open List

type typ = 
	| TBool 
	| TInt 
	| TArrow of typ * typ

type type_environment = (string*typ) list



type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * exp
  | Apply of exp * exp
  | Let of string * exp * exp
  | TypeError
  type environment = (string * exp) list

exception Eval_error 
exception Subsitution_error










let contains list (y:string) = 
  mem y list



let list_remove list =
  List.fold_left (fun mem x ->if List.mem x mem then mem else x::mem ) [] list
(*create new list without x*)
  let rec list_remove_x x list = match list with 
    |[]->[]
    |hd::tl-> if hd = x then list_remove_x x tl else hd::(list_remove_x x tl)





let rec step(env: environment) (e:exp) = match e with
    |True -> (env,True)
    |False -> (env,False)
    |Num(e)-> (env,Num(e))
    |Lambda(x,e)->(env,Lambda(x,e))
    |TypeError->(env,TypeError)
    |Var(str) -> 
    (
       match env with
         |[]-> (env,TypeError)
         |(s,e2) :: tl-> if(s = str) then (env,e2) else (step tl (Var(str)) )
         |_-> (env,TypeError)
          )   
    |If(e,e2,e3)-> 
                  (
                    match e with
                    |Num(n) ->  (env, TypeError)
                    |True->(env,e2)
                    |False->(env,e3)
                    |TypeError-> (env,TypeError)
                    |Lambda(x,e)->(env,TypeError)
                    |_->let (env',e') = (step env e) in (env'@env,If(e',e2,e3))
                  )
    |IsZero(e)->
              (
             match e with
             |Num(e1)-> if ((e1)==0) then (env,True) else (env,False)
             |True->(env,TypeError)
             |False->(env,TypeError)
             |TypeError->(env,TypeError)
             |Lambda(x,e)->(env,TypeError)
             |_-> let (env',e') = (step env e) in (env'@env,IsZero(e'))
              )
    |Plus(num1,num2) ->
    (
       match num1 with 
          |Num(n1) ->
              (
                 match num2 with

                 | Num(n2) -> (env,Num(n1+n2))
                 |True-> (env,TypeError)
                 |False->(env,TypeError)
                 |Lambda(x,e)-> (env,TypeError)
                 |TypeError->(env,TypeError)
                 | _-> let (env',e') = (step env num2) in (env'@env,Plus(num1,e'))

              )
              |True-> (env,TypeError)
              |False->(env,TypeError)
              |Lambda(x,e)->(env,TypeError)
              |TypeError->(env,TypeError)
              |_-> let (env',e') = (step env num1) in (env'@env,Plus(e',num2))
    )
    |Mult(num1,num2)-> (
      match num1 with 
         |Num(n1) ->
             (
                match num2 with

                | Num(n2) -> (env,Num(n1*n2))
                |False->(env,TypeError)
                |True->(env,TypeError)
                |Lambda(x,e)->(env,TypeError)
                |TypeError->(env,TypeError)
                | _-> let (env',e') = (step env num2) in (env'@env,Mult(num1,e'))

             )
             |True->(env,TypeError)
             |False->(env,TypeError)
             |Lambda(x,e)->(env,TypeError)
             |TypeError->(env,TypeError)
             |_-> let (env',e') = (step env num1) in (env'@env,Mult(e',num2))

   )
    |Apply(exp1,exp2)-> 
                       ( 
                         
                       match exp1 with

                       |Lambda(x, et)->(

                              match exp2 with

                            

                              |True -> ((x,True)::env,et)
                              |False -> ((x,False)::env,et)
                              |Num(e2)-> ((x,Num(e2))::env,et)
                              |Lambda(x',e')->(((x,Lambda(x',e'))::env),et)
                              |TypeError->(env,TypeError)
                         
                              |_-> let (env',e') = (step env exp2) in (env'@env,Apply(exp1,e'))

                       )
                       |True->(env,TypeError)
                       |False->(env,TypeError)
                       |Num(e1)->(env,TypeError)
                       |TypeError->(env,TypeError)
                       |_->let (env',e') = (step env exp1) in (env,Apply(e',exp2))
                       
                       )
   |Let(x,e1,e2)-> (env, Apply(Lambda(x,e2),e1)) 
 
   

  
   let rec multi_step (env:environment)(e:exp) = match e with
        |True -> (env,True)
        |False -> (env,False)
        |Num(e)->  (env,Num(e))
        |Var(str)->(env,Var(str))
        |Lambda(x, e)-> (env,Lambda(x,  e))
        |TypeError->(env,TypeError)
        |If(e1,e2,e3) -> let (env,e') = (step env e) in (multi_step env e' )
        |IsZero(e) -> let (env,e') = (step env e) in (multi_step env e' )
        |Plus(num1,num2) -> let (env,e') = (step env e) in (multi_step env e' )
        |Mult(num1,num2)-> let (env,e') = (step env e) in (multi_step env e' )
        |Apply(exp1,exp2)-> let (env,e') = (step env e) in (multi_step env e' )
        |Let(x, exp1,exp2)-> let (env,e') = (step env e) in (multi_step env e' )
        




