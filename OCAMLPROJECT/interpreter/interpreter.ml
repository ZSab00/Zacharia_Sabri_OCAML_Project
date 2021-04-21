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
  | Lambda of string * typ * exp
  | Apply of exp * exp
  | LambdaRec of string * typ * typ * string * exp

exception Eval_error 
exception Type_error
exception Substitution_error










let contains list (y:string) = 
  mem y list



let list_remove list =
  List.fold_left (fun mem x ->if List.mem x mem then mem else x::mem ) [] list
(*create new list without x*)
  let rec list_remove_x x list = match list with 
    |[]->[]
    |hd::tl-> if hd = x then list_remove_x x tl else hd::(list_remove_x x tl)


    let rec free_variables (e : exp) = match e with
    |Num(e) -> []
    |True -> []
    |False -> []
    |Var(x) -> [x]
    |Lambda(x,tp,y)-> (list_remove_x x (free_variables y) )
    |IsZero(e)->free_variables(e)
    |Plus(exp1,exp2)->free_variables(exp1) @ free_variables(exp2)
    |Mult(exp1,exp2)->free_variables(exp1) @ free_variables(exp2)
    |If(exp1,exp2,exp3)->free_variables(exp1) @ free_variables(exp2) @ free_variables(exp3)
    |LambdaRec(str1,typ1,typ2,str2,exp)-> list_remove_x str1 (list_remove_x str2 (free_variables exp))





(*Definition of substitution e1[e2/x]
x[e/x]= e
y[e/x] = y if y different from x
True[e/x] = True
(e1+e2)[e/x] = (e1[e/x] + e2[e/x])
If(e1,e2,e3)[e/x] = if(e1)[e/x] then e2[e/x] else e3[e/x]
*)



(*

(rec (f : T -> T) x = e1)[e2/x] = rec (f : T -> T) x = e1
e1[e/x]

(rec (f : T -> T) x = e1)[e2/f] = rec (f : T -> T) x = e1


(rec (f : T -> T) x = e1)[e2/y] = (rec (f : T -> T) x = (e1[e2/y]))  when y is different from f and x, x is NOT in FV(e2), and f is NOT in FV(e2)


(rec (f : T -> T) x = e1)[e2/y] = raise Substitution_error  when y is different from f and x, and:  x is in FV(e2) or f is in FV(e2) (or both)
*)




let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
|Var(var)-> if var = x then e2 else Var(var)
|True -> True 
|False -> False 
|Num(e) -> Num(e)
|Plus(exp1,exp2) -> Plus( substitution exp1 x e2 , substitution exp2 x e2)
|Mult(exp1,exp2) -> Mult(substitution exp1 x e2 , substitution exp2 x e2)
|IsZero(e)-> IsZero(substitution e x e2 )
|If(exp1,exp2,exp3)-> If( (substitution exp1 x e2) , (substitution exp2 x e2) ,(substitution exp3 x e2))
|Lambda(y,typ,exp1)-> if( (y = x) || (contains (free_variables e2) y)) then Lambda(y,typ,exp1) else Lambda(y,typ, substitution exp1 x e2)                    
|Apply(exp1, exp2)-> Apply((substitution exp1 x e2) , (substitution exp2 x e2))
|LambdaRec(str1, typ1, typ2 ,str2 , e)->  if(     (str2 = x) || (str1 = x)      ) then LambdaRec(str1, typ1, typ2 ,str2 , e) 
                                                        else if     (      (contains (free_variables e2) str1) ||   (contains (free_variables e2) str2)     )  
                                                                then 
                                                                     raise Substitution_error
                                                                           else 
                                                                               (substitution (LambdaRec(str1, typ1, typ2 ,str2 , e)) x e2)
                                          

                                           






















let rec step(e:exp) = match e with
    |True -> raise (Eval_error ) 
    |False -> raise (Eval_error )
    |Num(e)-> raise (Eval_error )
    |Var(e)-> raise (Eval_error )
    |If(True,e2,e3) -> e2
    |If(False,e2,e3) -> e3
    |If(e,e2,e3)-> 
                  (
                    match e with
                    |Num(n) -> raise (Eval_error )
                    |_->let e' = step(e) in If(e',e2,e3)
                  )
    |IsZero(Num(0))->True 
    |IsZero(e)->
              (
             match e with
             |Num(e1)-> if ((e1)==0) then True else False
             |_-> let e' = step(e) in IsZero(e')
              )
    |IsZero(True)->raise Eval_error
    |IsZero(False)->raise Eval_error
    |Plus(num1,num2) ->
    (
       match num1 with 
          | Num(n1) ->
              (
                 match num2 with

                 | Num(n2) -> Num(n1+n2)
                 | _-> Plus(num1,(step num2))

              )
         
          |_-> Plus((step num1),num2)
    )
    |Apply(exp1,exp2)-> 
                       ( match exp1 with

                       |Lambda(x, typ, e)->
                       (

                              match exp2 with


                             
                              |True -> substitution e x exp2
                              |False -> substitution e x exp2
                              |Var(e1) -> substitution e x exp2 
                              |Num(e2)-> substitution e x exp2
                              |Lambda(x',typ',e')-> substitution e x exp2

                              |_-> Apply(exp1,step exp2)


                       )
                       (*
                           rec((f:T->T ) x=e)v) --> e[v/x](rec(f:T->T)X=e)/f)
                       *)
                       |LambdaRec(str1, typ1, typ2 ,str2 , e)->  
                                (
                                    match exp2 with
                                    |True -> substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))
                                    |False -> substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))
                                    |Var(e1) -> substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))
                                    |Num(e2) -> substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))
                                    |Lambda(x',typ',e') -> substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))
                                   
                                    |_-> Apply(exp1,step exp2)



                                )
                       |_->Apply(step exp1 , exp2)
                       )
                            
    |Mult(num1,num2)-> (
      match num1 with 
         | Num(n1) ->
             (
                match num2 with

                | Num(n2) -> Num(n1*n2)
                | _-> Mult(num1,(step num2))

             )
            
         |_-> Mult((step num1),num2)

   )
   |Plus(True,num2)->raise Eval_error
   |Plus(num1,True)-> raise Eval_error
   |Plus(False, num2)->raise Eval_error
   |Plus(num1,False)->raise Eval_error
   |Mult(True,num2)->raise Eval_error
   |Mult(num1,True)-> raise Eval_error
   |Mult(False, num2)->raise Eval_error
   |Mult(num1,False)->raise Eval_error

  
   let rec multi_step(e:exp) = match e with
        |True -> True
        |False -> False
        |Num(e)->Num(e)
        |Var(str)->Var(str)
        |Lambda(x, typ, e)-> Lambda(x, typ, e)
        |LambdaRec(str1,typ1,typ2,str2,exp1)->LambdaRec(str1,typ1,typ2,str2,exp1)
        |If(e1,e2,e3) -> multi_step(step(If(e1,e2,e3)))
        |IsZero(e) -> multi_step(step(IsZero(e)))
        |Plus(num1,num2) -> multi_step(step(Plus(num1,num2)))
        |Mult(num1,num2)-> multi_step(step(Mult(num1,num2)))
        |Apply(exp1,exp2)-> multi_step(step(Apply(exp1,exp2)))
        





        let rec type_check (te : type_environment) (e : exp) = match e with
        |Num(e)-> TInt
        |True->TBool
        |False->TBool
        |Var(str) -> 
                   (
         match te with
         |[]-> raise Type_error
         |(s,t) :: tl-> if(s = str) then t  else (type_check tl (Var(str)) )
         |_-> raise Type_error
                  ) 
         |If(e1,e2,e3)-> (

               match (type_check te e1) with
               |TBool->
               (

                  let typ1 = (type_check te e2)
                  and typ2 = (type_check te e3) in
                    match (typ1, typ2) with
                    |(TInt, TInt) -> TInt
                    |(TBool, TBool)->TBool
                    |(TArrow(t1,t2), TArrow(t1',t2')) -> if ((t1=t1') && (t2 = t2')) then TArrow(t1,t2) else raise Type_error
                    |_-> raise Type_error
               )
              
               |_->raise Type_error
         )
         
        |IsZero(e)->
              (
             match (type_check te e) with
             |TInt-> TBool
             |_->raise Type_error
              )
         
         |Plus(num1,num2) ->
             (
                match (type_check te num1) with 
                  | TInt ->
              (
                 match (type_check te num2) with

                  | TInt -> TInt
                  | _-> raise Type_error
              )
                  |_-> raise Type_error
              )
         |Mult(num1,num2) ->
              (
                 match (type_check te num1) with 
                   | TInt ->
               (
                  match (type_check te num2) with
 
                   | TInt -> TInt
                   | _-> raise Type_error
               )
                   |_-> raise Type_error
               )
         
                     
          |Lambda(x,typ,exp)-> TArrow(typ,(type_check ((x,typ)::te) exp))        
          

         (*
                    let test_1 () =
                              assert_equal
                                (type_check []
                                   (LambdaRec ("f", TInt, TInt, "x", Plus (Var "x", Var "x"))))
                                (TArrow (TInt, TInt))
         *)


          |LambdaRec(str1,typ1,typ2,str2,exp1)->  
          (
  
            if((type_check ((str1,TArrow(typ1,typ2))::(str2,typ1)::te) (Lambda(str2,typ1,exp1)))<>(TArrow(typ1,typ2))) then raise Type_error else TArrow(typ1,typ2)
                

        

          )

          |Apply(exp1,exp2)->
                             (
                                match (type_check te exp1) with

                                |TArrow(typ1,typ2)->
                                (
                                    if((type_check te exp2)<>typ1) then raise Type_error else typ2
                                   

                                )



                                |_-> raise Type_error
                             )
          








             
              
               
                             let assert_equal a b = assert (a = b)

                             let test_1 () =
                              assert_equal
                                (type_check []
                                   (LambdaRec ("f", TInt, TInt, "x", Plus (Var "x", Var "x"))))
                                (TArrow (TInt, TInt))
                            
                            let test_2 () =
                              assert_equal
                                (multi_step (LambdaRec ("f", TInt, TInt, "x", Plus (Var "x", Var "x"))))
                                (LambdaRec ("f", TInt, TInt, "x", Plus (Var "x", Var "x")))
                            
                            let test_3 () =
                              assert_equal
                                (type_check []
                                   (LambdaRec
                                      ( "f"
                                      , TInt
                                      , TInt
                                      , "x"
                                      , If
                                          ( IsZero (Var "x")
                                          , Num 0
                                          , Apply (Var "f", Plus (Var "x", Num (-1))) ) )))
                                (TArrow (TInt, TInt))

                             let () =
                              print_endline "Recursion tests, your grade: 0.86";
                              test_1 ();
                              print_endline "passed test 1";
                              test_2 ();
                              print_endline "passed test 2";
                              test_3();
                              print_endline "passed test 3";