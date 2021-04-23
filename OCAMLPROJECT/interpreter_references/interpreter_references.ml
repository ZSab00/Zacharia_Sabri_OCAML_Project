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
  | Div of exp * exp
  | Try of exp * exp
  | RaiseDivByZero of typ * exp 
  | Label of int
  | Malloc of exp
  | Mread of exp
  | Assign of exp * exp
  | Sequence of exp * exp
  | Unit 
  type memory = (int * exp) list
exception Eval_error 
exception Type_error
exception Substitution_error



let find (m) v k = 
  (
     match m with
       |[]  ->  (k,v)::m
       |(i,e2) :: tl-> (find tl v k)
       |_-> raise Eval_error
        ) 

        let find2 m l = 
          (
             match m with
               |[]  -> raise Eval_error
               |(i,e2) :: tl-> if(l = i) then e2 else (find2 tl l)
                ) 






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
    |Label(e)->[]
    |Unit -> []
    |Var(x) -> [x]
    |Lambda(x,tp,y)-> (list_remove_x x (free_variables y) )
    |IsZero(e)->free_variables(e)
    |Plus(exp1,exp2)->free_variables(exp1) @ free_variables(exp2)
    |Mult(exp1,exp2)->free_variables(exp1) @ free_variables(exp2)
    |If(exp1,exp2,exp3)->free_variables(exp1) @ free_variables(exp2) @ free_variables(exp3)
    |LambdaRec(str1,typ1,typ2,str2,exp)-> list_remove_x str1 (list_remove_x str2 (free_variables exp))
    | Div(exp1,exp2)->free_variables(exp1) @ free_variables(exp2)
    | Try(exp1, exp2)->free_variables(exp1) @ free_variables(exp2)
    | RaiseDivByZero(typ, exp1)->free_variables(exp1) 
    | Malloc(exp1)->free_variables(exp1)
    | Mread(exp1)->free_variables(exp1)
    | Assign(exp1,exp2)->free_variables(exp1)@free_variables(exp2)
    | Sequence(exp1,exp2)->free_variables(exp1)@free_variables(exp2)
    


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

|Div(exp1, exp2)->Div(substitution exp1 x e2 , substitution exp2 x e2)
|Try(exp1,exp2)->Try(substitution exp1 x e2 , substitution exp2 x e2)
|RaiseDivByZero(typ1,exp1)->RaiseDivByZero(typ1 , substitution exp1 x e2)
|Label(exp)-> Label(exp)
|Malloc(exp)-> Malloc(substitution exp x e2 )
|Mread(exp)->Mread(substitution exp x e2 )
|Assign(exp1,exp2)->(substitution exp1 x e2 , substitution exp2 x e2)
|Sequence(exp1, exp2)->(substitution exp1 x e2 , substitution exp2 x e2)
|Unit->Unit 
                                          

                                           






















let rec step(e:exp) (m:memory) = match e with
    |True -> raise (Eval_error ) 
    |False -> raise (Eval_error )
    |Num(e)-> raise (Eval_error )
    |Var(e)-> raise (Eval_error )
    |If(True,e2,e3) -> (e2,m)
    |If(False,e2,e3) -> (e3,m)
    |If(e,e2,e3)-> 
                  (
                    match e with
                    |Num(n) -> raise (Eval_error )
                    |RaiseDivByZero(typ, Num(e1))->RaiseDivByZero(typ, Num(e1))
                    |_->let e' = (step e m) in If(e',e2,e3)
                  )
    |IsZero(Num(0))->(True,m)
    |IsZero(e)->
              (
             match e with
             |Num(e1)-> if ((e1)==0) then (True,m) else (False,m)
             |RaiseDivByZero(typ, Num(e1))->(RaiseDivByZero(typ, Num(e1)),m)
             |_-> let e' = (step e m) in IsZero(e')
              )
    |IsZero(True)->raise Eval_error
    |IsZero(False)->raise Eval_error
    |Plus(num1,num2) ->
    (
       match num1 with 
          | Num(n1) ->
              (
                 match num2 with

                 | Num(n2) -> (Num(n1+n2),m)
                 | _-> Plus(num1,(step num2 m))

              )
         
          |_-> Plus((step num1 m),num2)
    )
    |Apply(exp1,exp2)-> 
                       ( match exp1 with

                       |Lambda(x, typ, e)->
                       (

                              match exp2 with


                             
                              |True -> ((substitution e x exp2) , m)
                              |False -> ((substitution e x exp2),m)
                              |Var(e1) -> ((substitution e x exp2 ),m)
                              |Num(e2)-> ((substitution e x exp2),m)
                              |Lambda(x',typ',e')-> ((substitution e x exp2),m)
                              |Div(e1,e2)->((substitution e x exp2),m)
                              |Try(e1,e2)->((substitution e x exp2),m)
                              |_-> Apply(exp1,(step exp2 m))


                       )
                       (*
                           rec((f:T->T ) x=e)v) --> e[v/x](rec(f:T->T)X=e)/f)
                       *)
                       |LambdaRec(str1, typ1, typ2 ,str2 , e)->  
                                (
                                    match exp2 with
                                    |True -> ((substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))),m)
                                    |False -> ((substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))),m)
                                    |Var(e1) -> ((substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))),m)
                                    |Num(e2) -> ((substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))),m)
                                    |Lambda(x',typ',e') -> ((substitution (substitution e str2 exp2) str1 (LambdaRec(str1, typ1, typ2 ,str2 , e))),m)
                                   
                                    |_-> Apply(exp1,(step exp2 m))



                                )
                       |_->Apply((step exp1 m), exp2)
                       )
                            
    |Mult(num1,num2)-> (
      match num1 with 
         | Num(n1) ->
             (
                match num2 with

                | Num(n2) -> (Num(n1*n2),m)
                | _-> Mult(num1,((step num2 m))

             )
            
         |_-> Mult((step num1 m),num2)

   )
    )
   |Div(num1,num2)-> (
    match num1 with 
       |Num(n1) ->
           (
              match num2 with
              
              |Num(n2) -> if(n2 == 0) then ( RaiseDivByZero(TInt, Num(n1)),m) else (Num(n1/n2),m)
              | _-> Div(num1,(step num2 m))

           )
          
       |_-> Div((step num1 m),num2)

 )
 (*
      RaiseDivByZero T v -> raise Eval_error
      RaiseDivByZero(t, RaiseDivByZero(t1, v))->RaiseDivByZero(t1,v)
      RaiseDivByZero T exp - > RaiseDivByZero T (step exp)  

    
 
 
 *)
   |Try(True,e1)->(True , m)
   |Try(False,e1)->(False,m) 
   |Try(Num(e1),e2)->(Num(e1),m)
   |Try(RaiseDivByZero(typ, n1),e1)-> (step (Apply(e1,n1)),m)
   |Try(e1,e2)->Try((step e1 m), e2)
   |RaiseDivByZero(t,exp)->
                      (
                              match exp with
                              |True -> raise Eval_error
                              |False -> raise Eval_error
                              |Num(e1)-> raise Eval_error
                              |RaiseDivByZero(t, RaiseDivByZero(t1, v))->(RaiseDivByZero(t1,v),m)
                              |_-> RaiseDivByZero(t,(step exp m))

                      )



  
   |RaiseDivByZero(t, RaiseDivByZero(t1, v))->(RaiseDivByZero(t1,v),m)
   |If(RaiseDivByZero(t, v),e2,e3)-> (RaiseDivByZero(t, v),m)
   |IsZero(RaiseDivByZero(t, v))->(RaiseDivByZero(t, v),m)
   |Plus(RaiseDivByZero(t, v),e2)->(RaiseDivByZero(t, v),m)
   |Plus(e1,RaiseDivByZero(t, v))->(RaiseDivByZero(t, v),m)
   |Mult(e1,RaiseDivByZero(t, v))->(RaiseDivByZero(t, v),m)
   |Mult(RaiseDivByZero(t, v),e2)->(RaiseDivByZero(t, v),m)
   |Apply(RaiseDivByZero(t, v),e2)->(RaiseDivByZero(t, v),m)
   |Apply(e1,RaiseDivByZero(t, v))->(RaiseDivByZero(t, v),m)
   |Plus(True,num2)->raise Eval_error
   |Plus(num1,True)-> raise Eval_error
   |Plus(False, num2)->raise Eval_error
   |Plus(num1,False)->raise Eval_error
   |Mult(True,num2)->raise Eval_error
   |Mult(num1,True)-> raise Eval_error
   |Mult(False, num2)->raise Eval_error
   |Mult(num1,False)->raise Eval_error
   
   |Malloc(exp)->(
     
              match exp with
                |True ->  (

                           ((step Assign(Label(i),exp)),m)

                    


                          )
                |False ->
                (
                  ((step Assign(Label(i),exp)),m)
                            
                )
                |Num(e1)-> 
                (
                  
                
                  ((step Assign(Label(i),exp)),m)

                )
                
                |_-> Malloc(step exp m)
   

   )
   
   

   
   
   
   
   |Mread(exp)->
                 (
                   match exp with
                   |Label(i)->((find2 m Label(i)),m)
                   |_->Mread(step exp)
                 )
   |Assign(exp1, exp2)->(
                 match exp1 with
                 |Label(i)->
                 (
                    match exp2 with

                    |True -> (Unit,(find m exp2 Unit))
                    |False -> (Unit,(find m exp2 Unit))
                    |Num(e1)-> (Unit,(find m exp2 Unit))
                    |_-> Assign(exp1,(step exp2))
                 )
                 |_->Assign((step exp1,exp2))

(*
let find m v k = 
  (
     match m with
       |[] = ->  (Label(k),v)::m
       |(i,e2) :: tl-> (find tl v Plus(Num(i),Num(1)))
       |_-> raise Eval_error
        ) *)



   )
   |Sequence(exp1 , exp2)->
   (
    match exp1 with

    |True -> (exp2,m)
    |False -> (exp2,m)
    |Num(e1)-> (exp2,m)

    
    |_->Sequence((step exp1,exp2))



   )
   |Unit ->(Unit, m)
  

  














  
   let rec multi_step(e:exp) = match e with
        |True -> True
        |False -> False
        |Num(e)->Num(e)
        |Var(str)->Var(str)
        |Lambda(x, typ, e)-> Lambda(x, typ, e)
        |RaiseDivByZero(typ, Num(e1))->RaiseDivByZero(typ, Num(e1))
        |LambdaRec(str1,typ1,typ2,str2,exp1)->LambdaRec(str1,typ1,typ2,str2,exp1)
        |If(e1,e2,e3) -> multi_step(step(If(e1,e2,e3)))
        |IsZero(e) -> multi_step(step(IsZero(e)))
        |Plus(num1,num2) -> multi_step(step(Plus(num1,num2)))
        |Mult(num1,num2)-> multi_step(step(Mult(num1,num2)))
        |Apply(exp1,exp2)-> multi_step(step(Apply(exp1,exp2)))
        |Div(exp1,exp2)->multi_step(step(Div(exp1,exp2)))
        |Try(exp1, exp2)->multi_step(step(Try(exp1,exp2)))
        |RaiseDivByZero(typ, exp)-> multi_step(step(RaiseDivByZero(typ,exp)   ))  
    
        




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
          

           |Div(num1,num2) ->
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
           |Try(exp1, exp2)->
           (
                 match (type_check te exp1) with
                 |t-> (
                   match(type_check te exp2 ) with 
                   |TArrow(TInt,t')-> if(t' = t) then t' else  raise Type_error
                 )
           )
          
           |RaiseDivByZero(typ, exp)-> typ
                           








           let assert_equal a b = assert (a = b)

let assert_raises exc f =
  match f () with
  | exception exc' when exc = exc' -> ()
  | _ -> assert false

let test_1 () =
  assert_equal
    (multi_step (RaiseDivByZero (TInt, Plus (Num 4, Num 2))))
    (RaiseDivByZero (TInt, Num 6))

let test_2 () =
  assert_equal
    (multi_step (Div (Plus (Num 4, Num 2), Num 0)))
    (RaiseDivByZero (TInt, Num 6))

let test_3 () =
  assert_equal
    (multi_step (Try (Plus (Num 4, Num 4), Lambda ("x", TInt, Var "x"))))
    (Num 8)
    let test_4 () =
      assert_equal
        (multi_step
           (Try (Div (Num 4, Num 0), Lambda ("x", TInt, Plus (Var "x", Num 1)))))
        (Num 5)

        let test_5 () =
          assert_equal
            (multi_step (Apply (Lambda ("x", TInt, Var "x"), Div (Num 4, Num 0))))
            (RaiseDivByZero (TInt, Num 4))
        
        let test_6 () =
          assert_equal
            (multi_step (If (IsZero (Div (Num 4, Num 0)), Num 1, Num 2)))
            (RaiseDivByZero (TInt, Num 4))

 
            let test_7 () =
              assert_equal
                (type_check [] (RaiseDivByZero (TInt, Plus (Num 4, Num 2))))
                TInt
            
            let test_8 () =
              assert_equal
                (type_check []
                   (Try (Div (Num 4, Num 0), Lambda ("x", TInt, Plus (Var "x", Num 1)))))
                TInt
            
            let test_9 () =
              assert_equal
                (type_check []
                   (If (IsZero (Num 0), RaiseDivByZero (TBool, Num 4), False)))
                TBool
            
            let test_10 () =
              assert_raises Type_error (fun _ ->
                  type_check []
                    (Try (Div (Num 4, Num 0), Lambda ("x", TBool, Plus (Var "x", Num 1)))))
            
            let test_11 () =
              assert_raises Type_error (fun _ ->
                  type_check [] (Try (Div (Num 4, Num 0), Lambda ("x", TInt, False))))
            










             
    let () =
      print_endline "Exceptions tests, your grade: 0.9";
      test_1 ();
      print_endline "test 1 passed";
      test_2 ();
      print_endline "test 2 passed";
      test_3 ();
      print_endline "test 3 passed";
      test_4 ();
      print_endline "test 4 passed";
      test_5 ();
      print_endline "test 5 passed";
      test_6 ();
      print_endline "test 6 passed";
      test_7 ();
      print_endline "test 7 passed";
      test_8 ();
      print_endline "test 8 passed";
      test_9 ();
      print_endline "test 9 passed";
      test_10 ();
      print_endline "test 10 passed";
      test_11 ();
      print_endline "test 11 passed";