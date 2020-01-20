
type expr = Literal of int
        | Prod of (expr * expr)
        | Sum of (expr * expr)

let rec eval_expr e = match e with
        | Literal n             -> n
        | Prod (lhs, rhs)       -> (eval_expr lhs) * (eval_expr rhs)
        | Sum (lhs, rhs)        -> (eval_expr lhs) + (eval_expr rhs)

let () = 
        let e1 = Literal 42 in
        let e2 = Sum (Literal 40, Literal 2) in (*  40 + 2  *)
        let e3 = Prod (Literal 21, Literal 2) in (*  21 * 2  *)
        let e4 = Prod (
                Sum (Literal 1, Literal 2),
                Sum (Literal 3, Literal 4)
        ) in                                    (* (1 + 2) * (3 + 4)  *)
        let e5 = Sum (
                Sum (
                        Literal 1,
                        Prod (Literal 2, Literal 3)
                ),
                Literal 4
        ) in                                    (* 1 + 2*3  + 4  *)

        Printf.printf "42 = %d\n" (eval_expr e1);
        Printf.printf "40 + 2 = %d\n" (eval_expr e2);
        Printf.printf "21 * 2 = %d\n" (eval_expr e3);
        Printf.printf "(1 + 2) * (3 + 4) = %d\n" (eval_expr e4);
        Printf.printf "1 + 2*3 + 4 = %d\n" (eval_expr e5)

