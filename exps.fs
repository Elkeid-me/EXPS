type Expr =
    | Array of string * List<Expr>
    | Func of string * List<Expr>
    | Num of int
    | Var of string

    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Nega of Expr

let simplify expression =
    let rec simplify_impl = function
        | Num i -> Num i
        | Var x -> Var x
        | Array (x, l) -> Array (x, l |> List.map simplify_impl)
        | Func (x, l) -> Func (x, l |> List.map simplify_impl)

        | Add (l, r) -> match simplify_impl l, simplify_impl r with
                        | Num l, Num r -> Num (l + r)
                        | Num 0, r -> r
                        | l, Num 0 -> l
                        | l, Nega r -> Sub (l, r)
                        | Mul (a, b), Mul (c, d) when a = c -> Mul (a, Add (b, d))
                        | Mul (a, b), Mul (c, d) when a = d -> Mul (a, Add (b, c))
                        | Mul (a, b), Mul (c, d) when b = c -> Mul (b, Add (a, d))
                        | Mul (a, b), Mul (c, d) when b = d -> Mul (b, Add (a, c))
                        | l, r -> Add (l, r)

        | Sub (l, r) -> match simplify_impl l, simplify_impl r with
                        | Num l, Num r -> Num (l - r)
                        | Num 0, r -> Nega (r)
                        | l, Num 0 -> l
                        | l, Nega r -> Add (l, r)
                        | l, r when l = r -> Num 0
                        | l, r -> Sub (l, r)

        | Mul (l, r) -> match simplify_impl l, simplify_impl r with
                        | Num l, Num r -> Num (l * r)
                        | Num 1, r -> r
                        | l, Num 1 -> l
                        | Num 0, _ -> Num 0
                        | _, Num 0 -> Num 0
                        | l, r -> Mul (l, r)

        | Div (l, r) -> match simplify_impl l, simplify_impl r with
                        | _, Num 0 -> Num 2147483647
                        | Num l, Num r -> Num (l / r)
                        | l, Num 1 -> l
                        | Num 0, _ -> Num 0
                        | l, r when l = r -> Num 1
                        | l, r -> Div (l, r)

        | Nega e -> match simplify_impl e with
                    | Num i -> Num -i
                    | Nega e -> e
                    | Sub (l, r) -> Sub (r, l)
                    | e -> Nega e
    expression |> simplify_impl

let rec print_expr = function
    | Num i -> string i
    | Var x -> x
    | Array (x, l) -> x + "[]"
    | Func (x, l) -> x + "()"

    | Add (l, r) -> "(" + print_expr l + " + " + print_expr r + ")"
    | Sub (l, r) -> "(" + print_expr l + " - " + print_expr r + ")"
    | Mul (l, r) -> "(" + print_expr l + " * " + print_expr r + ")"
    | Div (l, r) -> "(" + print_expr l + " / " + print_expr r + ")"
    | Nega e -> "-" + print_expr e

let e = Add (Mul (Var "x", Add (Var "y", Num 0)), Mul (Mul (Var "x", Num 1), Add (Var "z", Num 1)))
printf "%s\n    => %s" (print_expr e) (e |> simplify |> print_expr)
