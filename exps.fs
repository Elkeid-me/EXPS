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

let rec print_expr = function
    | Num i -> string i
    | Var x -> x
    | Array (x, _) -> x + "[]"
    | Func (x, _) -> x + "()"

    | Add (l, r) -> "(" + print_expr l + " + " + print_expr r + ")"
    | Sub (l, r) -> "(" + print_expr l + " - " + print_expr r + ")"
    | Mul (l, r) -> "(" + print_expr l + " * " + print_expr r + ")"
    | Div (l, r) -> "(" + print_expr l + " / " + print_expr r + ")"
    | Nega e -> "-" + print_expr e

let rec simplify expression =
    let simplify_impl = function
        | Num i -> Num i
        | Var x -> Var x
        | Array (x, l) -> Array (x, l |> List.map simplify)
        | Func (x, l) -> Func (x, l |> List.map simplify)
        | Add (l, r) -> match simplify l, simplify r with
                        | Num l, Num r -> Num (l + r)
                        | Num 0, r -> r
                        | l, Num 0 -> l
                        | Num a, Add (Num b, r)
                        | Add (Num b, r), Num a
                        | Num a, Add (r, Num b)
                        | Add (r, Num b), Num a -> Add (Num (a + b), r)
                        | Sub (Num b, r), Num a
                        | Num a, Sub (Num b, r) -> Sub (Num (a + b), r)
                        | Num a, Sub (r, Num b)
                        | Sub (r, Num b), Num a -> Add (r, Num (a - b))
                        | l, Nega r -> Sub (l, r)
                        | Mul (a, b), Mul (c, d) when a = c -> Mul (a, Add (b, d)) // ab + ad
                        | Mul (a, b), Mul (c, d) when a = d -> Mul (a, Add (b, c)) // ab + ca
                        | Mul (a, b), Mul (c, d) when b = c -> Mul (b, Add (a, d)) // ab + bd
                        | Mul (a, b), Mul (c, d) when b = d -> Mul (b, Add (a, c)) // ab + cb
                        | l, r -> Add (l, r)
        | Sub (l, r) -> match simplify l, simplify r with
                        | Num l, Num r -> Num (l - r)
                        | Num 0, r -> Nega (r)
                        | l, Num 0 -> l
                        | Num a, Add (Num b, r) // a - (b + r)
                        | Num a, Add (r, Num b) // a - (r + b)
                            -> Sub (Num (a - b), r)
                        | Add (Num b, r), Num a // (b + r) - a
                        | Num a, Sub (r, Num b) // a - (r - b)
                        | Add (r, Num b), Num a // (r + b) - a
                            -> Sub (Num (a + b), r)
                        | Num a, Sub (Num b, r) // a - (b - r)
                            -> Add (Num (a - b), r)
                        | Sub (Num b, r), Num a // (b - r) - a
                            -> Sub (Num (b - a), r)
                        | Sub (r, Num b), Num a // (r - b) - a
                            -> Sub (r, Num (a + b))
                        | Mul (a, b), Mul (c, d) when a = c -> Mul (a, Sub (b, d)) // ab - ad
                        | Mul (a, b), Mul (c, d) when a = d -> Mul (a, Sub (b, c)) // ab - ca
                        | Mul (a, b), Mul (c, d) when b = c -> Mul (b, Sub (a, d)) // ab - bd
                        | Mul (a, b), Mul (c, d) when b = d -> Mul (b, Sub (a, c)) // ab - cb
                        | l, Nega r -> Add (l, r)
                        | l, r when l = r -> Num 0
                        | l, r -> Sub (l, r)
        | Mul (l, r) -> match simplify l, simplify r with
                        | Num l, Num r -> Num (l * r)
                        | Num 1, r -> r
                        | l, Num 1 -> l
                        | Num -1, r -> Nega r
                        | l, Num -1 -> Nega l
                        | Num 0, _ -> Num 0
                        | _, Num 0 -> Num 0
                        | Num a, Mul (Num b, r)
                        | Num a, Mul (r, Num b)
                        | Mul (Num b, r), Num a
                        | Mul (r, Num b), Num a -> Mul (Num (a * b), r)
                        | l, r -> Mul (l, r)
        | Div (l, r) -> match simplify l, simplify r with
                        | l, r when l = r -> Num 1
                        | _, Num 0 -> Num 2147483647
                        | Num l, Num r -> Num (l / r)
                        | l, Num 1 -> l
                        | l, Num -1 -> Nega l
                        | Num 0, _ -> Num 0
                        | Mul (a, b), c when b = c -> a
                        | Mul (a, b), c when a = c -> b
                        | Mul (a, b), Nega c when b = c -> Nega a
                        | Mul (a, b), Nega c when a = c -> Nega b
                        | l, r -> Div (l, r)
        | Nega e -> match simplify e with
                    | Num i -> Num -i
                    | Nega e -> e
                    | Sub (l, r) -> Sub (r, l)
                    | e -> Nega e

    let e = simplify_impl expression
    if e = expression then
        e
    else
        simplify e


let e = Add (Mul (Add (Num 1, Mul (Num 1, Mul (Nega (Num 4), Div(Var "y", Add (Nega (Num 1), Add (Num 4, Nega (Var "x"))))))), Num 2),
             Nega (Mul (Add (Num 1, Mul (Num 1, Mul (Nega (Num 4), Div(Var "y", Add (Nega (Num 1), Add (Num 4, Nega (Var "x"))))))), Num 2)))
printf "%s\n    => %s" (print_expr e) (e |> simplify |> print_expr)
