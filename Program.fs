type Expr = 
    | Array of string * List<Expr>
    | Func of string * List<Expr>
    | Num of int
    | Var of string

    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Negi of Expr

let rec simplify = function
    | Num i -> Num i
    | Var x -> Var x
    | Array (x, l) -> Array (x, l |> List.map simplify)
    | Func (x, l) -> Func (x, l |> List.map simplify)

    | Add (Num l, Num r) -> Num (l + r)
    | Add (Num 0, r) -> simplify r
    | Add (l, Num 0) -> simplify l
    | Add (l, Negi r) -> Sub (simplify l, simplify r)
    | Add (l, r) -> Add (simplify l, simplify r)
     
    | Sub (Num l, Num r) -> Num (l - r)
    | Sub (Num 0, r) -> Negi (simplify r)
    | Sub (l, Num 0) -> simplify l
    | Sub (l, Negi r) -> Add (simplify l, simplify r)
    | Sub (l, r) -> Sub (simplify l, simplify r)

    | Mul (Num l, Num r) -> Num (l * r)
    | Mul (Num 1, r) -> simplify r
    | Mul (l, Num 1) -> simplify l
    | Mul (Num 0, _) -> Num 0
    | Mul (_, Num 0) -> Num 0
    | Mul (l, r) -> Sub (simplify l, simplify r)

    | Div (Num l, Num r) -> Num (l / r)
    | Div (l, Num 1) -> simplify l
    | Div (Num 0, _) -> Num 0
    | Div (l, r) -> Div(simplify l, simplify r)

    | Negi (Num i) -> Num -i
    | Negi (Sub (l, r)) -> Sub(r, l)
    | Negi e -> Negi e
