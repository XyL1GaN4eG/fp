(* Modular solution for generating prime numbers using sequences and higher-order
   functions. The functions here focus on decomposing the task into reusable
   pieces: sequence generation, filtering and selecting the desired element. *)

let numbers_from start =
  Seq.unfold (fun n -> Some (n, n + 1)) start

let is_prime n =
  if n < 2 then false
  else
    let limit = int_of_float (sqrt (float_of_int n)) in
    numbers_from 2
    |> Seq.take_while (fun d -> d <= limit)
    |> Seq.for_all (fun d -> n mod d <> 0)

let primes () = numbers_from 2 |> Seq.filter is_prime

let rec seq_nth seq idx =
  match seq () with
  | Seq.Nil -> invalid_arg "Sequence shorter than requested index"
  | Seq.Cons (x, tl) -> if idx = 0 then x else seq_nth tl (idx - 1)

let nth_prime n =
  if n < 1 then invalid_arg "Prime index must be positive";
  seq_nth (primes ()) (n - 1)
