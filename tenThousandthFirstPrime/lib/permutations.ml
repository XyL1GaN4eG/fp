(* Utilities for lexicographic permutations of digit lists. The primary
   implementation uses the factoradic representation to jump directly to the
   required permutation in tail-recursive style. A secondary implementation
   relies on iterating through permutations to provide an alternative approach
   closer to the "traditional" next-permutation algorithm. *)

let factorial n =
  if n < 0 then invalid_arg "Factorial argument must be non-negative"
  else
    let rec aux acc k = if k <= 1 then acc else aux (acc * k) (k - 1) in
    aux 1 n

let rec remove_at idx = function
  | [] -> invalid_arg "Index out of range"
  | x :: xs ->
      if idx = 0 then (x, xs)
      else
        let picked, rest = remove_at (idx - 1) xs in
        (picked, x :: rest)

let digits_to_int digits =
  List.fold_left (fun acc d -> (acc * 10) + d) 0 digits

let digits_to_string digits =
  digits |> List.map string_of_int |> String.concat ""

let nth_lexicographic_permutation_factoradic digits position =
  if position < 1 then invalid_arg "Position must be positive";
  let rec build acc remaining idx =
    match remaining with
    | [] -> List.rev acc
    | _ ->
        let len = List.length remaining in
        let block = factorial (len - 1) in
        let block_idx = (idx - 1) / block in
        let next_idx = ((idx - 1) mod block) + 1 in
        let picked, rest = remove_at block_idx remaining in
        build (picked :: acc) rest next_idx
  in
  build [] (List.sort compare digits) position

let next_permutation digits =
  let arr = Array.of_list digits in
  let len = Array.length arr in
  let rec find_pivot i =
    if i <= 0 then None
    else if arr.(i - 1) < arr.(i) then Some (i - 1)
    else find_pivot (i - 1)
  in
  match find_pivot (len - 1) with
  | None -> None
  | Some pivot ->
      let rec find_successor j candidate =
        if j = len then candidate
        else
          let candidate =
            if arr.(j) > arr.(pivot)
               && (candidate = -1 || arr.(j) <= arr.(candidate))
            then j
            else candidate
          in
          find_successor (j + 1) candidate
      in
      let succ = find_successor (pivot + 1) (-1) in
      if succ = -1 then invalid_arg "No successor available";
      let swap i j =
        let tmp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- tmp
      in
      swap pivot succ;
      let rec reverse l r =
        if l < r then (
          swap l r;
          reverse (l + 1) (r - 1))
      in
      reverse (pivot + 1) (len - 1);
      Some (Array.to_list arr)

let nth_lexicographic_permutation_iterative digits position =
  if position < 1 then invalid_arg "Position must be positive";
  let rec step current idx =
    if idx = position then current
    else
      match next_permutation current with
      | None -> invalid_arg "Position exceeds number of permutations"
      | Some next -> step next (idx + 1)
  in
  step (List.sort compare digits) 1
