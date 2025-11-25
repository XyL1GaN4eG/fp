let is_prime n =
  let rec trial d =
    if d * d > n then true
    else if n mod d = 0 then false
    else trial (d + if d = 2 then 1 else 2)
  in
  n >= 2 && (n = 2 || (n land 1 = 1 && trial 2))

let nth_prime n =
  let rec step count cand =
    if count = n then cand - 2
    else if is_prime cand then step (count + 1) (cand + 2)
    else step count (cand + 2)
  in
  if n < 1 then invalid_arg "Prime index must be positive"
  else if n = 1 then 2
  else step 1 3
