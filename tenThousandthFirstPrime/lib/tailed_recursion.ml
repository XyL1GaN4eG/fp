let is_number_divided_by_digit n d = n mod d = 0

let is_prime n =
  if n = 2 then true
  else if n < 2 || n mod 2 = 0 then false
  else
    let rec loop denominator =
      if denominator * denominator > n then true
      else if is_number_divided_by_digit n denominator then false
      else loop (denominator + 2)
    in
    loop 3

let nth_prime n =
  let rec loop count cand last =
    if count = n then last
    else if is_prime cand then loop (count + 1) (cand + 2) cand
    else loop count (cand + 2) last
  in
  if n < 1 then invalid_arg "Prime index must be positive"
  else if n = 1 then 2
  else loop 1 3 2
