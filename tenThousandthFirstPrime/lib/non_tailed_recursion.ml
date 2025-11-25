(* объявление функции *)
let is_prime n =
  (* объявление рекурсивной функции *)
  let rec trial d =
    (* если квадрат текущего числа больше n-ого числа, то возвращаем true (почему) *)
    if d * d > n then true
      (* если если n делится на d без остатка то непростое числоа *)
    else if n mod d = 0 then false
      (* во всех остальных случаях мы вызываем эту же функцию и передаем данные: 
      текущее проверяемое число + если это первый цикл - то передаем 1 чтобы получить нечетное число, если 
      второй+ цикл то передаем 2 (опять же чтобы получить нечетное)
      *)
    else trial (d + if d = 2 then 1 else 2)
  in
  (* так как использовали конструкцию `let funcname .. in ..`,
   то функция trial становится локальной и может использоваться только в строчке ниже *)
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
