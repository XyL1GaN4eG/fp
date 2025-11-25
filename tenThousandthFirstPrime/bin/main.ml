let () =
  let target_prime = 10_001 in
  let digits = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let tail_prime = Tailed_recursion.nth_prime target_prime in
  let non_tail_prime = Non_tailed_recursion.nth_prime target_prime in
  let modular_prime = Module_solution.nth_prime target_prime in
  let permutation =
    Permutations.nth_lexicographic_permutation_factoradic digits 1_000_000
    |> Permutations.digits_to_string
  in
  Printf.printf "Target prime index: %d\n" target_prime;
  Printf.printf "Tail recursion: %d\n" tail_prime;
  Printf.printf "Non-tail recursion: %d\n" non_tail_prime;
  Printf.printf "Modular sequence solution: %d\n" modular_prime;
  Printf.printf "Millionth lexicographic permutation: %s\n" permutation
