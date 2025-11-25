open Alcotest

let prime_index = 10_001
let expected_prime = 104_743
let expected_permutation = "2783915460"
let expected_permutation_value = 2_783_915_460

let test_tail_prime () =
  check int "tail recursion" expected_prime
    (Tailed_recursion.nth_prime prime_index)

let test_non_tail_prime () =
  check int "non-tail recursion" expected_prime
    (Non_tailed_recursion.nth_prime prime_index)

let test_modular_prime () =
  check int "modular sequence" expected_prime
    (Module_solution.nth_prime prime_index)

let test_factoradic_permutation () =
  let digits = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let permutation =
    Permutations.nth_lexicographic_permutation_factoradic digits 1_000_000
    |> Permutations.digits_to_string
  in
  check string "factoradic millionth permutation" expected_permutation permutation

let test_factoradic_permutation_value () =
  let digits = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let permutation_value =
    Permutations.nth_lexicographic_permutation_factoradic digits 1_000_000
    |> Permutations.digits_to_int
  in
  check int "factoradic millionth permutation (int)" expected_permutation_value
    permutation_value

let test_iterative_small_permutation () =
  let digits = [ 0; 1; 2 ] in
  let third_perm =
    Permutations.nth_lexicographic_permutation_iterative digits 3
    |> Permutations.digits_to_string
  in
  check string "iterative third permutation" "102" third_perm

let () =
  run "Project Euler problems"
    [
      ( "Problem 7",
        [
          test_case "tail recursion prime" `Quick test_tail_prime;
          test_case "non-tail recursion prime" `Quick test_non_tail_prime;
          test_case "modular sequence prime" `Quick test_modular_prime;
        ] );
      ( "Problem 24",
        [
          test_case "factoradic millionth permutation" `Slow
            test_factoradic_permutation;
          test_case "factoradic millionth permutation value" `Slow
            test_factoradic_permutation_value;
          test_case "iterative small permutation" `Quick
            test_iterative_small_permutation;
        ] );
    ]
