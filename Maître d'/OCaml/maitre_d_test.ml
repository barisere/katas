open Core

module O = OUnit2
module M = Maitre_d

let test_submit_reservation (module R: M.Restaurant) table_size existing_reservations candidate expected _ =
  let r = R.make table_size existing_reservations in
  let response = R.submit_reservation r candidate in
  O.assert_equal response expected

let date = Date.of_string "2023-09-14"

let make_reservations quantities: M.reservation list =
  List.map quantities ~f:(fun q: M.reservation -> { date; quantity = q })

let suite_no_reservations_sufficient_capacity =
  let reservations = make_reservations [1; 12] in
  O.(
    "Bootique restaurant with no reservations accepts reservation when there are enough tables." >:::
    List.map ~f:(fun r ->
        (Printf.sprintf "table size = %d, quantity = %d" 12 r.quantity) >::
        test_submit_reservation (module M.BoutiqueRestaurant) 12 [] r M.Accepted) reservations)

let suite_no_reservations_insufficient_capacity =
  let r = M.({ date; quantity = 13 }) in
  O.(
    "Boutique restaurant without reservations rejects reservation if there aren't enough tables." >:::
    [ Printf.sprintf "table size = %d, quantity = %d" 12 r.quantity >::
      test_submit_reservation (module M.BoutiqueRestaurant) 12 [] r M.Rejected ])

let suite_existing_reservations_sufficient_capacity =
  let existing_reservations = make_reservations [2; 2] in
  let new_reservations = make_reservations [3; 3] in
  O.(
    "Boutique restaurant with reservations accepts reservation if there are enough tables." >:::
    List.map ~f:(fun r ->
        (Printf.sprintf "table size = %d, quantity = %d" 10 r.quantity) >::
        test_submit_reservation (module M.BoutiqueRestaurant) 10 existing_reservations r M.Accepted) new_reservations)

let suite_existing_reservations_insufficient_capacity =
  let existing_reservations = make_reservations [3; 2; 3] in
  let candidate = M.({ date; quantity = 3 }) in
  O.(
    "Boutique restaurant with reservations accepts reservation if there aren't enough tables." >:::
    [Printf.sprintf "table size = %d, quantity = %d" 10 candidate.quantity >::
     test_submit_reservation (module M.BoutiqueRestaurant) 10 existing_reservations candidate M.Rejected])

let suite_existing_reservations_are_grouped_by_date =
  let existing_reservations = M.({ date = Date.of_string "2023-09-15"; quantity = 2 })::[] in
  let candidate = M.({ date; quantity = 3 }) in
  O.(
    "Boutique restaurant considers reservations for only the requested day." >:::
    [Printf.sprintf "table size = %d, quantity = %d" 4 candidate.quantity >::
     test_submit_reservation (module M.BoutiqueRestaurant) 4 existing_reservations candidate M.Accepted])

let () =
  let open O in
  let boutique_restaurant_tests =
    "Boutique restaurant tests" >:::
    [
      suite_no_reservations_insufficient_capacity;
      suite_no_reservations_sufficient_capacity;
      suite_existing_reservations_sufficient_capacity;
      suite_existing_reservations_insufficient_capacity;
      suite_existing_reservations_are_grouped_by_date
    ]
  in
  run_test_tt_main boutique_restaurant_tests
