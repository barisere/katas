open Core

module O = OUnit2
module M = Maitre_d
module Q = Quickcheck

let surplus_quantity_generator =
  let open Q.Generator in
  let table_size, q = map ~f:abs small_positive_int, map ~f:abs small_positive_int in
  let date = Date.of_string "2023-09-14" |> return in
  map3 table_size q date ~f:(fun t q date -> M.({ date; quantity = t + q}, t))

let test_quantity_larger_than_table_size_rejects_reservation (module R: M.Restaurant) _ =
  Q.test surplus_quantity_generator
    ~f:(fun (c, table_size) ->
        let r = R.make table_size [] in
        let response = R.submit_reservation r c in
        O.assert_equal response M.Rejected)

let deficit_quantity_generator =
  let open Q.Generator in
  let table_size, q = map ~f:abs small_positive_int, map ~f:abs small_positive_int in
  let date = Date.of_string "2023-09-14" |> return in
  map3 table_size q date ~f:(fun t q date -> M.({ date; quantity = min t q }, t))

let test_quantity_smaller_than_table_size_accepts_reservation (module R: M.Restaurant) _ =
  Q.test deficit_quantity_generator
    ~f:(fun (c, table_size) ->
        let r = R.make table_size [] in
        let response = R.submit_reservation r c in
        O.assert_equal response M.Accepted)

let date = Date.of_string "2023-09-14"

let make_reservations quantities: M.reservation list =
  List.map quantities ~f:(fun q: M.reservation -> { date; quantity = q })

let existing_reservations_and_table_incr_gen =
  let open Q.Generator in
  let rs = list small_positive_int >>| make_reservations in
  let t_incr = small_positive_int in
  both rs t_incr

let test_with_existing_reservations_not_more_than_table_size_is_accepted (module R: M.Restaurant) _ =
  Q.test existing_reservations_and_table_incr_gen
    ~f:(fun (existing_reservations, table_incr) ->
        let total_existing_reservations = List.fold ~init:0
            ~f:(fun acc {quantity; _} -> acc + quantity) existing_reservations in
        let table_size = total_existing_reservations + table_incr in
        let quantity = Random.int (table_size - total_existing_reservations) + 1 in
        let candidate = M.({ quantity; date }) in
        let r = R.make table_size existing_reservations in
        let response = R.submit_reservation r candidate in
        O.assert_equal response M.Accepted;
      )

let test_with_existing_reservations_request_more_than_table_size_is_rejected (module R: M.Restaurant) _ =
  Q.test existing_reservations_and_table_incr_gen
    ~f:(fun (existing_reservations, t_incr) ->
        let table_size = List.fold ~init:0
            ~f:(fun acc {quantity; _} -> acc + quantity) existing_reservations in
        let quantity = Random.int (t_incr + 1) + 1 in
        let candidate = M.({ quantity; date }) in
        let r = R.make table_size existing_reservations in
        let response = R.submit_reservation r candidate in
        Printf.printf "table size = %d, quantity = %d\n" table_size quantity;
        O.assert_equal response M.Rejected;
      )

let test_submit_reservation (module R: M.Restaurant) table_size existing_reservations candidate expected _ =
  let r = R.make table_size existing_reservations in
  let response = R.submit_reservation r candidate in
  O.assert_equal response expected

let suite_no_reservations_sufficient_capacity =
  let open O in
  "Bootique restaurant with no reservations accepts reservation when there are enough tables." >::
  test_quantity_larger_than_table_size_rejects_reservation (module M.BoutiqueRestaurant)

let suite_no_reservations_insufficient_capacity =
  let open O in
  "Boutique restaurant without reservations rejects reservation if there aren't enough tables." >::
  test_quantity_smaller_than_table_size_accepts_reservation (module M.BoutiqueRestaurant)

let suite_existing_reservations_sufficient_capacity =
  let open O in
  "Boutique restaurant with reservations accepts reservation if there are enough tables." >::
  test_with_existing_reservations_not_more_than_table_size_is_accepted (module M.BoutiqueRestaurant)

let suite_existing_reservations_insufficient_capacity =
  let open O in
  "Boutique restaurant with reservations rejects reservation if there aren't enough tables." >::
  test_with_existing_reservations_request_more_than_table_size_is_rejected (module M.BoutiqueRestaurant)

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
