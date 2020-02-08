open Core

module O = OUnit2
module M = Maitre_d
module Q = Quickcheck

let surplus_quantity_generator =
  let open Q.Generator in
  let table_size, q = map ~f:abs small_positive_int, map ~f:abs small_positive_int in
  let date = Time.of_string "2023-09-14T00:00:00" |> return in
  map3 table_size q date ~f:(fun t q date -> M.({ date; quantity = t + q }, t))

let test_quantity_larger_than_table_size_rejects_reservation (reserve: M.reservation_fn) _ =
  Q.test surplus_quantity_generator
    ~f:(fun (c, table_size) ->
        let r = M.({ tables = [{ size = table_size }]; reservations = []}) in
        let response = reserve r c in
        O.assert_equal response M.Rejected)

let deficit_quantity_generator =
  let open Q.Generator in
  let table_size, q = map ~f:abs small_positive_int, map ~f:abs small_positive_int in
  let date = Time.of_string "2023-09-14T00:00:00" |> return in
  map3 table_size q date ~f:(fun t q date -> M.({ date; quantity = min t q }, t))

let test_quantity_smaller_than_table_size_accepts_reservation (reserve: M.reservation_fn) _ =
  Q.test deficit_quantity_generator
    ~f:(fun (c, table_size) ->
        let r = M.({ tables = [{ size = table_size }]; reservations = [] }) in
        let response = reserve r c in
        O.assert_equal response M.Accepted)

let date = Time.of_string "2023-09-14T00:00:00"

let make_reservations quantities: M.reservation list =
  List.map quantities ~f:(fun q: M.reservation -> { date; quantity = q })

let existing_reservations_and_table_incr_gen =
  let open Q.Generator in
  let rs = list small_positive_int >>| make_reservations in
  let t_incr = small_positive_int in
  both rs t_incr

let test_with_existing_reservations_not_more_than_table_size_is_accepted (reserve: M.reservation_fn) _ =
  Q.test existing_reservations_and_table_incr_gen
    ~f:(fun (existing_reservations, table_incr) ->
        let total_existing_reservations = List.fold ~init:0
            ~f:(fun acc {quantity; _} -> acc + quantity) existing_reservations in
        let table_size = total_existing_reservations + table_incr in
        let quantity = Random.int (table_size - total_existing_reservations) + 1 in
        let candidate = M.({ quantity; date }) in
        let r = M.({ tables = [{ size = table_size }]; reservations = existing_reservations }) in
        let response = reserve r candidate in
        O.assert_equal response M.Accepted;
      )

let test_with_existing_reservations_request_more_than_table_size_is_rejected (reserve: M.reservation_fn) _ =
  Q.test existing_reservations_and_table_incr_gen
    ~f:(fun (existing_reservations, t_incr) ->
        let table_size = List.fold ~init:0
            ~f:(fun acc {quantity; _} -> acc + quantity) existing_reservations in
        let quantity = Random.int (t_incr + 1) + 1 in
        let candidate = M.({ quantity; date }) in
        let r = M.({ tables = [{ size = table_size }]; reservations = existing_reservations }) in
        let response = reserve r candidate in
        O.assert_equal response M.Rejected;
      )

let test_submit_reservation (reserve: M.reservation_fn) table_size existing_reservations candidate expected _ =
  let r = M.({ tables = [{ size = table_size }]; reservations = existing_reservations }) in
  let response = reserve r candidate in
  O.assert_equal response expected

let suite_no_reservations_sufficient_capacity =
  let open O in
  "Bootique restaurant with no reservations accepts reservation when there are enough tables." >::
  test_quantity_larger_than_table_size_rejects_reservation M.submit_reservation

let suite_no_reservations_insufficient_capacity =
  let open O in
  "Boutique restaurant without reservations rejects reservation if there aren't enough tables." >::
  test_quantity_smaller_than_table_size_accepts_reservation M.submit_reservation

let suite_existing_reservations_sufficient_capacity =
  let open O in
  "Boutique restaurant with reservations accepts reservation if there are enough tables." >::
  test_with_existing_reservations_not_more_than_table_size_is_accepted M.submit_reservation

let suite_existing_reservations_insufficient_capacity =
  let open O in
  "Boutique restaurant with reservations rejects reservation if there aren't enough tables." >::
  test_with_existing_reservations_request_more_than_table_size_is_rejected M.submit_reservation

let suite_existing_reservations_are_grouped_by_date =
  let existing_reservations = M.({ date = Time.of_string "2023-09-15T00:00:00"; quantity = 2 })::[] in
  let candidate = M.({ date; quantity = 3 }) in
  O.(
    "Boutique restaurant considers reservations for only the requested day." >:::
    [Printf.sprintf "table size = %d, quantity = %d" 4 candidate.quantity >::
     test_submit_reservation M.submit_reservation 4 existing_reservations candidate M.Accepted])

let test_haute_cuisine tables existing_reservations candidate expected_outcome msg _ =
  let open M in
  let restaurant = { tables; reservations = existing_reservations } in
  let outcome = submit_reservation_haute restaurant candidate in
  O.assert_equal
    ~msg
    outcome
    expected_outcome

let suite_several_tables_with_capacity_accepts_reservation =
  let open O in
  let open M in
  let tables = [{ size = 2 }; { size = 2 }; { size = 4 }; { size = 4 }] in
  let existing_reservations = [] in
  let candidate = { quantity = 4; date } in
  "Two tables for two and two tables for four should accept reservation for four" >::
  test_haute_cuisine
    tables
    existing_reservations
    candidate
    Accepted
    "Expected reservation to be accepted, but it was rejected"

let suite_several_tables_without_capacity_rejects_reservation =
  let open O in
  let open M in
  let tables = [{ size = 2 }; { size = 2 }; { size = 4 }; { size = 4 }] in
  let existing_reservations = [] in
  let candidate = { quantity = 5; date } in
  "Two tables for two and two tables for four should reject reservation for five" >::
  test_haute_cuisine
    tables
    existing_reservations
    candidate
    Rejected
    "Expected reservation to be rejected, but it was accepted"

let suite_tables_with_existing_reservations_and_capacity_accepts_reservation =
  let open O in
  let open M in
  let tables = [{ size = 2 }; { size = 2 }; { size = 4 }] in
  let date = Time.of_string "2024-06-07T00:00:00" in
  let existing_reservations = [{ quantity = 2; date };] in
  let candidate = { quantity = 4; date } in
  "Two tables for 2 and one table for 4, with existing reservation for 2, should accept reservation for 4" >::
  test_haute_cuisine
    tables
    existing_reservations
    candidate
    Accepted
    "Expected reservation to be accepted, but it was rejected"

let suite_tables_with_existing_reservations_without_capacity_rejects_reservation =
  let open O in
  let open M in
  let tables = [{ size = 2 }; { size = 2 }; { size = 4 }] in
  let date = Time.of_string "2024-06-07T00:00:00" in
  let existing_reservations = [{ quantity = 3; date }] in
  let candidate = { quantity = 4; date } in
  "Two tables for 2 and one table for 4, with existing reservation for 3, should reject reservation for 4" >::
  test_haute_cuisine
    tables
    existing_reservations
    candidate
    Rejected
    "Expected reservation to be rejected, but it was accepted"

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
  let haute_cuisine_tests =
    "Haute Cuisine tests" >:::
    [
      suite_several_tables_with_capacity_accepts_reservation;
      suite_several_tables_without_capacity_rejects_reservation;
      suite_tables_with_existing_reservations_and_capacity_accepts_reservation;
      suite_tables_with_existing_reservations_without_capacity_rejects_reservation;
    ]
  in
  let tests = test_list [boutique_restaurant_tests; haute_cuisine_tests] in
  run_test_tt_main tests
