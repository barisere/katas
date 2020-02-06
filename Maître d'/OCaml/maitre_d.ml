open Core

type reservation = {
  quantity: int;
  date: Date.t;
}

type table = {
  size: int;
}

type restaurant = {
  tables: table list;
  reservations: reservation list;
}

type response = Accepted | Rejected

type reservation_fn = restaurant -> reservation -> response

let submit_reservation: reservation_fn = fun restaurant reservation ->
  let has_same_date r' = Date.(reservation.date = r'.date) in
  let reserved_tables = List.fold restaurant.reservations ~init:0
      ~f:(fun acc r' -> if has_same_date r' then r'.quantity + acc else acc)
  in
  let seating_capacity = List.fold restaurant.tables ~init:0 ~f:(fun acc table -> acc + table.size) in
  let available_tables = seating_capacity - reserved_tables in
  if available_tables >= reservation.quantity then Accepted else Rejected

let submit_reservation_haute: reservation_fn = fun restaurant reservation ->
  let accept_reservations =
    let tables = List.to_array restaurant.tables in
    let () = Array.sort ~compare:(fun t t' -> Int.compare t.size t'.size) tables in
    let reservations = List.filter restaurant.reservations ~f:(fun r -> Date.(r.date = reservation.date)) in
    List.iter reservations ~f:(fun r ->
        match Array.findi tables ~f:(fun _ t -> t.size >= r.quantity) with
        | Some(idx, _) -> tables.(idx) <- { size = 0 }
        | None -> ());
    Array.filter ~f:(fun t -> t.size > 0) tables |> Array.to_list
  in
  let can_take_reservation t = t.size >= reservation.quantity in
  let candidate_tables = accept_reservations in
  if List.exists candidate_tables ~f:can_take_reservation then
    Accepted
  else Rejected
