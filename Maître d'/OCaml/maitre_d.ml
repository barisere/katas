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
  submit_reservation restaurant reservation
