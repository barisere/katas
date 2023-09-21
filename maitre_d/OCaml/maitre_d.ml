open Core

module Time = Time_float

type reservation = {
  quantity: int;
  date: Time.t;
}

type table = {
  size: int;
}

type restaurant = {
  tables: table list;
  reservations: reservation list;
}

type response = Accepted | Rejected

let has_same_date r r' =
  let open Time in
  Date.(to_date ~zone:Zone.utc r.date = to_date ~zone:Zone.utc r'.date)

let submit_reservation restaurant reservation =
  let reserved_tables = List.fold restaurant.reservations ~init:0
      ~f:(fun acc r' -> if has_same_date reservation r' then r'.quantity + acc else acc)
  in
  let seating_capacity = List.fold restaurant.tables ~init:0 ~f:(fun acc table -> acc + table.size) in
  let available_tables = seating_capacity - reserved_tables in
  if available_tables >= reservation.quantity then Accepted else Rejected

let accept_reservations ~f restaurant =
  let tables = List.to_array restaurant.tables in
  let () = Array.sort ~compare:(fun t t' -> Int.compare t.size t'.size) tables in
  let reservations = List.filter restaurant.reservations ~f in
  List.iter reservations ~f:(fun r ->
      match Array.findi tables ~f:(fun _ t -> t.size >= r.quantity) with
      | Some(idx, _) -> tables.(idx) <- { size = 0 }
      | None -> ());
  Array.filter ~f:(fun t -> t.size > 0) tables |> Array.to_list

let submit_reservation_haute restaurant reservation =
  let can_take_reservation t = t.size >= reservation.quantity in
  let candidate_tables = accept_reservations ~f:(has_same_date reservation) restaurant in
  if List.exists candidate_tables ~f:can_take_reservation then
    Accepted
  else Rejected

let submit_reservation_2nd_seating seating_duration restaurant reservation =
  let within_same_seating r' =
    let r_time_of_day = Time.to_ofday ~zone:Time.Zone.utc reservation.date in
    let r'_time_of_day = Time.to_ofday ~zone:Time.Zone.utc r'.date in
    let time_diff = Time.Ofday.diff r_time_of_day r'_time_of_day in
    Time.Span.(abs time_diff < seating_duration)
  in
  let available_tables = accept_reservations
      ~f:(fun r -> has_same_date reservation r && within_same_seating r)
      restaurant
  in
  if List.exists available_tables ~f:(fun t -> t.size >= reservation.quantity) then
    Accepted
  else Rejected
