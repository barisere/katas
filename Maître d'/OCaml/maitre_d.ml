open Core

type reservation = {
  quantity: int;
  date: Date.t;
}

type restaurant = {
  table_size: int;
  reservations: reservation list;
}

type response = Accepted | Rejected

module type Restaurant = sig
  type t
  val make: int -> reservation list -> t
  val submit_reservation: t -> reservation -> response
end

module BoutiqueRestaurant: Restaurant = struct
  type t = restaurant
  let make s rs = { table_size = s; reservations = rs }
  let submit_reservation t r =
    let has_same_date r' = Date.(r.date = r'.date) in
    let reserved_tables = List.fold t.reservations ~init:0
        ~f:(fun acc r' -> if has_same_date r' then r'.quantity + acc else acc)
    in
    let available_tables = t.table_size - reserved_tables in
    if available_tables >= r.quantity then Accepted else Rejected
end

