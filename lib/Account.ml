type transaction = {
  date: string;
  amount: float;
}
type account = transaction list

let create () = []

let deposit amount account = {date= "14/01/2012"; amount} :: account
let withdrawal amount account = {date= "14/01/2012"; amount = amount *. -1.} :: account

let print account =
  let calculate account =
    List.fold_right (fun transaction (total, x) -> (total +. transaction.amount, (total +. transaction.amount, transaction) :: x)) account (0., [])
  in
  let buffer = Buffer.create (List.length account * 50) in
  let rec fill_buffer = function
    | [] -> ()
    | (total, transaction) :: tl ->
      Buffer.add_string buffer (Printf.sprintf "%s || %.2f || %.2f\n" transaction.date transaction.amount total);
      fill_buffer tl
  in
  let (_, calculated) = calculate account in
  Buffer.add_string buffer "date || amount || balance\n";
  fill_buffer calculated;
  print_string (Buffer.contents buffer)