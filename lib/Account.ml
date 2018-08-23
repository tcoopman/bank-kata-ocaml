type transaction = {
  date: string;
  amount: float;
}
type account = transaction list
let create () = []

let deposit amount account = {date= ""; amount} :: account
let withdrawal amount account = {date= ""; amount = amount *. -1.} :: account

let print account =
  let buffer = Buffer.create (List.length account * 50) in
  let rec fill_buffer = function
    | [] -> ()
    | transaction :: tl ->
      Buffer.add_string buffer (Printf.sprintf "%s || %.2f\n" transaction.date transaction.amount);
      fill_buffer tl
  in
  Buffer.add_string buffer "date || amount || balance\n";
  fill_buffer account;
  print_string (Buffer.contents buffer)