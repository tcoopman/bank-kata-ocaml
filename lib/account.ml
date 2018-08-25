open Common

module Make(Clock: Clock_S) = struct
  type transaction = {
    date: string;
    amount: float;
  }
  type account = transaction list

  let create () = []

  let deposit ~amount account = {date = Clock.now (); amount} :: account
  let withdrawal ~amount account = {date = Clock.now (); amount = amount *. -1.} :: account

  let print account =
    let add_balance account =
      let (_, with_calculated_balance) = List.fold_right (
          fun transaction (total, calculated_transactions) -> 
            (total +. transaction.amount, (total +. transaction.amount, transaction) :: calculated_transactions)
        ) account (0., []) in
      with_calculated_balance
    in
    let to_print_statements account_with_balance =
      List.map (fun (total, transaction) -> 
          Printf.sprintf "%s || %.2f || %.2f\n" transaction.date transaction.amount total
        ) account_with_balance
    in
    let print_statements_to_string_with_header print_statements =
      let buffer = Buffer.create (List.length account * 50) in
      Buffer.add_string buffer "date || amount || balance\n";
      List.iter (Buffer.add_string buffer) print_statements;
      Buffer.contents buffer
    in

    account
    |> add_balance
    |> to_print_statements
    |> print_statements_to_string_with_header
    |> print_string
end

include Make(Clock)