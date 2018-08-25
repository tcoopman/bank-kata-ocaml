open! Base
open! Stdio

module type TestDates = sig
  val dates : string array
end

module TestClock (D : TestDates) = struct
  let i = ref 0
  let now () =
    let date = D.dates.(!i) in
    i := !i + 1;
    date
end

let%expect_test "Printing the statements should contain all transactions" =
  let module Account = Lib.Account.Make(TestClock(struct let dates = [|"10/01/2012"; "13/01/2012"; "14/01/2012"|] end)) in
  Account.create ()
  |> Account.deposit ~amount:1000.0
  |> Account.deposit ~amount:2000.0
  |> Account.withdrawal ~amount:500.0
  |> Account.print;
  [%expect{|
    date || amount || balance
    14/01/2012 || -500.00 || 2500.00
    13/01/2012 || 2000.00 || 3000.00
    10/01/2012 || 1000.00 || 1000.00
  |}]