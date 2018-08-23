
open! Base
open! Stdio

let%expect_test "Printing the statements should contain all transactions" =
  Lib.Account.create ()
  |> Lib.Account.deposit 1000.0
  |> Lib.Account.deposit 2000.0
  |> Lib.Account.withdrawal 500.0
  |> Lib.Account.print;
  [%expect{|
date || amount || balance
14/01/2012 || -500.00 || 2500.00
13/01/2012 || 2000.00 || || 3000.00
10/01/2012 || 1000.00 || || 1000.00 |}]