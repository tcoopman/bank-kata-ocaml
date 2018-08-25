module type Clock_S = sig 
  val now : unit -> string
end