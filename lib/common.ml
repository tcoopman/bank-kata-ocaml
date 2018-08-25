module Clock = struct
  let now () = "SystemClock"
end

module type Clock_S = module type of Clock