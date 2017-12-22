open Core

module T = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

include T