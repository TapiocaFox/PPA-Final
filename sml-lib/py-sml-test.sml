(* Reduce *)
structure MyReduce =
struct
  fun sum_squares (lo, hi) =
    Parallel.reduce (op +) 0 (lo, hi) (fn i => i*i)
end

(* Main *)
structure Main =
struct
  fun main () =
    let
      val (lo, hi) =
        case CommandLine.arguments () of
            [lo, hi] => (valOf (Int.fromString lo), valOf (Int.fromString hi))
          | _ => (0, 10)
      val result = MyReduce.sum_squares (lo, hi)
    in
      print (Int.toString result ^ "\n")
    end
end

val _ = Main.main ()