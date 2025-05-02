structure Main =
struct
  fun parse_floats s =
    List.map (valOf o Real.fromString) (String.tokens Char.isSpace s)

  fun norm3 (x1, y1, z1) (x2, y2, z2) =
    let
      val dx = x1 - x2
      val dy = y1 - y2
      val dz = z1 - z2
    in
      Math.sqrt (dx*dx + dy*dy + dz*dz)
    end

  fun main () =
    let
      val args = CommandLine.arguments ()
      val filename = List.nth(args, 0)
      val h = valOf (Real.fromString (List.nth(args, 1)))
      val ins = TextIO.openIn filename
      val lines = List.filter (fn s => size s > 0) (String.tokens (fn c => c = #"\n") (TextIO.inputAll ins))
      val _ = TextIO.closeIn ins
      val point = case lines of
        p::_ => (case parse_floats p of [x, y, z] => (x, y, z) | _ => raise Fail "Invalid point")
      val points = List.map (fn s => case parse_floats s of [x, y, z] => (x, y, z) | _ => raise Fail "Invalid point") (List.tl lines)
      fun indexed xs = ListPair.zip (List.tabulate (length xs, fn i => i), xs)
      val results =
        List.filter (fn (_, norm) => norm < h)
          (List.map (fn (i, p2) => (i, norm3 point p2)) (indexed points))
      val indices = List.map #1 results
      val norms = List.map #2 results
      val indices_str = String.concatWith " " (List.map Int.toString indices)
      val norms_str = String.concatWith " " (List.map Real.toString norms)
    in
      print (indices_str ^ "\n" ^ norms_str ^ "\n")
    end
end

val _ = Main.main ()
