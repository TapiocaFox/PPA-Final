structure Main =
struct
  open Parallel

  fun parse_floats s =
    let
      val tokens = String.tokens Char.isSpace s
      fun parse_one t =
        case Real.fromString t of
          SOME v => v
        | NONE => raise Fail ("Could not parse float: " ^ t)
    in
      List.map parse_one tokens
    end

  fun norm3 (x1, y1, z1) (x2, y2, z2) =
    let
      val dx = x1 - x2
      val dy = y1 - y2
      val dz = z1 - z2
    in
      Math.sqrt (dx*dx + dy*dy + dz*dz)
    end

  fun array_to_list arr =
    let
      fun loop i acc =
        if i < 0 then acc
        else loop (i-1) (Array.sub(arr, i)::acc)
    in
      loop (Array.length arr - 1) []
    end

  fun main () =
    let
      val args = CommandLine.arguments ()
      val h = case Real.fromString (List.nth(args, 0)) of
        SOME v => v
      | NONE => raise Fail ("Could not parse h argument: " ^ List.nth(args, 0))
      val lines = List.filter (fn s => size s > 0) (String.tokens (fn c => c = #"\n") (TextIO.inputAll TextIO.stdIn))
      val point = case lines of
        p::_ => (case parse_floats p of [x, y, z] => (x, y, z) | _ => raise Fail "Invalid point")
      val points_list = List.map (fn s => case parse_floats s of [x, y, z] => (x, y, z) | _ => raise Fail "Invalid point") (List.tl lines)
      val n = List.length points_list
      val points = Array.fromList points_list

      (* This function computes the (index, distance) pair *)
      fun idx_dist i = (i, norm3 point (Array.sub(points, i)))

      val min_result =
        Parallel.reduce
          (fn ((i1, d1), (i2, d2)) => if d1 < d2 then (i1, d1) else (i2, d2))
          (0, h)
          (0, n)
          idx_dist

      val filtered =
        Parallel.filter (0, n)
          idx_dist
          (fn i => #2 (idx_dist i) < h)
      val filtered_list = Seq.toList filtered
      val indices = List.map #1 filtered_list
      val norms = List.map #2 filtered_list
      val indices_str = String.concatWith " " (List.map Int.toString indices)
      val norms_str = String.concatWith " " (List.map Real.toString norms)
    in
      print (indices_str ^ "\n" ^ norms_str ^ "\n")
    end
end

val _ = Main.main ()
