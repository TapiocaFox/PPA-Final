(* closest-point-spatial.sml *)

(* Utility functions *)
fun split sep s = String.tokens (fn c => c = sep) s
fun parse_floats s =
  let
    fun get (SOME x) = x | get NONE = raise Fail "parse_floats"
  in
    List.map get (List.map Real.fromString (String.tokens (fn c => c = #" ") s))
  end
fun parse_ints s =
  let
    fun get (SOME x) = x | get NONE = raise Fail "parse_ints"
  in
    List.map get (List.map Int.fromString (String.tokens (fn c => c = #" ") s))
  end

(* Read the spatial index file *)
fun read_spatial_index filename =
  let
    val ins = TextIO.openIn filename
    val grid_size = parse_ints (valOf (TextIO.inputLine ins))
    val bbox_min = 
      let val [x, y, z] = parse_floats (valOf (TextIO.inputLine ins))
      in (x, y, z) end
    val unit_size = 
      let val [u] = parse_floats (valOf (TextIO.inputLine ins))
      in u end
    val num_points = hd (parse_ints (valOf (TextIO.inputLine ins)))
    val points =
      let
        fun read_points 0 acc = List.rev acc
          | read_points n acc =
              case TextIO.inputLine ins of
                NONE => raise Fail "Unexpected EOF"
              | SOME line =>
                  let
                    val [x, y, z] = parse_floats line
                  in
                    read_points (n-1) ((x, y, z)::acc)
                  end
      in
        read_points num_points []
      end
    val (gx, gy, gz) = case grid_size of [a, b, c] => (a, b, c) | _ => raise Fail "bad grid size"
    fun read_cells 0 acc = List.rev acc
      | read_cells n acc =
          case TextIO.inputLine ins of
            NONE => raise Fail "Unexpected EOF"
          | SOME line =>
              let
                val indices = if String.size (String.implode (List.filter (fn c => not (Char.isSpace c)) (String.explode line))) = 0
                              then []
                              else parse_ints line
              in
                read_cells (n-1) (indices::acc)
              end
    val num_cells = gx * gy * gz
    val cells = read_cells num_cells []
    val _ = TextIO.closeIn ins
  in
    (grid_size, bbox_min, unit_size, points, cells)
  end

(* Hardcoded bounding box min and unit size (should match Python) *)

fun get_cell_index (grid_size as [gx, gy, gz], (xmin, ymin, zmin), unit) (x, y, z) =
  let
    val ix = let val i = Real.floor ((x - xmin) / unit) in if i < 0 then 0 else if i > gx-1 then gx-1 else i end
    val iy = let val i = Real.floor ((y - ymin) / unit) in if i < 0 then 0 else if i > gy-1 then gy-1 else i end
    val iz = let val i = Real.floor ((z - zmin) / unit) in if i < 0 then 0 else if i > gz-1 then gz-1 else i end
  in
    (ix, iy, iz)
  end

fun flatten_index (gx, gy, gz) (ix, iy, iz) = ix * gy * gz + iy * gz + iz

fun dist2 (x1, y1, z1) (x2, y2, z2) =
  let val dx = x1-x2 val dy = y1-y2 val dz = z1-z2 in dx*dx + dy*dy + dz*dz end

fun find_closest_point (grid_size, points, cells) bbox_min unit_size query =
  let
    val (ix, iy, iz) = get_cell_index (grid_size, bbox_min, unit_size) query
    val (gx, gy, gz) = case grid_size of [a, b, c] => (a, b, c) | _ => raise Fail "bad grid size"
    val cell_idx = flatten_index (gx, gy, gz) (ix, iy, iz)
    val indices = List.nth(cells, cell_idx)
    val candidates = List.map (fn i => List.nth(points, i)) indices
    val (best, best_dist) =
      List.foldl (fn (pt, (best, best_dist)) =>
        let val d = dist2 pt query
        in if d < best_dist then (pt, d) else (best, best_dist) end)
        ((0.0,0.0,0.0), Real.posInf) candidates
  in
    best
  end

fun parse_query_point s =
  case parse_floats s of
    [x, y, z] => (x, y, z)
  | _ => raise Fail "Expected 3 floats for query point"

fun main () =
  let
    val (grid_size, bbox_min, unit_size, points, cells) = read_spatial_index "spatial_index.txt"
    val query_line = valOf (TextIO.inputLine TextIO.stdIn)
    val query = parse_query_point query_line
    val (x, y, z) = find_closest_point (grid_size, points, cells) bbox_min unit_size query
    val _ = print (String.translate (fn #"~" => "-" | c => str c) (Real.toString x) ^ " " ^
                   String.translate (fn #"~" => "-" | c => str c) (Real.toString y) ^ " " ^
                   String.translate (fn #"~" => "-" | c => str c) (Real.toString z) ^ "\n")
  in
    ()
  end

val _ = main ()
