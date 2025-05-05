(* Benchmark driver for parallel closest point search: parallelize across queries for real speedup *)
open Parallel

(* Utility functions: copy from closest-point-spatial.sml *)
fun split sep s = String.tokens (fn c => c = sep) s
fun parse_floats s =
  let fun get (SOME x) = x | get NONE = raise Fail "parse_floats"
  in List.map get (List.map Real.fromString (String.tokens (fn c => c = #" ") s)) end
fun parse_ints s =
  let fun get (SOME x) = x | get NONE = raise Fail "parse_ints"
  in List.map get (List.map Int.fromString (String.tokens (fn c => c = #" ") s)) end

fun read_spatial_index filename =
  let
    val ins = TextIO.openIn filename
    val grid_size = parse_ints (valOf (TextIO.inputLine ins))
    val bbox_min = let val [x, y, z] = parse_floats (valOf (TextIO.inputLine ins)) in (x, y, z) end
    val unit_size = let val [u] = parse_floats (valOf (TextIO.inputLine ins)) in u end
    val num_points = hd (parse_ints (valOf (TextIO.inputLine ins)))
    val points =
      let
        fun read_points 0 acc = List.rev acc
          | read_points n acc =
              case TextIO.inputLine ins of
                NONE => raise Fail "Unexpected EOF"
              | SOME line =>
                  let val [x, y, z] = parse_floats line
                  in read_points (n-1) ((x, y, z)::acc) end
      in read_points num_points [] end
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
              in read_cells (n-1) (indices::acc) end
    val num_cells = gx * gy * gz
    val cells = read_cells num_cells []
    val _ = TextIO.closeIn ins
  in
    (grid_size, bbox_min, unit_size, points, cells)
  end

fun get_cell_index (grid_size as [gx, gy, gz], (xmin, ymin, zmin), unit) (x, y, z) =
  let
    val ix = let val i = Real.floor ((x - xmin) / unit) in if i < 0 then 0 else if i > gx-1 then gx-1 else i end
    val iy = let val i = Real.floor ((y - ymin) / unit) in if i < 0 then 0 else if i > gy-1 then gy-1 else i end
    val iz = let val i = Real.floor ((z - zmin) / unit) in if i < 0 then 0 else if i > gz-1 then gz-1 else i end
  in (ix, iy, iz) end

fun flatten_index (gx, gy, gz) (ix, iy, iz) = ix * gy * gz + iy * gz + iz

fun dist2 (x1, y1, z1) (x2, y2, z2) =
  let val dx = x1-x2 val dy = y1-y2 val dz = z1-z2 in dx*dx + dy*dy + dz*dz end

fun block_cells (gx, gy, gz) (ix, iy, iz) radius =
  let
    val xs = List.tabulate (2*radius+1, fn d => ix - radius + d)
    val ys = List.tabulate (2*radius+1, fn d => iy - radius + d)
    val zs = List.tabulate (2*radius+1, fn d => iz - radius + d)
    fun in_bounds x max = x >= 0 andalso x < max
    val coords = List.concat (List.map (fn x => List.concat (List.map (fn y => List.map (fn z => (x,y,z)) zs) ys)) xs)
  in
    List.filter (fn (x,y,z) => in_bounds x gx andalso in_bounds y gy andalso in_bounds z gz) coords
  end

(* Helper to convert a Seq to a list *)
fun seq_to_list s =
  let fun go i acc = if i >= Seq.length s then List.rev acc else go (i+1) (Seq.nth s i :: acc)
  in go 0 [] end

(* Serial search for a single query *)
fun find_closest_point_serial (grid_size, points_arr, cells_arr) bbox_min unit_size query =
  let
    val (ix, iy, iz) = get_cell_index (grid_size, bbox_min, unit_size) query
    val (gx, gy, gz) = case grid_size of [a, b, c] => (a, b, c) | _ => raise Fail "bad grid size"
    val block_radius = 1
    val cell_indices = block_cells (gx, gy, gz) (ix, iy, iz) block_radius
    val all_candidate_indices =
      List.concat (List.map (fn (cx, cy, cz) =>
        let val cell_idx = flatten_index (gx, gy, gz) (cx, cy, cz)
        in Array.sub(cells_arr, cell_idx) end
      ) cell_indices)
    val best =
      List.foldl (fn (i, (best_pt, best_dist)) =>
        let
          val pt = Array.sub(points_arr, i)
          val d = dist2 pt query
        in
          if d < best_dist then (pt, d) else (best_pt, best_dist)
        end) ((0.0,0.0,0.0), Real.posInf) all_candidate_indices
  in
    #1 best
  end

(* Parallel batch search: split queries into n chunks, process each chunk serially, run chunks in parallel *)
fun batch_find_closest_points_parallel (grid_size, points_arr, cells_arr) bbox_min unit_size queries ncores =
  let
    val n = List.length queries
    val arr = Array.fromList queries
    fun chunk_start i = (i * n) div ncores
    fun chunk_end i = (((i+1) * n) div ncores) - 1
    fun process_chunk i =
      let
        val s = chunk_start i
        val e = chunk_end i
        fun loop j acc = if j > e then List.rev acc
                         else loop (j+1) (find_closest_point_serial (grid_size, points_arr, cells_arr) bbox_min unit_size (Array.sub(arr, j)) :: acc)
      in
        loop s []
      end
    val results_chunks = Parallel.tabulate (0, ncores) process_chunk
  in
    List.concat (seq_to_list results_chunks)
  end

(* Main benchmark driver *)
fun main () =
  let
    val (grid_size, bbox_min, unit_size, points, cells) = read_spatial_index "spatial_index.txt"
    val bbox_max_pt = let
      fun max3 ((x1, y1, z1), (x2, y2, z2)) = (Real.max (x1, x2), Real.max (y1, y2), Real.max (z1, z2))
    in
      case points of [] => raise Fail "No points" | h::t => List.foldl max3 h t
    end
    val num_queries = CommandLineArgs.parseInt "queries" 5000
    val num_repeat = CommandLineArgs.parseInt "repeat" 4
    val num_procs = CommandLineArgs.parseInt "procs" 16
    val core_counts = List.tabulate (num_procs, fn i => i + 1)
    val seed = ref 42
    fun rand_real (a, b) =
      let
        val s = !seed
        val next = (s * 1103515245 + 12345) mod 2147483648
        val _ = seed := next
        val r = Real.fromInt (next mod 1000000) / 1000000.0
      in
        a + r * (b - a)
      end

    (* Define random_query *)
    fun random_query ((xmin, ymin, zmin), (xmax, ymax, zmax)) =
      (rand_real (xmin, xmax), rand_real (ymin, ymax), rand_real (zmin, zmax))

    val queries = List.tabulate (num_queries, fn _ => random_query (bbox_min, bbox_max_pt))
    val points_arr = Array.fromList points
    val cells_arr = Array.fromList cells
    fun timeit f =
      let val t0 = Time.now ()
          val _ = f ()
          val t1 = Time.now ()
      in Time.toReal (Time.- (t1, t0)) end
    fun run_for_cores n =
      let
        fun repeat k acc =
          if k = 0 then List.rev acc
          else repeat (k-1) (timeit (fn () => ignore (batch_find_closest_points_parallel (grid_size, points_arr, cells_arr) bbox_min unit_size queries n)) :: acc)
        val times = repeat num_repeat []
        val avg = List.foldl op+ 0.0 times / Real.fromInt (List.length times)
        val times_str = String.concatWith ", " (List.map (fn t => Real.fmt (StringCvt.FIX (SOME 4)) t) times)
      in
        print ("Cores: " ^ Int.toString n ^ " Avg: " ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s\n")
      end
    val _ = List.app run_for_cores core_counts
  in
    ()
  end

val _ = main ()
