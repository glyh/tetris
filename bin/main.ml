open Minttea
open Libtetris.Shapes

exception Unreachable

type position = int * int (* row, col *)

type moving_shape = {
  position : int * int;
  shape : shape;
  color : Spices.color;
}

type map = Spices.color array BatRefList.t
type game = { map : map; current : moving_shape; score : int; dead : bool }

let game_rows, game_cols = (20, 20)
let white = Spices.color "#ffffff"

let generate_new_shape () =
  let deg =
    match degree_of_enum (Random.int 4) with
    | None -> raise Unreachable
    | Some deg -> deg
  in
  let shape_id = Random.int (Array.length shapes) in
  let shape = clockwise_rotate shapes.(shape_id) deg in
  let height, width = get_dimension shape in
  let position = (-height, (game_cols - width) / 2) in
  let color = colors.(shape_id) in
  { shape; position; color }

let create_model () =
  {
    map =
      Array.make_matrix game_rows game_cols white
      |> Array.to_list |> BatRefList.of_list;
    current = generate_new_shape ();
    score = 0;
    dead = false;
  }

let timer_ref = Riot.Ref.make ()
let delta = 0.5

let init _ =
  Command.Seq [ Set_timer (timer_ref, delta); Command.Enter_alt_screen ]

let fall (s : moving_shape) =
  let row, col = s.position in
  { s with position = (row + 1, col) }

type hit_dir = Down | Right | Left | Origin

let apply_direction (d : hit_dir) (p : position) =
  let row, col = p in
  let del_y, del_x =
    match d with
    | Down -> (1, 0)
    | Left -> (0, -1)
    | Right -> (0, 1)
    | Origin -> (0, 0)
  in
  (row + del_y, col + del_x)

let check_hit (s : moving_shape) (map : map) (direction : hit_dir) : bool =
  let row_new, col_new = apply_direction direction s.position in
  let height, width = get_dimension s.shape in
  let result = ref false in
  for i = max row_new 0 to row_new + height - 1 do
    for j = col_new to col_new + width - 1 do
      result :=
        !result || i >= game_rows || j < 0 || j >= game_cols
        || (BatRefList.Index.at_index map i).(j) != white
           && s.shape.(i - row_new).(j - col_new)
    done
  done;
  !result

let row_full = Array.for_all (fun ele -> ele != white)

let try_eliminate map =
  BatRefList.filter (fun row -> not (row_full row)) map;
  let remained_length = BatRefList.length map in
  let score = game_rows - remained_length in
  for _ = 1 to score do
    BatRefList.push map (Array.init game_cols (fun _ -> white))
  done;
  (map, score)

let implement_hit model =
  let height, width = get_dimension model.current.shape in
  let r, c = model.current.position in
  if r < 0 then { model with dead = true }
  else (
    for i = r to r + height - 1 do
      for j = c to c + width - 1 do
        let row_in_shape = i - r in
        let col_in_shape = j - c in
        if model.current.shape.(row_in_shape).(col_in_shape) then
          let row = BatRefList.Index.at_index model.map i in
          row.(j) <- model.current.color
      done
    done;
    let map, delta = try_eliminate model.map in
    {
      model with
      current = generate_new_shape ();
      map;
      score = model.score + delta;
    })

let tick model =
  match check_hit model.current model.map Down with
  | false -> { model with current = fall model.current }
  | true -> implement_hit model

let update event model =
  if model.dead then
    match event with
    | Event.KeyDown Escape -> (model, Command.Quit)
    | Event.KeyDown (Key "r") -> (create_model (), Command.Noop)
    | _ -> (model, Command.Noop)
  else
    match event with
    | Event.KeyDown Escape -> (model, Command.Quit)
    | Event.Timer _ -> (tick model, Command.Set_timer (timer_ref, delta))
    | Event.KeyDown (Key (("q" | "e") as k)) ->
        let deg = if k == "q" then Degree270 else Degree90 in
        let rotated_shape = clockwise_rotate model.current.shape deg in
        let rotated = { model.current with shape = rotated_shape } in
        let new_model =
          if check_hit rotated model.map Origin then model
          else { model with current = rotated }
        in
        (new_model, Command.Noop)
    | Event.KeyDown (Key (("a" | "d" | "s") as k)) ->
        let shift_dir = match k with "a" -> Left | "d" -> Right | _ -> Down in
        if check_hit model.current model.map shift_dir then (model, Command.Noop)
        else
          let current =
            {
              model.current with
              position = apply_direction shift_dir model.current.position;
            }
          in
          ({ model with current }, Command.Noop)
    | _ -> (model, Command.Noop)

let full = "██"
let empty = "  "
let border = Spices.(default |> fg white |> build) "%s" full

let render_map (cur : moving_shape) (map : map) =
  let row, col = cur.position in
  let height, width = get_dimension cur.shape in
  let output = ref "" in
  for i = 0 to game_rows - 1 do
    output := !output ^ border;
    for j = 0 to game_cols - 1 do
      let to_print =
        if
          row <= i
          && i < row + height
          && col <= j
          && j < col + width
          && cur.shape.(i - row).(j - col)
        then Spices.(default |> fg cur.color |> build) "%s" full
        else
          let row = BatRefList.Index.at_index map i in
          let color_at_grid = row.(j) in
          if color_at_grid = white then empty
          else Spices.(default |> fg color_at_grid |> build) "%s" full
      in
      output := !output ^ to_print
    done;
    output := !output ^ border ^ "\n"
  done;
  !output

let view model =
  Format.sprintf {|Score: %d
%s%s%s%s
|} model.score
    (BatString.repeat border (game_cols + 2) ^ "\n")
    (render_map model.current model.map)
    (BatString.repeat border (game_cols + 2) ^ "\n")
    (if model.dead then "Press R to restart" else "")

let () =
  Minttea.app ~init ~update ~view ()
  |> Minttea.start ~initial_model:(create_model ())
