let blue = Spices.color "#0341AE"
let green = Spices.color "#72CB3B"
let yellow = Spices.color "#FFD500"
let orange = Spices.color "#FF971C"
let red = Spices.color "#FF3213"
let purple = Spices.color "#800080"
let cyan = Spices.color "#00B2FF"
let colors = [| cyan; blue; orange; yellow; green; purple; red |]

type shape = bool array array

let matrix_of_str (s : string) : shape =
  let mat =
    BatString.split_on_char '\n' s
    |> List.map (fun s ->
           String.to_seq s
           |> Seq.map (( == ) '+')
           |> List.of_seq |> BatDeque.of_list)
    |> BatDeque.of_list
  in
  let rec unpad_heading_rows mat =
    match BatDeque.front mat with
    | Some (row, rest) ->
        let num_nonempty =
          BatDeque.fold_left (fun acc ele -> if ele then acc + 1 else acc) 0 row
        in
        if num_nonempty == 0 then unpad_heading_rows rest else mat
    | None -> mat
  in
  let rec unpad_backing_rows mat =
    match BatDeque.rear mat with
    | Some (rest, row) ->
        let num_nonempty =
          BatDeque.fold_left (fun acc ele -> if ele then acc + 1 else acc) 0 row
        in
        if num_nonempty == 0 then unpad_backing_rows rest else mat
    | None -> mat
  in
  let mat = mat |> unpad_heading_rows |> unpad_backing_rows in
  let width =
    BatDeque.fold_left (fun acc row -> max acc (BatDeque.size row)) 0 mat
  in
  mat
  |> BatDeque.map (fun q ->
         let cur_width = BatDeque.size q in
         let q =
           BatDeque.append_list q
             (List.init (width - cur_width) (fun _ -> false))
         in
         BatDeque.to_list q |> Array.of_list)
  |> BatDeque.to_list |> Array.of_list

[@@@ocamlformat "disable"]
let shapes: shape array = [ 
(* I shape *) {|
+
+
+
+
|};
(* J shape *)  {|
 +
 +
++
|}; 
(* L shape *) {|
+
+
++
|};
(* O shape *) {|
++
++
|};
(* S shape *) {|
 ++
++
|};
(* T shape *) {|
+++
 +
|};
(* Z shape *) {|
++
 ++
|};

] |> List.to_seq |> Seq.map matrix_of_str |> Array.of_seq
[@@@ocamlformat "enable"]

type degree = Degree0 | Degree90 | Degree180 | Degree270 [@@deriving enum]

let get_dimension (s : shape) =
  let height = Array.length s in
  let width = if height > 0 then Array.length s.(0) else 0 in
  (height, width)

let clockwise_rotate (shape : shape) = function
  | Degree0 -> shape
  | Degree180 ->
      let height, width = get_dimension shape in
      let ret = Array.make_matrix height width false in
      for i = 0 to height - 1 do
        for j = 0 to width - 1 do
          ret.(i).(j) <- shape.(height - 1 - i).(width - 1 - j)
        done
      done;
      ret
  | Degree90 ->
      let height, width = get_dimension shape in
      let ret = Array.make_matrix width height false in
      for i = 0 to width - 1 do
        for j = 0 to height - 1 do
          ret.(i).(j) <- shape.(height - 1 - j).(i)
        done
      done;
      ret
  | Degree270 ->
      let height, width = get_dimension shape in
      let ret = Array.make_matrix width height false in
      for i = 0 to width - 1 do
        for j = 0 to height - 1 do
          ret.(i).(j) <- shape.(j).(width - i - 1)
        done
      done;
      ret
