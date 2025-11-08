open! Core
open Hw2_patchwork_logic
open! Bonsai_web
open! Bonsai.Let_syntax
open Js_of_ocaml

let dim_ineligible_patches (pos : int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "dimIneligiblePatches" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then
    ignore (fun_call fn [| inject pos |])
      else ()

let pieces = Game_pieces.setup_game "Player" "AI" "Red" "Blue"

let game_component =
  let initial_state = Game_state.initialize_state pieces in
  let%sub game_state, _set_game_state =
    Bonsai.state (module Game_state) ~default_model:initial_state
  in
  let%arr game_state = game_state in
  let neutral_position = game_state.neut.pos in
  dim_ineligible_patches neutral_position;
  Vdom.Node.none

let () = Start.start ~bind_to_element_with_id:"patchwork_game" game_component
