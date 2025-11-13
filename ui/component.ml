open! Core
open Hw2_patchwork_logic
open! Bonsai_web
open! Bonsai.Let_syntax
open Js_of_ocaml

let log s = Firebug.console##log (Js.string s)

let dim_ineligible_patches (pos : int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "dimIneligiblePatches" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [| inject pos |]) else ()
;;

let reposition_time_tokens (p1 : int) (p2 : int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "repositionTimeTokens" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then ignore (fun_call fn [| inject p1; inject p2 |])
  else ()
;;

let _move_time_token (pnum : int) (pos : int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "positionToken" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then ignore (fun_call fn [| inject pnum; inject pos |])
  else ()
;;

let move_neut_token (pos : int) (init : bool) =
  let open Js.Unsafe in
  let g = global in
  let fn = if init then get g "moveNeutralTokenInitial" else get g "moveNeutralToken" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [| inject pos |]) else ()
;;

let update_patch_rotation (pl : Patch.t list) (pnum : int) (rot : int) =
  let rec walk l acc =
    match l with
    | [] -> List.rev acc
    | (hd : Patch.t) :: tl ->
      if hd.pos_around_board = pnum
      then walk tl ({ hd with rotated = rot } :: acc)
      else walk tl (hd :: acc)
  in
  walk pl []
;;

let initialize_patches (pl : Patch.t list) (np : int) =
  let rec process_patch
            (pl : Patch.t list)
            (pcol : int list)
            (pc : int list)
            (pt : int list)
            (pi : int list)
            pd
    =
    match pl with
    | [] -> List.rev pcol, List.rev pc, List.rev pt, List.rev pi, List.rev pd
    | (hd : Patch.t) :: tl ->
      let col, _row = Patch.get_col_row hd.shape in
      process_patch
        tl
        (col :: pcol)
        (hd.cost :: pc)
        (hd.move_num :: pt)
        (hd.income :: pi)
        (Patch.get_patch_dim hd.shape :: pd)
  in
  let patch_cols, patch_costs, patch_times, patch_incomes, patch_dims_ocaml =
    process_patch pl [] [] [] [] []
  in
  let rec flatten_list (pl : (int * string) list) acc =
    match pl with
    | [] -> List.rev acc
    | hd :: tl ->
      flatten_list tl (Js.Unsafe.inject (snd hd) :: Js.Unsafe.inject (fst hd) :: acc)
  in
  let rec flatten_patch_dim_js (dims_list : (int * string) list list) acc =
    match dims_list with
    | [] -> acc
    | hd :: tl ->
      let flattened = flatten_list hd [] in
      let flat_js = Js.array (Array.of_list flattened) in
      flatten_patch_dim_js tl (acc @ [ flat_js ])
  in
  let patch_dims_js =
    flatten_patch_dim_js patch_dims_ocaml [] |> Array.of_list |> Js.array
  in
  let patch_cols_js = Js.array (Array.of_list patch_cols) in
  let patch_costs_js = Js.array (Array.of_list patch_costs) in
  let patch_times_js = Js.array (Array.of_list patch_times) in
  let patch_incomes_js = Js.array (Array.of_list patch_incomes) in
  let open Js.Unsafe in
  let g = global in
  let fn = get g "buildInitialPatches" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then
    ignore
      (fun_call
         fn
         [| inject patch_dims_js
          ; inject patch_cols_js
          ; inject patch_costs_js
          ; inject patch_times_js
          ; inject patch_incomes_js
          ; inject np
         |])
  else ()
;;

let set_button_count (p : int) (count : int) =
  let id = if p = 1 then "p1-buttons" else "p2-buttons" in
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some span ->
    span##.textContent := Js.Opt.return (Js.string ("x " ^ string_of_int count))
;;

let pieces =
  let p1_span = Dom_html.getElementById_exn "player-box-1" in
  let player_name = Js.Opt.case p1_span##.textContent (fun () -> "") Js.to_string in
  Game_pieces.setup_game player_name "AI" "Red" "Blue"
;;

let status_message_component ~message ~bg_color =
  let%sub msg, set_msg = Bonsai.state (module String) ~default_model:message in
  let%sub bg, set_bg = Bonsai.state (module String) ~default_model:bg_color in
  let%arr msg = msg
  and set_msg = set_msg
  and bg = bg
  and set_bg = set_bg in
  let attrs =
    let base =
      [ Vdom.Attr.id "status-message"
      ; Vdom.Attr.class_ "status-message"
      ; Vdom.Attr.style (Css_gen.background_color (`Hex bg))
      ]
    in
    if String.is_empty msg
    then base
    else
      Vdom.Attr.class_ "show" :: Vdom.Attr.on_animationend (fun _ev -> set_msg "") :: base
  in
  Vdom.Node.div ~attrs [ Vdom.Node.text msg ], set_msg, set_bg
;;

let place_patch_component ~game_state ~set_game_state ~set_status_msg ~set_status_bg =
  let%sub () =
    Bonsai.Edge.on_change
      (module Game_state)
      game_state
      ~callback:
        (let%map set_game_state = set_game_state
         and set_status_msg = set_status_msg
         and set_status_bg = set_status_bg in
         fun game_state ->
           Bonsai.Effect.of_sync_fun
             (fun () ->
                let rec install (state : Hw2_patchwork_logic.Game_state.t) =
                  Js_of_ocaml.Js.Unsafe.set
                    Js_of_ocaml.Js.Unsafe.global
                    "updatePatchRotation"
                    (Js_of_ocaml.Js.wrap_callback (fun (pnum : float) (rot : float) ->
                       let patch_pos = int_of_float pnum in
                       let num_rots = int_of_float rot in
                       log "update patch rotation called";
                       let upd_patch_list =
                         update_patch_rotation state.patches patch_pos num_rots
                       in
                       let upd_st = { state with patches = upd_patch_list } in
                       let effect = set_game_state upd_st in
                       Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect;
                       install upd_st));
                  Js_of_ocaml.Js.Unsafe.set
                    Js_of_ocaml.Js.Unsafe.global
                    "bonsaiPlacePatch"
                    (Js_of_ocaml.Js.wrap_callback
                       (fun (r : float) (c : float) (pc : float) (time : float) ->
                          try
                            let row = int_of_float r in
                            let col = int_of_float c in
                            let patch_choice = int_of_float pc in
                            let advance_spaces = int_of_float time in
                            let gs : Hw2_patchwork_logic.Game_state.t = state in
                            let player = gs.turn in
                            log
                              ("BonsaiPlacePatch: player "
                               ^ string_of_int player.player_num);
                            try
                              let upd_state =
                                Move.choose_move gs PlacePatch patch_choice row col
                              in
                              let effect1 = set_game_state upd_state in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                              let status_advance_str =
                                "Advanced " ^ string_of_int advance_spaces
                              in
                              let effect2 = set_status_msg status_advance_str in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                              let effect3 = set_status_bg "#5cb85c" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect3;
                              install upd_state;
                              Js_of_ocaml.Js._true
                            with
                            | Button.Insufficient_funds ->
                              let effect1 = set_status_msg "Not Enough Buttons" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                              let effect2 = set_status_bg "#FF2C2C" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                              install state;
                              Js_of_ocaml.Js._false
                            | Game_board.Out_of_bounds ->
                              let effect1 = set_status_msg "Out of Bounds" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                              let effect2 = set_status_bg "#FF2C2C" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                              install state;
                              Js_of_ocaml.Js._false
                            | Game_board.Patch_does_not_fit_there ->
                              let effect1 = set_status_msg "Patch Does Not Fit There" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                              let effect2 = set_status_bg "#FF2C2C" in
                              Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                              install state;
                              Js_of_ocaml.Js._false
                          with
                          | exn ->
                            log ("OCaml: unexpected exn: " ^ Exn.to_string exn);
                            Js_of_ocaml.Js._false))
                in
                install game_state)
             ())
  in
  let%arr () = Bonsai.Value.return () in
  Vdom.Node.none
;;

let advance_component ~game_state ~set_game_state ~set_status_msg ~set_status_bg =
  let%sub _position_on_mb, _set_position_on_mb =
    Bonsai.state (module Int) ~default_model:1
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module Game_state)
      game_state
      ~callback:
        (let%map set_game_state = set_game_state
         and set_status_msg = set_status_msg
         and set_status_bg = set_status_bg in
         fun game_state ->
           Bonsai.Effect.of_sync_fun
             (fun () ->
                Js_of_ocaml.Js.Unsafe.set
                  Js_of_ocaml.Js.Unsafe.global
                  "bonsaiCheckTokenPosition"
                  (Js_of_ocaml.Js.wrap_callback (fun (i : float) ->
                     let n = int_of_float i in
                     let gs : Hw2_patchwork_logic.Game_state.t = game_state in
                     if n <> gs.tk2.position + 1
                     then (
                       let effect1 = set_status_msg "Invalid Move" in
                       Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                       let effect2 = set_status_bg "#FF2C2C" in
                       Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                       reposition_time_tokens
                         game_state.tk1.position
                         game_state.tk2.position)
                     else (
                       let upd_state = Move.choose_move game_state Advance 0 0 0 in
                       let effect1 = set_game_state upd_state in
                       Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                       let effect2 = set_status_msg "Advanced!" in
                       Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                       let effect3 = set_status_bg "#5cb85c" in
                       Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect3))))
             ())
  in
  let%arr () = Bonsai.Value.return () in
  Vdom.Node.none
;;

let game_component =
  let initial_state = Hw2_patchwork_logic.Game_state.initialize_state pieces in
  let%sub game_state, set_game_state =
    Bonsai.state (module Hw2_patchwork_logic.Game_state) ~default_model:initial_state
  in
  let%sub status, set_status_msg, set_status_bg =
    status_message_component ~message:"" ~bg_color:"#ffffff"
  in
  let%sub _place_patch =
    place_patch_component ~game_state ~set_game_state ~set_status_msg ~set_status_bg
  in
  let%sub _advance =
    advance_component ~game_state ~set_game_state ~set_status_msg ~set_status_bg
  in
  let%sub seen_first, set_seen_first = Bonsai.state (module Bool) ~default_model:false in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map (game_state : Hw2_patchwork_logic.Game_state.t) = game_state in
         Bonsai.Effect.of_sync_fun
           (fun () ->
              let open Js.Unsafe in
              let g = global in
              (try
                 let fn = get g "onBonsaiReady" in
                 ignore (fun_call fn [||])
               with
               | _ -> ());
              set
                g
                "queryPlayerTurn"
                (Js_of_ocaml.Js.wrap_callback (fun () ->
                   Js_of_ocaml.Js.number_of_float
                     (float_of_int game_state.turn.player_num)));
              initialize_patches game_state.patches game_state.neut.pos;
              move_neut_token game_state.neut.pos true)
           ())
      ()
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module Hw2_patchwork_logic.Game_state)
      game_state
      ~callback:
        (let%map () = Bonsai.Value.return ()
         and set_seen_first = set_seen_first
         and seen_first = seen_first in
         fun (gs : Hw2_patchwork_logic.Game_state.t) ->
           let js_effect =
             Bonsai.Effect.of_sync_fun
               (fun () ->
                  Js_of_ocaml.Js.Unsafe.set
                    Js_of_ocaml.Js.Unsafe.global
                    "queryPlayerTurn"
                    (Js_of_ocaml.Js.wrap_callback (fun () ->
                       Js_of_ocaml.Js.number_of_float (float_of_int gs.turn.player_num)));
                  reposition_time_tokens gs.tk1.position gs.tk2.position;
                  set_button_count 1 gs.tk1.owned_by.buttons_owned;
                  set_button_count 2 gs.tk2.owned_by.buttons_owned;
                  if seen_first then move_neut_token gs.neut.pos false;
                  let dim_slot =
                    if seen_first
                    then gs.neut.pos
                    else if gs.neut.pos = 1
                    then 33
                    else gs.neut.pos - 1
                  in
                  dim_ineligible_patches dim_slot)
               ()
           in
           if not seen_first
           then Bonsai.Effect.Many [ set_seen_first true; js_effect ]
           else js_effect)
  in
  let%arr status = status in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.id "patchwork_game" ]
    [ (* [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "patch-info"; Vdom.Attr.id "patch-info"
         ] [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "patch-attrs" ] [ Vdom.Node.div
         ~attrs:[ Vdom.Attr.class_ "patch-label" ] [ Vdom.Node.text "Cost: " ] ;
         Vdom.Node.div ~attrs:[ Vdom.Attr.id "patch-cost"; Vdom.Attr.class_ "patch-value"
         ] [ Vdom.Node.text "5" ] ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "patch-label"
         ] [ Vdom.Node.text "Time: " ] ; Vdom.Node.div ~attrs:[ Vdom.Attr.id "patch-time";
         Vdom.Attr.class_ "patch-value" ] [ Vdom.Node.text "2" ] ] ] *)
      status
    ]
;;

let () = Start.start ~bind_to_element_with_id:"patchwork_game" game_component
