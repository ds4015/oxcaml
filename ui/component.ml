open! Core
open Hw2_patchwork_logic
open! Bonsai_web
open! Bonsai.Let_syntax
open Js_of_ocaml

let log s = Firebug.console##log (Js.string s)

(* HOOKS into three.js *)

(* initialize multiplayer mode *)
let init_mp (matchID : string) (role : int) =
  log "bonsai: init_mp called";
  let open Js.Unsafe in
  let g = global in
  let fn = get g "initializeMultiplayerMode" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then ignore (fun_call fn [| inject matchID; inject role |])
  else ()
;;

(* dim/wireframe all but active 3 patch choices *)

let dim_ineligible_patches (p1 : int) (p2 : int) (p3 : int) =
  log "dimming patches";
  let open Js.Unsafe in
  let g = global in
  let fn = get g "dimIneligiblePatches" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then ignore (fun_call fn [| inject p1; inject p2; inject p3 |])
  else ()
;;

let clear_opp_patches () =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "clearOpponentBoardPatches" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [||]) else ()
;;

let get_patch_choices (rem_list : int list) (neut_pos : int) (_new_game : bool) =
  let patch1 = ref (-1) in
  let patch2 = ref (-1) in
  let patch3 = ref (-1) in
  let found_pos = ref false in
  let rec start_over rl =
    match rl with
    | [] ->
      if !patch1 = -1 || !patch2 = -1 || !patch3 = -1
      then failwith "No more patches"
      else ()
    | hd :: tl ->
      if !patch1 = -1
      then (
        patch1 := hd;
        start_over tl)
      else if !patch2 = -1
      then (
        patch2 := hd;
        start_over tl)
      else if !patch3 = -1
      then patch3 := hd
      else start_over tl
  in
  let rec walk l =
    match l with
    | [] ->
      if !patch1 = -1 || !patch2 = -1 || !patch3 = -1 then start_over rem_list else ()
    | hd :: tl ->
      if not !found_pos
      then
        if hd <= neut_pos
        then walk tl
        else (
          patch1 := hd;
          found_pos := true;
          walk tl)
      else if !patch2 = -1
      then (
        patch2 := hd;
        walk tl)
      else if !patch3 = -1
      then patch3 := hd
      else ();
      walk tl
  in
  walk rem_list;
  log
    ("Patch choices: "
     ^ string_of_int !patch1
     ^ ", "
     ^ string_of_int !patch2
     ^ ", "
     ^ string_of_int !patch3);
  dim_ineligible_patches !patch1 !patch2 !patch3
;;

(* animate button income from advancing *)
let play_button_flip_animation (num : int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "playButtonFlipAnimation" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [| inject num |]) else ()
;;

let mp_save_state ?(_json = "") (h : int) (j : int) =
  log "mp_save_state called from bonsai";
  let open Js.Unsafe in
  let g = global in
  let fn = get g "saveMPState" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then (
    let json_inject =
      if String.equal _json ""
      then Js.Unsafe.inject Js.null
      else Js.Unsafe.inject (Js.string _json)
    in
    ignore (fun_call fn [| inject json_inject; inject h; inject j |]))
  else ()
;;

let mark_initial_state_as_saved () =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "markInitialStateAsSaved" in
  let ty = Js.to_string (Js.typeof fn) in
  log "state saved. calling markInitialStateAsSaved";
  if String.equal ty "function" then ignore (fun_call fn [||]) else ()
;;

(* button income when passing button slot on board - for animation *)
let determine_button_income player old_pos new_pos board_inc isAi =
  log "Determine button income called.";
  let board_button_slots = [ 6; 12; 18; 24; 30; 36; 42; 48; 54 ] in
  let rec check_bbs bbs acc =
    match bbs with
    | [] -> acc
    | hd :: tl ->
      if old_pos < hd && new_pos >= hd
      then check_bbs tl (acc + board_inc)
      else check_bbs tl acc
  in
  let total_new_income = check_bbs board_button_slots 0 in
  let pnum = if isAi then 0 else player in
  log ("Total button income: " ^ string_of_int total_new_income);
  let open Js.Unsafe in
  let g = global in
  let fn = get g "checkAndSetButtonIncome" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then ignore (fun_call fn [| inject pnum; inject total_new_income; inject new_pos |])
;;

(* single player ai turn marker to lock UI controls *)
let ai_start_turn () =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "beginAIMove" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [||]) else ()
;;

(* ai turn end to unlock UI controls *)
let ai_end_turn () =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "endAIMove" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [||]) else ()
;;

(* refresh time token positions on main board *)
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

(* position neutral token on main board *)
let move_neut_token (pos : int) (init : bool) =
  let open Js.Unsafe in
  let g = global in
  let fn = if init then get g "moveNeutralTokenInitial" else get g "moveNeutralToken" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then ignore (fun_call fn [| inject pos |]) else ()
;;

(* position patch in 3d coords on AI/P2 quilt board *)
let place_ai_patch_on_qb
      (pl : int)
      (p : int)
      (r : int)
      (c : int)
      (rot : int)
      (isAI : bool)
  =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "placeAIPatch" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function"
  then
    ignore
      (fun_call fn [| inject pl; inject p; inject r; inject c; inject rot; inject isAI |])
  else ()
;;

(* set rotation count for patch on right click *)
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

(* timeout for AI turn for more naturalistic feel *)
let delay (ms : int) (f : unit -> unit) =
  ignore
    (Js_of_ocaml.Dom_html.window##setTimeout
       (Js_of_ocaml.Js.wrap_callback f)
       (float_of_int ms))
;;

(* position 3D patches around main board - run once per match *)
let initialize_patches (pl : Patch.t list) (np : int) =
  log "initializing patches";
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

(* update DOM with current button counts for CSS2DRenderer *)
let set_button_count (p : int) (count : int) =
  let id = if p = 1 then "p1-buttons" else "p2-buttons" in
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some span ->
    span##.textContent := Js.Opt.return (Js.string ("x " ^ string_of_int count))
;;

(* update turn status box at bottom of screen with player names *)
let set_player_turn (p : Player.t) (ai : bool) =
  let id = "player-turn" in
  let name = if p.player_num = 2 && ai then "AI" else p.player_name in
  let divText = name ^ "'s Turn" in
  match Dom_html.getElementById_opt id with
  | None -> ()
  | Some div -> div##.textContent := Js.Opt.return (Js.string divText)
;;

(* UI COMPONENTS *)

(* set up the game *)

let pieces = Game_pieces.setup_game "Player 1" "Player 2" "Red" "Blue"

(* state for status message in center of screen *)
let status_message_component ~message =
  let%sub msg, set_msg = Bonsai.state (module String) ~default_model:message in
  let%arr msg = msg
  and set_msg = set_msg in
  let attrs =
    let base = [ Vdom.Attr.id "status-message"; Vdom.Attr.class_ "status-message" ] in
    if String.is_empty msg
    then base
    else
      Vdom.Attr.class_ "show" :: Vdom.Attr.on_animationend (fun _ev -> set_msg "") :: base
  in
  Vdom.Node.div ~attrs [ Vdom.Node.text msg ], set_msg
;;

(* state for winner message on game over *)
let winner_component ~game_state =
  let%sub msg, set_msg = Bonsai.state (module String) ~default_model:"" in
  let%sub () =
    Bonsai.Edge.on_change
      (module Hw2_patchwork_logic.Game_state)
      game_state
      ~callback:
        (let%map set_msg = set_msg in
         fun (gs : Hw2_patchwork_logic.Game_state.t) ->
           if gs.tk1.position = 54 && gs.tk2.position = 54
           then (
             let scores = Move.score_game gs in
             let p1_score = fst scores in
             let p2_score = snd scores in
             let p1name = gs.tk1.owned_by.player_name in
             let p2name = gs.tk2.owned_by.player_name in
             let message =
               if p1_score > p2_score
               then Printf.sprintf "%s wins! %d to %d" p1name p1_score p2_score
               else if p2_score > p1_score
               then Printf.sprintf "%s wins! %d to %d" p2name p2_score p1_score
               else Printf.sprintf "Tie! %d to %d" p1_score p2_score
             in
             set_msg message)
           else set_msg "")
  in
  let%arr msg = msg
  and set_msg = set_msg in
  let attrs =
    let base = [ Vdom.Attr.id "winner-message"; Vdom.Attr.class_ "winner-message" ] in
    if String.is_empty msg
    then base
    else
      Vdom.Attr.class_ "show" :: Vdom.Attr.on_animationend (fun _ev -> set_msg "") :: base
  in
  Vdom.Node.div ~attrs [ Vdom.Node.text msg ], set_msg
;;

(* state for patch placement on quilt board / hooks into Bonsai from three.js *)
let place_patch_component ~game_state ~set_game_state ~set_status_msg ~role =
  let%sub () =
    Bonsai.Edge.on_change
      (module Game_state)
      game_state
      ~callback:
        (let%map set_game_state = set_game_state
         and set_status_msg = set_status_msg
         and role = role in
         fun game_state ->
           let my_turn =
             (game_state : Hw2_patchwork_logic.Game_state.t).turn.player_num = role
           in
           Bonsai.Effect.of_sync_fun
             (fun () ->
                if not my_turn
                then
                  set_player_turn
                    (if role = 2
                     then (game_state : Game_state.t).tk1.owned_by
                     else (game_state : Game_state.t).tk2.owned_by)
                    false
                else (
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
                         (fun
                             (pl : float)
                              (r : float)
                              (c : float)
                              (pc : float)
                              (_time : float)
                            ->
                            try
                              let row = int_of_float r in
                              let col = int_of_float c in
                              let patch_choice = int_of_float pc in
                              log
                                ("player: "
                                 ^ string_of_float pl
                                 ^ ", row: "
                                 ^ string_of_int row
                                 ^ ", col: "
                                 ^ string_of_int col
                                 ^ ", patch choice: "
                                 ^ string_of_int patch_choice);
                              let gs : Hw2_patchwork_logic.Game_state.t = state in
                              try
                                let upd_state =
                                  Move.choose_move gs PlacePatch patch_choice row col
                                in
                                if role = 1
                                then
                                  determine_button_income
                                    gs.tk1.owned_by.player_num
                                    gs.tk1.position
                                    upd_state.tk1.position
                                    upd_state.p1qb.accumulated_income
                                    false
                                else
                                  determine_button_income
                                    gs.tk2.owned_by.player_num
                                    gs.tk2.position
                                    upd_state.tk2.position
                                    upd_state.p2qb.accumulated_income
                                    false;
                                reposition_time_tokens
                                  upd_state.tk1.position
                                  upd_state.tk2.position;
                                move_neut_token upd_state.neut.pos false;
                                if gs.turn.player_num = 1
                                then
                                  set_button_count 1 upd_state.tk1.owned_by.buttons_owned
                                else
                                  set_button_count 2 upd_state.tk2.owned_by.buttons_owned;
                                get_patch_choices
                                  upd_state.patches_remaining
                                  upd_state.neut.pos
                                  false;
                                set_player_turn upd_state.turn false;
                                let effect1 = set_game_state upd_state in
                                Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                                let status_advance_str = "You Place Patch" in
                                let effect2 = set_status_msg status_advance_str in
                                Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                                let json =
                                  Hw2_patchwork_logic.Game_state.to_yojson upd_state
                                  |> Yojson.Safe.to_string
                                in
                                mp_save_state ~_json:json 1 2;
                                let opp_turn =
                                  if role = 1
                                  then upd_state.tk2.owned_by
                                  else upd_state.tk1.owned_by
                                in
                                set_player_turn opp_turn false;
                                install upd_state;
                                Js_of_ocaml.Js._true
                              with
                              | Button.Insufficient_funds ->
                                let effect1 = set_status_msg "Not Enough Buttons" in
                                Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                                Js_of_ocaml.Js._false
                              | Game_board.Out_of_bounds ->
                                let effect1 = set_status_msg "Out of Bounds" in
                                Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                                Js_of_ocaml.Js._false
                              | Game_board.Patch_does_not_fit_there ->
                                let effect1 = set_status_msg "Patch Does Not Fit There" in
                                Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                                Js_of_ocaml.Js._false
                            with
                            | exn ->
                              log ("OCaml: unexpected exn: " ^ Exn.to_string exn);
                              Js_of_ocaml.Js._false))
                  in
                  install game_state))
             ())
  in
  let%arr () = Bonsai.Value.return () in
  Vdom.Node.none
;;

let advance_component ~game_state ~set_game_state ~set_status_msg ~multiplayer_mode ~role =
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
         and role = role
         and multiplayer_mode = multiplayer_mode in
         fun game_state ->
           Bonsai.Effect.of_sync_fun
             (fun () ->
                let my_turn =
                  (not multiplayer_mode)
                  || (multiplayer_mode
                      && (game_state : Hw2_patchwork_logic.Game_state.t).turn.player_num
                         = role)
                in
                if not my_turn
                then
                  set_player_turn
                    (if role = 2 then game_state.tk1.owned_by else game_state.tk2.owned_by)
                    (if multiplayer_mode then false else true)
                else
                  Js_of_ocaml.Js.Unsafe.set
                    Js_of_ocaml.Js.Unsafe.global
                    "bonsaiCheckTokenPosition"
                    (Js_of_ocaml.Js.wrap_callback (fun (i : float) ->
                       let n = int_of_float i in
                       let gs : Hw2_patchwork_logic.Game_state.t = game_state in
                       let opp_tok_pos =
                         if gs.turn.player_num = 1
                         then gs.tk2.position
                         else gs.tk1.position
                       in
                       if gs.turn.player_num = 1 && gs.tk1.position = 54
                       then (
                         let effect1 = set_status_msg "Already at End" in
                         Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1)
                       else if gs.turn.player_num = 2 && gs.tk2.position = 54
                       then (
                         let effect1 = set_status_msg "Already at End" in
                         Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1)
                       else if n <> 54 && n <> opp_tok_pos + 1
                       then (
                         let effect1 = set_status_msg "Invalid Move" in
                         Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                         reposition_time_tokens
                           game_state.tk1.position
                           game_state.tk2.position)
                       else (
                         let upd_state = Move.choose_move game_state Advance 0 0 0 in
                         set_button_count 1 upd_state.tk1.owned_by.buttons_owned;
                         set_button_count 2 upd_state.tk2.owned_by.buttons_owned;
                         if role = 1
                         then
                           determine_button_income
                             gs.tk1.owned_by.player_num
                             gs.tk1.position
                             upd_state.tk1.position
                             upd_state.p1qb.accumulated_income
                             false
                         else
                           determine_button_income
                             gs.tk2.owned_by.player_num
                             gs.tk2.position
                             upd_state.tk2.position
                             upd_state.p2qb.accumulated_income
                             false;
                         move_neut_token upd_state.neut.pos false;
                         log
                           ("neut pos after advance: " ^ string_of_int upd_state.neut.pos);
                         let spots_advanced =
                           if gs.turn.player_num = 1
                           then upd_state.tk1.position - gs.tk1.position
                           else upd_state.tk2.position - gs.tk2.position
                         in
                         play_button_flip_animation spots_advanced;
                         get_patch_choices
                           upd_state.patches_remaining
                           upd_state.neut.pos
                           true;
                         let effect1 = set_game_state upd_state in
                         Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
                         let effect2 = set_status_msg "You Advance" in
                         Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
                         let json =
                           Hw2_patchwork_logic.Game_state.to_yojson upd_state
                           |> Yojson.Safe.to_string
                         in
                         mp_save_state ~_json:json 1 2;
                         let opp_turn = upd_state.turn in
                         set_player_turn opp_turn false))))
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
  let%sub multiplayer_mode, set_multiplayer_mode =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%sub _last_turn, _set_last_turn = Bonsai.state (module Int) ~default_model:0 in
  let%sub status, set_status_msg = status_message_component ~message:"" in
  let%sub role, set_role = Bonsai.state (module Int) ~default_model:1 in
  let%sub multiplayer_host, set_multiplayer_host =
    Bonsai.state (module Int) ~default_model:0
  in
  let%sub multiplayer_joinee, set_multiplayer_joinee =
    Bonsai.state (module Int) ~default_model:0
  in
  let%sub _place_patch =
    place_patch_component ~game_state ~set_game_state ~set_status_msg ~role
  in
  let%sub _advance =
    advance_component ~game_state ~set_game_state ~set_status_msg ~multiplayer_mode ~role
  in
  let%sub seen_first, _set_seen_first = Bonsai.state (module Bool) ~default_model:false in
  let%sub winner, _set_winner = winner_component ~game_state in
  let state_to_json_string (st : Game_state.t) : string =
    st |> Game_state.to_yojson |> Yojson.Safe.to_string
  in
  let%sub ui_ready, set_ui_ready = Bonsai.state (module Bool) ~default_model:false in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map (game_state : Hw2_patchwork_logic.Game_state.t) = game_state
         and set_multiplayer_mode = set_multiplayer_mode
         and set_game_state = set_game_state
         and set_multiplayer_host = set_multiplayer_host
         and multiplayer_mode = multiplayer_mode
         and set_multiplayer_joinee = set_multiplayer_joinee
         and set_ui_ready = set_ui_ready
         and set_role = set_role in
         Bonsai.Effect.of_sync_fun
           (fun () ->
              if not multiplayer_mode
              then (
                initialize_patches game_state.patches game_state.neut.pos;
                reposition_time_tokens game_state.tk1.position game_state.tk2.position;
                set_button_count 1 game_state.tk1.owned_by.buttons_owned;
                set_button_count 2 game_state.tk2.owned_by.buttons_owned;
                move_neut_token game_state.neut.pos false;
                set_player_turn game_state.turn true;
                log "calling get_patch_choices";
                get_patch_choices game_state.patches_remaining game_state.neut.pos true)
              else ();
              Js_of_ocaml.Js.Unsafe.set
                Js_of_ocaml.Js.Unsafe.global
                "setMultiplayerMode"
                (Js_of_ocaml.Js.wrap_callback (fun (onOff : bool) (matchID : string) rl ->
                   log ("matchID: " ^ matchID ^ ", rl: " ^ rl);
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_multiplayer_mode onOff);
                   let rl_i = int_of_string rl in
                   if rl_i = 2
                   then Bonsai_web.Effect.Expert.handle_non_dom_event_exn (set_role 2)
                   else Bonsai_web.Effect.Expert.handle_non_dom_event_exn (set_role 1);
                   init_mp matchID rl_i));
              let open Js.Unsafe in
              let g = global in
              set
                g
                "setUIReady"
                (Js.wrap_callback (fun () ->
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn (set_ui_ready true)));
              set
                g
                "setPlayerName"
                (Js.wrap_callback (fun (nm : string) (num : string) ->
                   let pnum = int_of_string num in
                   if pnum = 1
                   then (
                     log ("setting player " ^ num ^ "'s name to " ^ nm);
                     (game_state : Hw2_patchwork_logic.Game_state.t).tk1.owned_by.player_name
                     <- nm)
                   else (
                     log ("setting player " ^ num ^ "'s name to " ^ nm);
                     (game_state : Hw2_patchwork_logic.Game_state.t).tk2.owned_by.player_name
                     <- nm);
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_game_state game_state);
                   mp_save_state 1 2));
              set
                g
                "setHost"
                (Js.wrap_callback (fun (hst : float) ->
                   let host_int = int_of_float hst in
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_multiplayer_host host_int);
                   let joinee_int = if host_int = 1 then 2 else 1 in
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_multiplayer_joinee joinee_int);
                   reposition_time_tokens game_state.tk1.position game_state.tk2.position;
                   set_button_count 1 game_state.tk1.owned_by.buttons_owned;
                   set_button_count 2 game_state.tk2.owned_by.buttons_owned;
                   move_neut_token game_state.neut.pos false;
                   set_player_turn game_state.turn false;
                   get_patch_choices
                     game_state.patches_remaining
                     game_state.neut.pos
                     false;
                   mp_save_state host_int joinee_int;
                   mark_initial_state_as_saved ()));
              set
                g
                "setInitialHostAndJoinee"
                (Js.wrap_callback (fun (h : float) (j : float) ->
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_multiplayer_host (int_of_float h));
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_multiplayer_joinee (int_of_float j))));
              set
                g
                "setRole"
                (Js.wrap_callback (fun (rl : float) ->
                   let role_int = int_of_float rl in
                   Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                     (set_role (if role_int = 2 then 2 else 1))));
              (try
                 let fn = get g "onBonsaiReady" in
                 ignore (fun_call fn [||])
               with
               | _ -> ());
              set g "lastStateJson" (Js.string (state_to_json_string game_state));
              set
                g
                "getStateJson"
                (Js.wrap_callback (fun () ->
                   (Js.Unsafe.get g "lastStateJson" : Js.js_string Js.t)));
              set
                g
                "applyStateJson"
                (Js.wrap_callback
                   (fun
                       (s : Js.js_string Js.t)
                        (h : float)
                        (j : float)
                        (ident : bool Js.t)
                        (role_f : float)
                      ->
                      let role_i = int_of_float role_f in
                      log ("applying state for player " ^ string_of_int role_i);
                      try
                        let s = Js.to_string s in
                        let host = int_of_float h in
                        let joinee = int_of_float j in
                        Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                          (set_multiplayer_host host);
                        Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                          (set_multiplayer_joinee joinee);
                        match Game_state.of_yojson (Yojson.Safe.from_string s) with
                        | Ok st ->
                          let effect = set_game_state st in
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect;
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                            (set_multiplayer_host host);
                          if Js.to_bool ident
                          then initialize_patches st.patches st.neut.pos
                          else ();
                          (* reproduce opponent's quilt board *)
                          let rec place_opponent_patches_in_ui pl =
                            match pl with
                            | [] -> ()
                            | (row, col, patch, rot) :: tl ->
                              log
                                ("applying state, placing patch on qb, role of player: "
                                 ^ string_of_int role_i);
                              let board_to_copy_onto = if role_i = 2 then 1 else 2 in
                              place_ai_patch_on_qb
                                board_to_copy_onto
                                patch
                                row
                                col
                                rot
                                false;
                              place_opponent_patches_in_ui tl
                          in
                          clear_opp_patches ();
                          place_opponent_patches_in_ui
                            (if role_i = 1 then st.p2qb.patches else st.p1qb.patches);
                          reposition_time_tokens st.tk1.position st.tk2.position;
                          set_button_count 1 st.tk1.owned_by.buttons_owned;
                          set_button_count 2 st.tk2.owned_by.buttons_owned;
                          set_player_turn st.turn false;
                          move_neut_token st.neut.pos false;
                          get_patch_choices st.patches_remaining st.neut.pos false;
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                            (set_multiplayer_joinee role_i)
                        | Error _ -> ()
                      with
                      | _ -> ()));
              set
                g
                "queryPlayerTurn"
                (Js_of_ocaml.Js.wrap_callback (fun () ->
                   Js_of_ocaml.Js.number_of_float
                     (float_of_int game_state.turn.player_num))))
           ())
      ()
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module Hw2_patchwork_logic.Game_state)
      game_state
      ~callback:
        (let%map set_game_state = set_game_state
         and set_status_msg = set_status_msg
         and seen_first = seen_first
         and ui_ready = ui_ready
         and multiplayer_host = multiplayer_host
         and multiplayer_joinee = multiplayer_joinee
         and role = role
         and set_multiplayer_joinee = set_multiplayer_joinee
         and multiplayer_mode = multiplayer_mode in
         fun (gs : Hw2_patchwork_logic.Game_state.t) ->
           if multiplayer_mode && not ui_ready
           then Bonsai.Effect.of_sync_fun (fun () -> ()) ()
           else (
             let ui_effect =
               Bonsai.Effect.of_sync_fun
                 (fun () ->
                    let open Js.Unsafe in
                    let g = global in
                    set
                      g
                      "queryPlayerTurn"
                      (Js_of_ocaml.Js.wrap_callback (fun () ->
                         Js_of_ocaml.Js.number_of_float (float_of_int gs.turn.player_num)));
                    if multiplayer_mode
                    then (
                      if multiplayer_host <> role && multiplayer_joinee = 0
                      then (
                        initialize_patches gs.patches gs.neut.pos;
                        Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                          (set_multiplayer_joinee 2))
                      else ();
                      reposition_time_tokens gs.tk1.position gs.tk2.position;
                      set_button_count 1 gs.tk1.owned_by.buttons_owned;
                      set_button_count 2 gs.tk2.owned_by.buttons_owned;
                      set_player_turn gs.turn (if multiplayer_mode then false else true);
                      if seen_first then move_neut_token gs.neut.pos false;
                      get_patch_choices gs.patches_remaining gs.neut.pos false;
                      if gs.turn.player_num <> role
                      then ai_start_turn ()
                      else ai_end_turn ())
                    else ())
                 ()
             in
             let ai_effect =
               if (not multiplayer_mode) && gs.turn.player_num = 2
               then
                 Bonsai.Effect.of_sync_fun
                   (fun () ->
                      ai_start_turn ();
                      delay 3000 (fun () ->
                        let mv, p, r, c = Hw4_patchwork_ai_heuristics.determine_move gs in
                        match mv with
                        | Move.PlacePatch ->
                          log ("ai chooses patch " ^ string_of_int p);
                          place_ai_patch_on_qb 2 p r c 0 true;
                          let upd = Move.choose_move gs Move.PlacePatch p r c in
                          determine_button_income
                            gs.tk2.owned_by.player_num
                            gs.tk2.position
                            upd.tk2.position
                            upd.p2qb.accumulated_income
                            true;
                          reposition_time_tokens upd.tk1.position upd.tk2.position;
                          set_button_count 1 upd.tk1.owned_by.buttons_owned;
                          set_button_count 2 upd.tk2.owned_by.buttons_owned;
                          move_neut_token upd.neut.pos false;
                          log
                            ("neut token pos after ai place patch: "
                             ^ string_of_int upd.neut.pos);
                          set_player_turn upd.turn true;
                          get_patch_choices upd.patches_remaining upd.neut.pos false;
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                            (set_game_state upd);
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                            (set_status_msg "AI Places Patch");
                          ai_end_turn ()
                        | Move.Advance ->
                          let upd = Move.choose_move gs Move.Advance 0 0 0 in
                          determine_button_income
                            gs.tk2.owned_by.player_num
                            gs.tk2.position
                            upd.tk2.position
                            upd.p2qb.accumulated_income
                            true;
                          reposition_time_tokens upd.tk1.position upd.tk2.position;
                          set_button_count 1 upd.tk1.owned_by.buttons_owned;
                          set_button_count 2 upd.tk2.owned_by.buttons_owned;
                          set_player_turn upd.turn true;
                          get_patch_choices upd.patches_remaining upd.neut.pos false;
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                            (set_game_state upd);
                          Bonsai_web.Effect.Expert.handle_non_dom_event_exn
                            (set_status_msg "AI Advances");
                          ai_end_turn ()))
                   ()
               else Bonsai.Effect.of_sync_fun (fun () -> ()) ()
             in
             Bonsai.Effect.Many [ ui_effect; ai_effect ]))
  in
  let%arr status = status
  and winner = winner in
  Vdom.Node.div ~attrs:[ Vdom.Attr.id "patchwork_game" ] [ status; winner ]
;;

let () = Start.start ~bind_to_element_with_id:"patchwork_game" game_component
