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
  if String.equal ty "function" then
    ignore (fun_call fn [| inject pos |])
      else ()

let reposition_time_tokens (p1 : int) (p2: int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "repositionTimeTokens" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then
    ignore (fun_call fn [| inject p1; inject p2 |])
      else ()

let move_time_token (pnum : int) (pos: int) =
  let open Js.Unsafe in
  let g = global in
  let fn = get g "positionToken" in
  let ty = Js.to_string (Js.typeof fn) in
  if String.equal ty "function" then
    ignore (fun_call fn [| inject pnum; inject pos |])
    else ()

let set_button_count (p : int) (count : int) =
  let id = if p = 1 then "p1-buttons" else "p2-buttons" in
  match Dom_html.getElementById_opt id with
  | None -> log "set button count: failed"
  | Some span -> span##.textContent := Js.Opt.return (Js.string ("x " ^ string_of_int count));
    log("set button count: succeeded" ^ string_of_int count)

let pieces =
  let p1_span = Dom_html.getElementById_exn "player-box-1" in
  let player_name = Js.Opt.case p1_span##.textContent (fun () -> "") Js.to_string in
  log player_name;
  Game_pieces.setup_game player_name "AI" "Red" "Blue"


let advance_component ~game_state ~set_game_state =
  let%sub position_on_mb, set_position_on_mb = Bonsai.state (module Int) ~default_model:1
  in
  let%sub status_msg, set_status_msg = Bonsai.state (module String) ~default_model:"" in
  let%arr status_msg = status_msg
  and set_status_msg = set_status_msg
  and _position_on_mb = position_on_mb
  and set_position_on_mb = set_position_on_mb
  and game_state = game_state
  and set_game_state = set_game_state in


  let move_tok (player : int) (new_pos : int) =
    move_time_token player new_pos;
    let effect = set_position_on_mb new_pos in
    Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect
  in

  let () =
    let open Js.Unsafe in
    let callback =
      Js.wrap_callback (fun (i: float) ->
        let n = int_of_float i in
        if (n <> game_state.Game_state.tk2.position + 1) then (
          log ("board position moved to: " ^ string_of_int n ^ ", tk2 position: " ^ string_of_int pieces.time_piece2.position);
          let effect1 = set_status_msg "Invalid Move" in
          Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
          reposition_time_tokens game_state.Game_state.tk1.position game_state.Game_state.tk2.position;
        ) else (
          let player = game_state.turn in
          let upd_state = Move.choose_move game_state Advance 0 0 0 in
          let effect1 = set_game_state upd_state in
          Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect1;
          if (player.player_num = 1) then set_button_count 1 upd_state.tk1.owned_by.buttons_owned else
          set_button_count 2 upd_state.tk2.owned_by.buttons_owned;
          let effect2 = set_status_msg "Advanced!" in
          Bonsai_web.Effect.Expert.handle_non_dom_event_exn effect2;
          move_tok player.player_num n;
        ))
    in
    set global "bonsaiCheckTokenPosition" callback
  in
  let attrs =
    let base = [ Vdom.Attr.id "status-message"; Vdom.Attr.class_ "status-message"]
    in
    if String.is_empty status_msg then base
      else Vdom.Attr.class_ "show" ::
      Vdom.Attr.on_animationend (fun _ev ->
        set_status_msg "")
  :: base
  in

  Vdom.Node.div
  ~attrs
    [ Vdom.Node.text status_msg ]


let game_component =
  let initial_state = Game_state.initialize_state pieces in
  let%sub game_state, set_game_state =
    Bonsai.state (module Game_state) ~default_model:initial_state
  in
  let%sub advance = advance_component ~game_state ~set_game_state in

  let%arr game_state = game_state
  and advance = advance in

  let neutral_position = game_state.neut.pos in
  dim_ineligible_patches neutral_position;
  reposition_time_tokens game_state.tk1.position game_state.tk2.position;
  set_button_count 1 game_state.tk1.owned_by.buttons_owned;
  set_button_count 2 game_state.tk2.owned_by.buttons_owned;
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.id "patchwork_game" ]
    [ advance; ]


let () = Start.start ~bind_to_element_with_id:"patchwork_game" game_component
