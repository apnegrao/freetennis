open Camera

open Sdlevent (* for mme_xrel etc *)

(** --- Consts --- **)
let pressGMessage = 
  "Press F or G to grab the mouse input. Read the manual for more info! :-)"
(* These two next strings are not Input related...*)
let loadingString = "Loading... please wait"
let freeTennisString = "Free Tennis"

let mouseRefresh = 1.0 /. 24.0 (* seconds *)

(** --- Data Types --- **)
type mouse = {
  m_rightButtonPressed:bool; 
  m_leftButtonPressed:bool ;
  m_xRel:int; 
  m_yRel:int; 
  m_secondsSinceLastMouseMotion: float
}

(* TODO: Not yet used. Add after the refactoring *)
type keyboard = {
  downKeyPressed:bool; upKeyPressed:bool ;
  rightKeyPressed:bool; leftKeyPressed:bool;
  wantsToDive:bool; wantsToSlice:bool;
  secondsSinceKeyPress: float
}

type varData = {
  vd_mouse:mouse; vd_windowWt:int; vd_windowHt:int;
  vd_pausedWithKey:bool;vd_slowMotionFactor:float;
  vd_fullScreen:bool; vd_deltaCamera:float; vd_mustQuit:bool
  (* vd_keyboard:keyboard*)
}

(** --- Functions --- **)
(* TODO: These next two functions are not being used for the moment. They manage the
   new keyboard based game play that has not yet been introduced to the game. Both
   functions are called by the manageAllPendingSdlEvents in the main file.
   let processKeyPressedEvents ~k (*the Key pressed *) ~vd= 
   	(* Pressing the minus key slows the game *)
   	if k.keysym = Sdlkey.KEY_MINUS then
{vd with vd_slowMotionFactor = vd.vd_slowMotionFactor -. 0.1}
  	(* Pressing the 0 key makes the game faster [until a maximum value is reached]*)
  	else if k.keysym = Sdlkey.KEY_0 then
  	      { vd with vd_slowMotionFactor = min 1.0 ( vd.vd_slowMotionFactor +. 0.1)}
  	(* Pressing escape quits the game *)	  
  	else if k.keysym = Sdlkey.KEY_ESCAPE then
  	      begin
  		  print_endline "Exiting";
  		  {vd with vd_mustQuit = true}
  	      end
  	(* Pressing escape pauses the game *)
  	else if k.keysym = Sdlkey.KEY_p then
  		if vd.vd_pausedWithKey then 
  			( print_endline "Resuming";
  			{ vd with vd_pausedWithKey = false})
  		else
  			( print_endline "Pausing";
  			{ vd with vd_pausedWithKey = true})
  	(* Pressing F either toggles or leaves fullscreen *)
  	else if k.keysym = Sdlkey.KEY_f then
  		if vd.vd_fullScreen then
  			let _ = Sdlwm.toggle_fullscreen () in
  				( Sdlmouse.show_cursor true;
  				Sdlwm.grab_input false;
  				Sdlwm.set_caption ~title:pressGMessage ~icon:freeTennisString;
  				{vd with vd_fullScreen = false})
  		else
  			(* I may have or not have the grab *)
  			let _ = Sdlwm.toggle_fullscreen () in
  				( Sdlmouse.show_cursor false;
  				Sdlwm.grab_input true;
  				Sdlwm.set_caption ~title:freeTennisString ~icon:freeTennisString;
  				{vd with vd_fullScreen = true})
  	(* Pressing G either attaches the mouse to the game window of frees it *)
  	else if k.keysym = Sdlkey.KEY_g then
  		let full = 
  			if Sdlwm.query_grab () then
  				(* removing the grab and the fullscreen *)
  				(Sdlwm.grab_input false;
  				Sdlmouse.show_cursor true;
  				Sdlwm.set_caption ~title:pressGMessage ~icon:freeTennisString;
  				if vd.vd_fullScreen then
  					let _ = Sdlwm.toggle_fullscreen () in
  						false
  				else
  					false
  				)
  			else
  			      ( Sdlwm.grab_input true;
  				Sdlmouse.show_cursor false;
  				Sdlwm.set_caption ~title:freeTennisString ~icon:freeTennisString;
  				vd.vd_fullScreen )
  	      in
  	      { vd with vd_fullScreen = full}
  	(** Experimenting with keys**)
  	(* Key down *)
  	else if k.keysym = Sdlkey.KEY_s then
  		let keyboard =
  			{vd.vd_keyboard with downKeyPressed = true;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	(* Key up *)
  	else if k.keysym = Sdlkey.KEY_w then
  		let keyboard =
  			{vd.vd_keyboard with upKeyPressed = true;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	(* Key left *)
  	else if k.keysym = Sdlkey.KEY_a then
  		let keyboard =
  			{vd.vd_keyboard with leftKeyPressed = true;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	(* Key right *)
  	else if k.keysym = Sdlkey.KEY_d then
  		let keyboard =
  			{vd.vd_keyboard with rightKeyPressed = true;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	else
  	      vd

  let processKeyReleasedEvents ~k ~vd =
  	if k.keysym = Sdlkey.KEY_s then
  		let keyboard =
  			{vd.vd_keyboard with downKeyPressed = false;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	(* Key up *)
  	else if k.keysym = Sdlkey.KEY_w then
  		let keyboard =
  			{vd.vd_keyboard with upKeyPressed = false;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	(* Key left *)
  	else if k.keysym = Sdlkey.KEY_a then
  		let keyboard =
  			{vd.vd_keyboard with leftKeyPressed = false;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	(* Key right *)
  	else if k.keysym = Sdlkey.KEY_d then
  		let keyboard =
  			{vd.vd_keyboard with rightKeyPressed = false;
  			    secondsSinceKeyPress = 0.0}
  		in
  		{vd with vd_keyboard = keyboard}
  	else
  vd*)

(** Processes the queue of pending IO (mostly mouse and key) events **)
(** This could be put on a different file, right? **)
let rec manageAllPendingSdlEvents ~vd ~windowHt ~windowWt=
  let nextEvent = Sdlevent.poll () in
  match nextEvent with
  | None -> vd
  | Some e ->
    begin
      match e with
      | Sdlevent.QUIT ->
        print_endline "Exiting";
        {vd with vd_mustQuit = true}
      | Sdlevent.KEYDOWN k ->
        let vd' = 
          if k.keysym = Sdlkey.KEY_MINUS then
            {vd with vd_slowMotionFactor = vd.vd_slowMotionFactor -. 0.1}
          else if k.keysym = Sdlkey.KEY_0 then
            { vd with vd_slowMotionFactor = 
                        min 1.0 ( vd.vd_slowMotionFactor +. 0.1)}
          else if k.keysym = Sdlkey.KEY_ESCAPE then
            begin
              print_endline "Exiting";
              {vd with vd_mustQuit = true}
            end
          else if k.keysym = Sdlkey.KEY_p then
            if vd.vd_pausedWithKey then
              ( print_endline "Resuming";
                { vd with vd_pausedWithKey = false})
            else
              ( print_endline "Pausing";
                { vd with vd_pausedWithKey = true})
          else if k.keysym = Sdlkey.KEY_f then
            if vd.vd_fullScreen then
              let _ = Sdlwm.toggle_fullscreen () in
              ( Sdlmouse.show_cursor true;
                Sdlwm.grab_input false;
                Sdlwm.set_caption ~title:pressGMessage ~icon:freeTennisString;
                {vd with vd_fullScreen = false})
            else
              (* I may have or not have the grab *)
              let _ = Sdlwm.toggle_fullscreen () in
              ( Sdlmouse.show_cursor false;
                Sdlwm.grab_input true;
                Sdlwm.set_caption ~title:freeTennisString ~icon:freeTennisString;
                {vd with vd_fullScreen = true})
          else if k.keysym = Sdlkey.KEY_g then
            let full = 
              if Sdlwm.query_grab () then
                (* removing the grab and the fullscreen *)
                (Sdlwm.grab_input false;
                 Sdlmouse.show_cursor true;
                 Sdlwm.set_caption ~title:pressGMessage ~icon:freeTennisString;
                 if vd.vd_fullScreen then
                   let _ = Sdlwm.toggle_fullscreen () in
                   false
                 else
                   false
                )
              else
                ( Sdlwm.grab_input true;
                  Sdlmouse.show_cursor false;
                  Sdlwm.set_caption ~title:freeTennisString ~icon:freeTennisString;
                  vd.vd_fullScreen )
            in
            { vd with vd_fullScreen = full}
          else
            vd
        in
        manageAllPendingSdlEvents vd' windowHt windowWt
      | Sdlevent.VIDEORESIZE (w, h) ->
        print_endline ( "Video resized" ^ string_of_int w ^
                        ", " ^ string_of_int h);
        let w, h = resizeCallback w h in
        manageAllPendingSdlEvents {vd with vd_windowWt = w; vd_windowHt = h}
              windowHt windowWt
      | Sdlevent.MOUSEBUTTONDOWN m ->
        let mouse =
          if m.mbe_button = Sdlmouse.BUTTON_LEFT then
            { vd.vd_mouse with m_leftButtonPressed = true}
          else if m.mbe_button = Sdlmouse.BUTTON_RIGHT then
            { vd.vd_mouse with m_rightButtonPressed = true}
          else
            vd.vd_mouse
        in
        manageAllPendingSdlEvents {vd with vd_mouse = mouse} windowHt windowWt
      | Sdlevent.MOUSEBUTTONUP m ->
        let mouse =
          if m.mbe_button = Sdlmouse.BUTTON_LEFT then
            { vd.vd_mouse with m_leftButtonPressed = false}
          else if m.mbe_button = Sdlmouse.BUTTON_RIGHT then
            { vd.vd_mouse with m_rightButtonPressed = false}
          else
            vd.vd_mouse
        in
        manageAllPendingSdlEvents {vd with vd_mouse = mouse} windowHt windowWt
      | Sdlevent.ACTIVE e -> manageAllPendingSdlEvents vd windowHt windowWt
      | Sdlevent.MOUSEMOTION m ->
        if m.mme_xrel = 0 && m.mme_yrel =0 then
          (* @@ why does this happen sometimes? *)
          manageAllPendingSdlEvents 
            {vd with vd_mouse = { vd.vd_mouse with
                                  m_xRel = 0;
                                  m_yRel = 0;
                                  m_secondsSinceLastMouseMotion = 0.0}}
            windowHt windowWt
        else
          let warpx =
            if m.mme_x > windowWt - 20 then Some 20
            else if m.mme_x < 20 then Some (windowWt - 20)
            else None
          in
          let warpy =
            if m.mme_y > windowHt - 20 then Some 20
            else if m.mme_y < 20 then Some (windowHt - 20)
            else None
          in
          if Sdlwm.query_grab () then
            begin
              match warpx with
              | None ->
                begin
                  match warpy with
                  | None -> ()
                  | Some y -> Sdlmouse.warp m.mme_x y
                end
              | Some x ->
                begin
                  match warpy with
                  | None -> Sdlmouse.warp x m.mme_y
                  | Some y -> Sdlmouse.warp x y
                end
            end
          else
            ();

          let mouse =
            if Sdlwm.query_grab () then
              let xr =
                if abs m.mme_xrel < windowWt - 50 then m.mme_xrel
                else vd.vd_mouse.m_xRel 
              in
              let yr =
                if abs m.mme_yrel < windowHt - 50 then m.mme_yrel
                else vd.vd_mouse.m_yRel 
              in
              {vd.vd_mouse with m_xRel = xr;
                                m_yRel = yr;
                                m_secondsSinceLastMouseMotion = 0.0} 
            else
              vd.vd_mouse
          in
          manageAllPendingSdlEvents {vd with vd_mouse = mouse} windowHt windowWt
      | JOYAXISMOTION _ | JOYBALLMOTION _ | JOYHATMOTION _ | JOYBUTTONDOWN _ |
       JOYBUTTONUP _ | SYSWM  | VIDEOEXPOSE  | USER _ | KEYUP _ ->
        manageAllPendingSdlEvents vd windowHt windowWt
    end
