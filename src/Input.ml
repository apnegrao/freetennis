open Sdlevent

(** --- Consts --- **)
let pressGMessage = "Press F or G to grab the mouse input. Read the manual for more info! :-)"
(*These two next strings are not Input related...*)
let loadingString = "Loading... please wait"
let freeTennisString = "Free Tennis"

let mouseRefresh = 1.0 /. 24.0 (* seconds *)

(** --- Data Types --- **)
type mouse = {m_rightButtonPressed:bool; m_leftButtonPressed:bool ;
	      m_xRel:int; 
	      m_yRel:int; 
	      m_secondsSinceLastMouseMotion: float}

(* TODO: Not yet used. Add after the refactoring *)
type keyboard = {
		downKeyPressed:bool; upKeyPressed:bool ;
		rightKeyPressed:bool; leftKeyPressed:bool;
		wantsToDive:bool; wantsToSlice:bool;
		secondsSinceKeyPress: float
}

type varData = { vd_mouse:mouse; vd_windowWt:int; vd_windowHt:int;
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
