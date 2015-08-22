open PlayerData	(*For playerName*)

open SharedData (*For material and possibly other stuff*)

open Network (*For CouldNotConnectException*)

open Unix

type options = 
{ opt_p0: playerName;
  opt_p1: playerName;
  opt_skillLevel: float;
  opt_noSound:bool;
  opt_realisticParabolaOpacity:bool;
  opt_surface : material;
  opt_resX : int;
  opt_resY : int;
  opt_showFps : bool;
  opt_aidebug : bool;
  opt_doNotShowPause : bool }

type argumentResult = ArgumentError of string | ArgumentsOk of options

let parseOptions = 
	(**It starts by loading default data and configuring options**)
    let defaultComputerSkill = 190 
    and defaultPort = 4000 in
    let aidebug = ref false
    and doNotShowPause = ref false
    and p0Name = ref "ivan"
    and p1Name = ref "ivan"
    and showFps = ref false
    and noSound = ref false
    and port = ref defaultPort
    and realisticPar = ref false
    and newbiePar = ref false
    and server = ref false
    and resX = ref 640
    and resY = ref 480
    and client = ref ""
    (* Unused variable? -- and camHt = ref "mid" --*)
    (* Unused variable? -- and clientByName = ref "" --*)
    and computerSkill = ref defaultComputerSkill
    and surf = ref "cement" in

    let arguments = [ "-p0", Arg.Set_string p0Name, "The name of player 0. Possible names are \"pete\", \"ivan\", \"mats\".";
		      "-p1", Arg.Set_string p1Name, "The name of player 1. Possible names are \"pete\", \"ivan\", \"mats\".";
		      "-donotshowpause", Arg.Set doNotShowPause, "When the game is paused, do not show the \"paused\" sign. Useful for screenshots." ;
		      "-showfps", Arg.Set showFps, "Display the number of Frames Per Second (on the standard output). Useful for debugging." ;
		      "-aidebug", Arg.Set aidebug, "Shows some info useful to debug AI." ;
		      "-realistic", Arg.Set realisticPar, "Make the game realistic. This alters the parabola visibility depending on the player and shot. Better gameplay, but not suitable for newbies." ;
		      "-newbie", Arg.Set newbiePar, "Make the game not realistic, but more suited to newbies. The parabola is fully visible." ;
		      "-nosound", Arg.Set noSound, "Run with no sound." ;
		      "-xres", Arg.Set_int resX, "The horizontal resolution." ;
		      "-yres", Arg.Set_int resY, "The vertical resolution." ;
		      "-port", Arg.Set_int port, "The TCP port to use. Makes sense with -client and -server. The default is " ^ string_of_int defaultPort ;
		      "-computerskill", Arg.Set_int computerSkill, "Skill level of computer. A number from 0 (very difficult) to 250 (very easy). Default is " ^
			  string_of_int defaultComputerSkill;
		      "-client", Arg.Set_string client, "The IP address of the server. Only for multiplayer." ;
		      "-server", Arg.Set server, "Run Free Tennis as server. Only for multiplayer." ;
		      "-surf", Arg.Set_string surf, ". SURF: surface for the court. Choices are \"clay\", \"cement\", \"grass\"."
		    ] in

    let anonFun s = 
	print_endline "No anonymous arguments allowed." in

    Arg.parse arguments anonFun "Usage: see manual.";



    let parsedOptions = 

	if not !realisticPar && not !newbiePar then
	    ArgumentError ("You must speficy either -realistic or -newbie.")
	else if  !realisticPar &&  !newbiePar then
	    ArgumentError ("You cannot speficy both -realistic and -newbie. These are mutually exclusive flags.")
	else
	    let translateName n =
		if 0 = compare  n  "mats" then Some Mats 
		else if 0 = compare n  "pete" then Some Pete
		else if 0 = compare n "ivan" then Some Ivan 
		else None 
	    in
	    let mayb = translateName (String.lowercase !p0Name ) in
	    match mayb with
		| None -> 
		      ArgumentError ("name " ^ !p0Name ^" invalid.")
		| Some p0 ->
		      let mayb = translateName (String.lowercase !p1Name) in
		      match mayb with
			  | None ->
				ArgumentError ("name " ^ !p1Name ^" invalid.")
			  | Some p1 ->

				let mayb = 

				    let translateSurf s =
					if 0 = compare s "cement" then Some Cement
					else if 0 = compare s "clay" then Some Clay 
					else if 0= compare s "grass" then Some Grass
					else None in
				    translateSurf (String.lowercase !surf) in
				match mayb with 
				    | None ->
					  ArgumentError ("surface " ^ !surf ^" is invalid.")
				    | Some su ->
					  if !computerSkill < 0 || !computerSkill > 250 then
					      ArgumentError ("computer-skill is invalid")
					  else
					      ArgumentsOk {opt_p0 = p0;
							   opt_p1 = p1;
							   opt_realisticParabolaOpacity = !realisticPar;
							   opt_skillLevel = float_of_int !computerSkill;
							   opt_noSound = !noSound;
							   opt_surface = su;
								opt_resX = !resX;
  								opt_resY = !resY;
  								opt_showFps = !showFps;
  								opt_aidebug = !aidebug;
							  	opt_doNotShowPause = !doNotShowPause }
		in
		(parsedOptions, server, port, client)
