open PlayerData

open Objects3D

open SharedData

open BallMovement

open Math

open Network

open Camera

open List

(** --- Consts --- **)
let fovY = 16.9 (* increase the fov, and the upper player will be smaller with respect to the lower *)

let zNear = 100.0

(** --- Data Types --- **)

type noTie = {points: int array; games: int array}

type scoreState = TieBreak of int array | NoTieBreak of noTie

type score = { sc_state: scoreState;
	       sc_finishedSets: (int array) list }

(** --- Functions --- **)
(* *)
let whoServes s = 
    let firstToServe = 0 in
    match s.sc_state with
	| TieBreak points ->
	       let sum = points.(0) + points.(1) in
	       let sum_q_mo =
		   let q = sum / 2 
		   and mo = sum mod 2 in
		   q + mo in
	       
	       let isEven n = n mod 2 = 0 in

	       if isEven sum_q_mo then firstToServe else 1 - firstToServe
	| NoTieBreak q ->
	      (* @@ sbagliato, funziona solo per
		 il primo set. In realta' devo
		 contare tutti i game di tutti i
		 set. *)
	      if ((q.games.(0) +  q.games.(1)) mod 2 ) = 0 then firstToServe else 1 - firstToServe

(* As the name indicates, this functions prints 2d elements such as the score and 
other strings shown in various parts of the game ('Pause', 'Fault', etc...), as well
as the energy bar of the player *)
(* TODO: Check if we can coalesce all these pause parameters into a single one *)
(* FIXME: Actually, take a good look at all these parameters to see what makes 
sense here... *)
let render2dStuff ~players ~ball ~serverData ~doNotShowPause ~pausedOnTheOtherSide 
                ~pausedWithKey  ~noOneIsServing  ~windowHt ~windowWt ~handleOfTexture s = 
    let factor = float_of_int windowHt /. 480.0 in
    let d = 40.0 *.  factor
		and h = 25.0 *. factor
		and d2 = (22.0 *. 22.0 /. 26.0) *. factor
		and h2 = 22.0 *. factor in

    let plBelow =
				if playsInTopmostCourtHalf  players.(0) then players.(1) else players.(0) in
    let renderNumber n destx desty =
				    GlDraw.color ~alpha:1.0 (1.0 , 1.0, 1.0);

				    let t = 
					if n = 0 then
					    gfxDir ^ "/0.png"
					else if n = 1 then
					    gfxDir ^ "/15.png"
					else if n = 2 then
					    gfxDir ^ "/30.png"
					else if n = 3 then
					    gfxDir ^ "/40.png"
					else
					    gfxDir ^ "/adv.png"
				    in
				    let te = 
					try
					    (StringMap.find t handleOfTexture)
					with Not_found ->
					    ( print_endline ("texture  Not_found:" ^ t); raise Not_found)
				    in
				    GlTex.bind_texture ~target:`texture_2d te;
				    Gl.enable `texture_2d;
				    GlDraw.begins `triangle_fan;
				    

				    let verts =
					[  (0.0 ,  0.0, 0.0 , 0.0);
					   (d,  0.0, 1.0, 0.0);
					   (d, h, 1.0, 1.0);
					   (0.0, h, 0.0, 1.0) ] in
				    let foo (x, y, u, v) = 
					GlTex.coord2 (u, v) ;
					GlDraw.vertex2 ( x +. destx, y +. desty) in
				    
				    List.iter foo verts;
				    GlDraw.ends ();
				    
				in

				let renderString09 s destx desty = 

				    let renderNumber09 n destx desty =
					if n = ' ' then
					    ()
					else
					    (
						GlDraw.color ~alpha:1.0 (1.0 , 1.0, 1.0);
						
						let t = 
						    gfxDir ^ "/n" ^ String.make 1 n ^ ".png" in
						let te = 
						    try
							(StringMap.find t handleOfTexture)
						    with Not_found ->
							( print_endline ("texture  Not_found:" ^ t); raise Not_found)
						in
						GlTex.bind_texture ~target:`texture_2d te;
						Gl.enable `texture_2d;
						GlDraw.begins `triangle_fan;
						
						
						let verts =
						    [  (0.0 ,  0.0, 0.0 , 0.0);
						       (d2,  0.0, 1.0, 0.0);
						       (d2, h2, 1.0, 1.0);
						       (0.0, h2, 0.0, 1.0) ] in
						let foo (x, y, u, v) = 
						    GlTex.coord2 (u, v) ;
						    GlDraw.vertex2 ( x +. destx, y +. desty) in
						
						List.iter foo verts;
						GlDraw.ends ())
						
				    in

				    for x = 0 to (String.length s) -1 do
					let tx = destx +. (float_of_int x) *. d2 in
					renderNumber09 s.[x] tx desty

				    done
				in

				GlMat.mode `projection;
				GlMat.load_identity ();
				GlMat.ortho ~x:(0.0, float_of_int windowWt)
				    ~y:(float_of_int windowHt, 0.0) 
				    ~z:(-. 1.0, 0.0) ;
				GlMat.mode `modelview;
				GlMat.load_identity ();
				Gl.disable `depth_test;



				let ballIsInPlay = 
				    match ball.b_state with
					| BS_Still _ -> false
					| BS_Moving bsm ->
					      bsm.bsm_isItGoodSoFar
				in
				let shouldRenderFault = 
				    match ball.b_state with
					| BS_Still _ -> false
					| BS_Moving bsm ->
					      not bsm.bsm_isItGoodSoFar  && bsm.bsm_bouncesSoFar = 1
				in
				let shouldRenderTooLate = 
				    match plBelow with
					| HP h ->
					      (match h.hp_state with
						   | HPS_DivingFake (_, reas) ->
							 (match reas with
							      | DiveTooLate ->
								    true
									
							      | _ -> false)
						   | HPS_GettingUpAfterDive (_, _, tooLate) ->
							 tooLate
						   | _ -> false)
					| _ -> false
				in

				if not noOneIsServing || not ballIsInPlay || pausedWithKey || pausedOnTheOtherSide then
				    match s.sc_state with
					| TieBreak points -> 
					      
					      (let w = whoServes s in
					       let tieStr =  string_of_int points.(w) ^ " " ^ string_of_int points.(1-w) in
					       let destX =  float_of_int windowWt -. d2 *. float_of_int (String.length tieStr) in
					       renderString09 tieStr destX 0.0 ;
					       if not shouldRenderTooLate  then 
						   renderString09 "6 6" 0.0 0.0
					       else
						   ())
						  
						  
					| NoTieBreak n ->
					      
					      (let w = whoServes s in
					       renderNumber n.points.(w)     (float_of_int windowWt -. d *. 2.4)     0.0;
					       renderNumber n.points.(1-w)   (float_of_int windowWt -. d *. 1.0)     0.0;
					       
					       if not shouldRenderTooLate  then
						   let scoreStr = 
						       (string_of_int (n.games.(0)) ^ " " ^ string_of_int (n.games.(1))) in
						   renderString09 scoreStr 0.0 0.0 
					       else
						   ())
				else
				    ();


				let renderTexture x0 y0 x1 y1 tex =
				    GlDraw.color ~alpha:1.0 (1.0 , 1.0, 1.0);

				    GlTex.bind_texture ~target:`texture_2d tex;
				    Gl.enable `texture_2d;
				    GlDraw.begins `triangle_fan;
				    

				    let verts =
					[  (x0 ,  y0, 0.0 , 0.0);
					   (x1,  y0, 1.0, 0.0);
					   (x1, y1, 1.0, 1.0);
					   (x0, y1, 0.0, 1.0) ] in

				    let foo (x, y, u, v) = 
					GlTex.coord2 (u, v) ;
					GlDraw.vertex2 ( x, y) in
				    
				    List.iter foo verts;
				    GlDraw.ends ();


				in
				if pausedWithKey && not doNotShowPause then
				    let wtPixmap = 196.0 
				    and htPixmap = 39.0 in
				    let wtScaled = wtPixmap 
				    and htScaled = htPixmap in
				    let winHt2 = float_of_int windowHt *. 0.5 in
				    let offsx = (float_of_int windowWt -. wtScaled) *. 0.5 in
				    let offsy = ( winHt2 -. htScaled) *. 0.5 in
				    renderTexture offsx offsy  (offsx +. wtScaled)  (offsy +. htScaled)
					(StringMap.find ( gfxDir ^ "/paused.png") handleOfTexture)

				else
				    ();

				if pausedOnTheOtherSide && not doNotShowPause then
				    let wtPixmap = 408.0 
				    and htPixmap = 39.0 in
				    let wtScaled = wtPixmap 
				    and htScaled = htPixmap in
				    let winHt2 = float_of_int windowHt *. 0.5 in
				    let offsx = (float_of_int windowWt -. wtScaled) *. 0.5 in
				    let offsy = winHt2 +. ( winHt2 -. htScaled) *. 0.5 in
				    renderTexture offsx offsy  (offsx +. wtScaled)  (offsy +. htScaled)
					(StringMap.find (gfxDir ^ "/paused-remote.png") handleOfTexture)
				else
				    ();

				
				if shouldRenderFault then 
				    let yOffs = 50.0 in
				    renderTexture 0.0 yOffs 86.0   (25.0 +. yOffs)
					(StringMap.find (gfxDir ^ "/fault.png") handleOfTexture)
				else
				    ();

				if shouldRenderTooLate then 
				    renderTexture 0.0 0.0 249.0   46.0
					(StringMap.find (gfxDir ^ "/too-late.png") handleOfTexture)
				else
				    ();


				let maybeRenderSprint p = 
				    match p with
					| HP h ->
					      let shouldRender = 
						  match serverData with
						      | NeitherServerNorClient -> true
						      | Server _ -> not h.hp_playsInTopmostCourtHalf
						      | Client _ -> h.hp_playsInTopmostCourtHalf
					      in
					      if shouldRender then
						  let wh3 = float_of_int windowHt /. 3.0 in
						  let maxRectHt = wh3 *. 0.8 in
						  let m = maxRectHt /. maxSprintCm in
						  let rectHt = maxRectHt -. m *. ( maxSprintCm -.  h.hp_fatigueData.fatigueAvailableSprintDistance) 
						  and rectWt = max 1.0 (4.0 *. float_of_int windowWt /. 1000.0) in
						  
						  renderTexture (float_of_int windowWt -. rectWt  ) (wh3 -. rectHt )
						      (float_of_int windowWt ) wh3
						      (StringMap.find (gfxDir ^ "/sprint-level.png") handleOfTexture)
					      else
						  ()
					| CP _ -> ()
				in
				Array.iter maybeRenderSprint players;


				GlMat.mode `projection;
				GlMat.load_identity ();
				GluMat.perspective ~fovy:fovY ~aspect:(float_of_int windowWt /. float_of_int windowHt) ~z:(zNear,  20000.0);
				GlMat.mode `modelview;
