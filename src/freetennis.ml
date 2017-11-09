(* 

   Copyright 2005 Maurizio Colucci.

   This file is part of Free Tennis.

   Free Tennis is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   Free Tennis is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Free Tennis; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)

open List

open Network

open Sound

open Input

open Math

open SharedData

open Camera

open Objects3D

open Animation

open BallMovement

open PlayerData

open Options

open Renderization

open Sdlevent (* for mme_xrel etc *)

exception TheAngleAlongXIsTooCloseToPi2
exception MistakeWithUncertainPhysicalMeaning
exception NotImplemented

let calcMinShotPower ~ballVelZ ~exploit = 
    (* determines how easy drop volley is. with 1.4 it is almost impossible *)
    abs_float ballVelZ *. exploit *. 1.3

(* Warning. Too low values can make the computer choose bad
   trajectories. The reason is that the computer chooses among a small
   number of possible heights over the net. If the impact is very
   close to the net, a small difference in height over the net can
   make a big difference in maximum shot height. So the computer
   produces lob-volleys *)
let minZIcanHitTheBall = 60.0 

(* "bool IntMap.t" is a map  int -> bool *)
module IntMap = Map.Make (struct
			      type t = int
			      let compare = compare
			  end )

type td2 = { 
		   t2_numFramesSinceLastFpsUpdate:int;
		   t2_timeOfLatestFpsCalculation:int;
		   t2_frameCountList:int list }

type td1 = { t1_numFramesSinceLastFpsUpdate:int;
	     t1_timeOfLatestFpsCalculation:int }

type timerData = TimerData0 | TimerData1 of td1 | TimerData2 of td2


let fpsRefreshRate = 200 (* milliseconds *)

(* calculate the number of seconds passed since last frame *)
let calcDt ~timer ~slowMotionFactor =
    match timer with
	| TimerData0 -> assert false
	| TimerData1 t -> 1.0 /. 300.0
	| TimerData2 t ->
	      let dt = 
		  let framesPassedInOneSecond = 
		      let  m = 1000.0 /. float_of_int fpsRefreshRate in
		      float_of_int (hd t.t2_frameCountList) *. m in
		  slowMotionFactor  /.  framesPassedInOneSecond
	      in
	      dt


let updateTimer  ~tim ~slowMotionFactor= 
    let curTime =  Sdltimer.get_ticks () in
    match tim with 
	| TimerData0 ->

	      TimerData1 {  
		  t1_numFramesSinceLastFpsUpdate = 0;
		  t1_timeOfLatestFpsCalculation = curTime}
	| TimerData1 td1 ->
	      let mFPS = 
		  if curTime -  td1.t1_timeOfLatestFpsCalculation >= fpsRefreshRate then
		      Some (td1.t1_numFramesSinceLastFpsUpdate + 1)
		  else
		      None
	      in
	      (match mFPS with
		   | None -> 
			 TimerData1 {  td1 with t1_numFramesSinceLastFpsUpdate = 
				 td1.t1_numFramesSinceLastFpsUpdate + 1}

		   | Some fps ->
			 TimerData2 {
			     t2_numFramesSinceLastFpsUpdate = 0;
			     t2_timeOfLatestFpsCalculation  = curTime;
			     t2_frameCountList =  [fps]}
	      )

	| TimerData2 t ->

	      let itIsTimeToUpdate = curTime - t.t2_timeOfLatestFpsCalculation >= fpsRefreshRate  in

	      if not itIsTimeToUpdate then
		  TimerData2 { t with
				   t2_numFramesSinceLastFpsUpdate = t.t2_numFramesSinceLastFpsUpdate + 1}
	      else
		  let newFrameCount = 
		      let idealFrameCount = t.t2_numFramesSinceLastFpsUpdate +1 
		      and averageFrameCount = float_of_int (List.fold_left (+) 0 t.t2_frameCountList) /.
			  float_of_int (length t.t2_frameCountList) in
		      if float_of_int idealFrameCount < 0.92 *. averageFrameCount then
			  int_of_float (0.92 *. averageFrameCount)
		      else
			  idealFrameCount
		  in
		  TimerData2 { 
		      t2_numFramesSinceLastFpsUpdate = 0;
		      
		      t2_timeOfLatestFpsCalculation  = curTime;

		      t2_frameCountList = 
			  let lengthOfListOfFrameCounts = 10 in
			  ( if length t.t2_frameCountList < lengthOfListOfFrameCounts then
				newFrameCount :: t.t2_frameCountList
			    else
				let rec truncate n l = 
				    if n <= 0 then
					[]
				    else
					match l with
					    | [] -> assert false
					    | h::t -> h::(truncate (n -1 ) t)
				in					

				truncate lengthOfListOfFrameCounts (newFrameCount:: t.t2_frameCountList)
			  )
		  }
		      

let loadTextureFromExistingGdkPixBuf ~name  ~pixbuf ~colorKey
	~handleOfTexture ~nextFreeTextureIndex ~textureHandles ~make64x64 = 
    
    let loadTextureFromExistingGlPix ~name ~glpix ~colorKey ~handleOfTexture
	    ~nextFreeTextureIndex ~textureHandles =
	if StringMap.mem name handleOfTexture then
	    begin
		print_endline ("texture already present: " ^ name);
		(handleOfTexture, nextFreeTextureIndex)
	    end
	else
	    let handleOfTexture' = StringMap.add name 
		textureHandles.(nextFreeTextureIndex) handleOfTexture in

	    GlTex.bind_texture ~target:`texture_2d   textureHandles.(nextFreeTextureIndex);

	    
	    GlTex.parameter     ~target:`texture_2d (`mag_filter `linear);
	    GlTex.parameter     ~target:`texture_2d (`min_filter `linear);
	    
	    GlTex.image2d glpix;

	    (handleOfTexture', nextFreeTextureIndex + 1 )

    in
    let glPixOfPixBuf ~pixbuf ~makeSquare64x64 = 
	let pixbuf2 = 
	    if not makeSquare64x64 then
		GdkPixbuf.create ~width:(GdkPixbuf.get_width pixbuf) ~height:(GdkPixbuf.get_height pixbuf)
		    ~has_alpha:true ()
	    else 
		GdkPixbuf.create ~width:64 ~height:64 ~has_alpha:true ()
		 
	in
	if makeSquare64x64 then
	    ( GdkPixbuf.fill pixbuf2 Int32.zero;
	      GdkPixbuf.composite pixbuf ~alpha:255 ~dest:pixbuf2 ~width:64 ~height:64 ~interp:`BILINEAR)
	else
	    ( GdkPixbuf.fill pixbuf2 Int32.zero;
	      GdkPixbuf.composite pixbuf ~alpha:255 ~dest:pixbuf2 ~width:(GdkPixbuf.get_width pixbuf) 
		  ~height:(GdkPixbuf.get_height pixbuf) ~interp:`BILINEAR);


	let src = GdkPixbuf.get_pixels pixbuf2 in
	let raw = Raw.create `ubyte ~len:(Gpointer.length src) 

	and region_of_raw raw =
	    Gpointer.unsafe_create_region ~path:[|1|] ~get_length:Raw.byte_size raw in
	Gpointer.blit ~src ~dst:(region_of_raw raw);
	GlPix.of_raw raw ~format:`rgba ~width:(GdkPixbuf.get_width pixbuf2)
	    ~height:(GdkPixbuf.get_height pixbuf2)
	    
    in
    loadTextureFromExistingGlPix ~name ~glpix:(glPixOfPixBuf ~pixbuf ~makeSquare64x64:make64x64)
	~colorKey ~handleOfTexture
	~nextFreeTextureIndex ~textureHandles



let loadTextureFromFile ~fileName ~colorKey ~handleOfTexture ~make64x64
	~nextFreeTextureIndex ~textureHandles =

    let pixbuf = GdkPixbuf.from_file fileName in
    loadTextureFromExistingGdkPixBuf ~name:fileName ~colorKey ~make64x64
	~pixbuf
	~handleOfTexture
	~nextFreeTextureIndex
	~textureHandles


let stringOfScore s  ~nextServiceIsFirst =
    
    let stringOfI i =
	if i = 0 then "0"
	else if i = 1 then "15"
	else if i = 2 then "30"
	else if i = 3 then "40"
	else if i =4 then "ADV"
	else assert(false) in

    match s.sc_state  with
	| TieBreak arr ->
	      "Tie Break: " ^ string_of_int arr.(0) ^ " - " ^ string_of_int arr.(1) ^ if nextServiceIsFirst then "" else "    Second service"
	| NoTieBreak n ->
	      "games: "  ^ string_of_int n.games.(0) ^ "-" ^ string_of_int n.games.(1)
	      ^"       " ^ stringOfI n.points.(0) ^ "-" ^ stringOfI n.points.(1)
	      ^ if nextServiceIsFirst then "" else "    Second service"
	      
    
    
    
    

let serviceIsToTheRight s = 
    let p1, p2 = 
	match s.sc_state with
	    | TieBreak a -> a.(0), a.(1)
	    | NoTieBreak a -> a.points.(0), a.points.(1)
    in
    (( p1 + p2) mod 2) = 0
	    

let calcMaxShotPower ~ballVelZ ~exploit ~search ~maxPow = 
    let po =
	match search with
	    | RKH_Dive _ | RKH_StretchForward _ -> maxPow *. powerAttenuationForStretchForwardAndDive
	    | RKH_Normal VOT_Volee -> maxPow *. powerAttenuationForVolee
	    | RKH_Normal VOT_NotVolee _ | RKH_Smash _ -> maxPow
    in
    po +. (abs_float ballVelZ) *. exploit 


let scoreIndex p = 
    match p with
	| HP h -> h.hp_scoreIndex
	| CP c -> c.cp_scoreIndex

let createRunningBall ~traj ~scoreIndexOfLastPlayerWhoHit ~polyBall
	~polyRedCross ~polyShadow ~service =


    assert(traj.impact.y3 > 0.0); 

    let bsm = 

	let bo =  whenWillItBounce traj in
	let ne =   whenWillTheTrajectoryHitTheNet traj  in

	{ bsm_trajectory = traj;
	  bsm_isItGoodSoFar = true;
	  bsm_curTimer = 0.0;
	  bsm_lastShotWasAService = service;
	  bsm_whenWillItBounce = bo;
	  bsm_whenWillHitTheNet = ne;
	  bsm_bouncesSoFar = 0;
	} 
    in
    
    { b_state = BS_Moving bsm;
      b_polygon = polyBall;
      b_redCrossPolygon = polyRedCross;
      b_shadowPolygon = polyShadow;

      b_siolpwhtb = scoreIndexOfLastPlayerWhoHit}
	



let intentionsDebug ~p = 
    if length p.cp_pointsWonAttacking > 0 ||
	length p.cp_pointsWonStayingBack  > 0
    then
	let wa = numOf p.cp_pointsWonAttacking (fun q -> q = Won) 
	and la = numOf p.cp_pointsWonAttacking (fun q -> q = Lost) in
	let ws = numOf p.cp_pointsWonStayingBack (fun q -> q = Won) 
	and ls = numOf p.cp_pointsWonStayingBack (fun q -> q = Lost) in
	(wa- la) - (  ws-  ls )
    else
	(0)

    

let shouldAttack ~p = 

    if length p.cp_pointsWonAttacking > 0 ||
	length p.cp_pointsWonStayingBack  > 0
    then
	let wa = numOf p.cp_pointsWonAttacking (fun q -> q = Won) 
	and la = numOf p.cp_pointsWonAttacking (fun q -> q = Lost) in
	let ws = numOf p.cp_pointsWonStayingBack (fun q -> q = Won) 
	and ls = numOf p.cp_pointsWonStayingBack (fun q -> q = Lost) in
	(* 	( print_endline ( "shouldAttack: wa = " ^ string_of_int wa ^ ", la  = " ^ string_of_int la ^ *)
	(* 	      ", ws = " ^ string_of_int ws ^ ", ls = " ^ string_of_int ls ); *)
	wa - la > ws - ls 
	    (* 	)  *)
    else
	  p.cp_pc.pc_prefersVolleysToGroundShots 


let updateMemoryOfPointsWonAndLost ~p ~won ~opponentCurPos = 

    let iAmUnderNet = AI.isAttacking (curPosOfComputerPlayer p) in
    if won then
	if iAmUnderNet then
	    Won::p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack
	else
	    p.cp_pointsWonAttacking, Won::p.cp_pointsWonStayingBack
    else
	if iAmUnderNet then
	    (* The reason I lost may be that I attacked, OR that I 
	       attacked too late, i.e. that I stayed back *)
	    if AI.isAttacking opponentCurPos then
		(* presumably the human has forced me to attack after him. Then,
		   the reason I lost is not that I attacked, but that I attacked too late! 
		   I.e. I lost a point staying back!
		*)
		p.cp_pointsWonAttacking, Lost::p.cp_pointsWonStayingBack
	    else
		Lost::p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack
	else
	    p.cp_pointsWonAttacking, Lost::p.cp_pointsWonStayingBack

let updateBall ~b ~dt ~score ~surf ~sounds ~nextServiceIsFirst ~opt ~players = 

    let letComputerKnowHeWon ~p ~siolpwhtb ~players= 
	match p with 
	    | CP c ->
		  if c.cp_scoreIndex = siolpwhtb then
		      let pwa, pws = 
			  let opponentCurPos = 
			      let mOpp = pick (Array.to_list players) (fun x -> scoreIndex x != c.cp_scoreIndex) in
			      match mOpp with
				  | None -> assert false
				  | Some opp -> curPosOfPlayer opp in
			  updateMemoryOfPointsWonAndLost ~p:c ~won:true ~opponentCurPos in
		      CP {c with cp_pointsWonAttacking = pwa;
			      cp_pointsWonStayingBack = pws}
		  else
		      p
	    | HP _ -> 
		  p
    in

    match b.b_state with
	| BS_Still _ -> 
	      score, b, nextServiceIsFirst, players

	| BS_Moving m ->
	      let newTimer = dt +. m.bsm_curTimer in

	      let b = 
		  let newm = { m with bsm_curTimer = newTimer} in
		  {b with b_state = BS_Moving newm} in
	      let incScore p s  = 
		  assert (p = 0 || p = 1);
		  let opponent x = 1-x in
		  let incPair p arr = 
		      if p = 0 then
			  [| arr.(0) +1 ; arr.(1) |]
		      else
			  [| arr.(0) ; arr.(1)+1 |]
		  in
		  match s.sc_state with

		      | TieBreak points -> 
			    let newPoints = incPair p points in
			    if newPoints.(p) >=7 && newPoints.(p) > newPoints.(opponent p) +1 then
				let newSets = (incPair p    [|6; 6 |]   )::s.sc_finishedSets in
				
				{ sc_state = NoTieBreak {points= [|0;0|]; games=[|0;0|]};
				  sc_finishedSets = newSets }
			    else
				{ s with sc_state =  TieBreak newPoints}

		      | NoTieBreak n ->

			    let rec normalize p = 
				if p.(0) > 3 && p.(1) > 3 then
				    normalize [| p.(0) -1 ;  p.(1) -1 |]
				else
				    p
			    in
			    

			    let newPoints = normalize (incPair p n.points) in
			    if newPoints.(p) > 3 && newPoints.(p) > newPoints.(opponent p)+1 then
				let newGames = incPair p n.games in
				if newGames.(p) >= 6 && newGames.(p) > newGames.(opponent p) + 1 then
				    { sc_state = NoTieBreak {points= [|0;0|]; games=[|0;0|]};
				      sc_finishedSets = newGames::s.sc_finishedSets }
				else if newGames.(p) = 6 && newGames.(opponent p) = 6 then
				    { s with sc_state = TieBreak  [|0;0|]}
				else
				    { s with sc_state = NoTieBreak {points= [|0;0|]; games=newGames}}
			    else
				{ s with sc_state = NoTieBreak { n with points= newPoints}}

	      in
	      let score, b, nextServiceIsFirst, newPlayers0  = 
		  if newTimer <= m.bsm_whenWillItBounce then
		      score, b, nextServiceIsFirst, players
		  else
		      let bounceOnGround ~b ~s ~score ~surf ~nextServiceIsFirst = 
			  let newBounceCount = s.bsm_bouncesSoFar +1 in 
			  
			  let (newScore, isGoodSoFar, nextServiceIsFirst, newPlayers) = 
			      if newBounceCount = 1 then
				  match s.bsm_trajectory.targetRect with
				      | None ->
					    score, false, true, players
				      | _ ->

					    let isIn = theTrajectoryFallsInTheTargetRect s.bsm_trajectory in
					    if isIn then
						( score, true, true, players)
					    else
						( playSoundId ~sounds ~id:SoundFault;
						  
						  if s.bsm_lastShotWasAService && nextServiceIsFirst then
						      (score, false, false, players)
						  else
						      let s' = incScore (1-b.b_siolpwhtb) score in

						      s' ,false, true , players
						)
						    
			      else if newBounceCount = 2 then
				  if s.bsm_isItGoodSoFar then
				      let s' = incScore b.b_siolpwhtb score  in
				      let pl' = 
(* 					  print_endline ("bounceOnGround: inform computer he has won"); *)
					  
					  Array.map (fun p -> letComputerKnowHeWon ~p
							 ~siolpwhtb:b.b_siolpwhtb ~players)
					      players
				      in
				      ( s' ,false, true, pl')
				  else
				      (score, false, nextServiceIsFirst, players)
			      else
				  ( score, false, nextServiceIsFirst, players)

			  in
			  let newTraj = 
			      let curV = curBallVel s in
			      let whereBounce = whereWillItBounce s.bsm_trajectory in

			      { impact = vec3dCreate whereBounce.x2 0.0 whereBounce.z2 ;
				startVel = vec3dCreate (curV.x3 /. surf.s_velXZAttenuationFactor)
				      (-.(curV.y3 /. surf.s_velYAttenuationFactor) )
				      (curV.z3 /. surf.s_velXZAttenuationFactor) ;
				spin = vec3dCreate (s.bsm_trajectory.spin.x3 /. surf.s_spinAttenuationFactor)
				      (s.bsm_trajectory.spin.y3 /. surf.s_spinAttenuationFactor)
				      (s.bsm_trajectory.spin.z3 /. surf.s_spinAttenuationFactor);
				targetRect = None   }
			  in
			  
			  let newState =   
			      if newTraj.startVel.y3 < 120.0 then
				  BS_Still newTraj.impact
			      else
				  let bo = whenWillItBounce newTraj in
				  BS_Moving { bsm_trajectory = newTraj;
					      bsm_isItGoodSoFar = isGoodSoFar;
					      bsm_curTimer = 0.0;
					      bsm_whenWillItBounce = bo;
					      bsm_lastShotWasAService = m.bsm_lastShotWasAService;
					      bsm_whenWillHitTheNet = whenWillTheTrajectoryHitTheNet newTraj;
					      bsm_bouncesSoFar = newBounceCount}
			  in


			  (newScore, { b with b_state = newState }, nextServiceIsFirst, newPlayers)
			      
		      in (* end bounceOnGround *)

		      playSoundId  ~sounds ~id:SoundBounce;
		      bounceOnGround ~b ~s:m ~score ~surf ~nextServiceIsFirst
	      in

	      match b.b_state with 
		  | BS_Still _ -> score, b, nextServiceIsFirst, newPlayers0

		  | BS_Moving m ->

			let bounceAgainstNetOrWall ~b ~z ~score ~s ~nextServiceIsFirst ~players = 
			    let score', nextServiceIsFirst, newPlayers = 
				if s.bsm_bouncesSoFar = 0 then
				    if nextServiceIsFirst && s.bsm_lastShotWasAService then
					score, false, players
				    else
					let s' = incScore (1 - b.b_siolpwhtb) score in
					s', true, players
				else if s.bsm_bouncesSoFar = 1 && s.bsm_isItGoodSoFar then
				    let s' = incScore b.b_siolpwhtb score in
				    let pl' = 
(* 					print_endline ("bounceAgainstNetOrWall: inform computer he has won"); *)
					Array.map (fun p -> letComputerKnowHeWon ~p
						       ~siolpwhtb:b.b_siolpwhtb ~players)
					    players

				    in
				    s', true, pl'
				else
				    score, nextServiceIsFirst, players
			    in

			    let curV = curBallVel s in
			    let newVel = vec3dCreate (curV.x3 /. 8.0) (curV.y3 /. 4.0) (-. curV.z3
											/. 8.0) in

			    let curP = curBallPos b in
			    let newImpact = 
				if newVel.z3 > 0.0 then
				    vec3dCreate curP.x3 curP.y3 (z +. 2.0)
				else
				    vec3dCreate curP.x3 curP.y3 (z -. 2.0) in
			    
			    let newTraj = { impact = newImpact;
					    startVel = newVel;
					    spin = vec3dNull ;
					    targetRect = None
					  } in
			    let s' = { bsm_bouncesSoFar = s.bsm_bouncesSoFar +1 ;
				       bsm_isItGoodSoFar = false;
				       bsm_trajectory = newTraj;
				       bsm_lastShotWasAService = s.bsm_lastShotWasAService;
				       bsm_curTimer = 0.0;
				       bsm_whenWillItBounce = whenWillItBounce newTraj;
				       bsm_whenWillHitTheNet = whenWillTheTrajectoryHitTheNet newTraj } in

			    let b' = { b with  b_state= BS_Moving s' } in
			    (b', score', nextServiceIsFirst, newPlayers)
				
			in

			let b, score, nextServiceIsFirst, newPlayers0 = 
			    match m.bsm_whenWillHitTheNet with
				| None ->
				      b, score, nextServiceIsFirst, newPlayers0
				| Some hnr ->
				      if newTimer > hnr.hnr_t then
					  (playSoundId ~sounds ~id:SoundHitNet;

					   bounceAgainstNetOrWall ~b ~z:0.0 ~score ~s:m ~nextServiceIsFirst ~players:newPlayers0)
				      else
					  b, score, nextServiceIsFirst, newPlayers0
			in
			match b.b_state with 
			    | BS_Still _ -> 
				  score, b, nextServiceIsFirst, newPlayers0
				      
			    | BS_Moving m ->

				  let curP = curBallPos b in

				  let b, score , nextServiceIsFirst, newPlayers0 = 
				      if curP.z3 < upperBound then
					  (playSoundId ~sounds ~id:SoundHitBorder;
					   bounceAgainstNetOrWall ~b ~z:upperBound ~score ~s:m ~nextServiceIsFirst ~players:newPlayers0)
					      
				      else
					  b, score, nextServiceIsFirst, newPlayers0
				  in
				  match b.b_state with 
				      | BS_Still _ -> score, b, nextServiceIsFirst, newPlayers0
					    
				      | BS_Moving m ->

					    let b, score, nextServiceIsFirst, newPlayers0 = 
						let curP = curBallPos b in
						if curP.z3 > lowerBound then
						    (playSoundId ~sounds ~id:SoundHitBorder;
						     bounceAgainstNetOrWall ~b ~z:lowerBound ~score ~s:m ~nextServiceIsFirst ~players:newPlayers0)
						else
						    b, score, nextServiceIsFirst, newPlayers0
					    in

					    (score, b, nextServiceIsFirst, newPlayers0)
						

let startServiceHuman ~scoreIsEven ~h ~serverData = 

    let state =
	let pos = 
	    let dirsign = if h.hp_playsInTopmostCourtHalf then -. 1.0 else
		1.0 in
	    let posx = if scoreIsEven then 160.0 *. dirsign else -. 160.0 *. dirsign in
	    vec2dCreate posx (dirsign *. (courtHt2 +. 50.0)) in
	HPS_ServingBeforeLaunch (scoreIsEven, pos) in
    let objLeading = 
	let prefix = 
	    match serverData with
		| Client _ -> gfxDir ^ "/B"
		| Server _ -> gfxDir ^ "/B"
		| NeitherServerNorClient ->
		      if h.hp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B" in
	setAnim  ~o:h.hp_objLeading ~animName:(prefix ^ "servizio") ~restartIfSameAnimation:true in
    let objSlave = 
	let prefix = 
	    match serverData with
		| Client _ -> 
		      gfxDir ^ "/A"
		| Server _ ->
		      gfxDir ^ "/A"
		| NeitherServerNorClient ->
		      if h.hp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B" in
	setAnim  ~o:h.hp_objSlave ~animName:(prefix ^ "servizio") ~restartIfSameAnimation:true in
    assert(match objLeading.o3d_animState  with Animated _  -> true
	       | NotAnimated | PausedDuringService -> false);
    (state,  objLeading, objSlave)


	    


let startServiceComputer ~scoreIsEven ~h = 
    
    let dirsign = if h.cp_playsInTopmostCourtHalf then -. 1.0 else 1.0 in


    let state = CPS_ServingBeforeLaunch scoreIsEven in
    let obj = 
	let prefix = if h.cp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B" in
	setAnim  ~o:h.cp_obj ~animName:(prefix ^ "servizio") ~restartIfSameAnimation:true in
    assert(match obj.o3d_animState  with Animated _  -> true | NotAnimated | PausedDuringService -> false);
    let umd =
	let pos = 
	    let posx = if scoreIsEven then 100.0 *. dirsign else -. 100.0 *. dirsign in
	    vec2dCreate posx (dirsign *. (courtHt2 +. 50.0)) in

	{umd_timer = 0.0;
	 umd_startVel = vec2dCreate 0.0 0.0;
	 umd_startPos = pos}in

    (state,  obj, umd)


type decreaseLenResult = DLR_ErrorInsufficientPowerToSurpassNet
			 | DLR_Ok of float | DLR_ErrorCountReachedZero
	
let updateHumanPlayer  ~p  ~dt ~b ~opponentCurPos  ~mouse ~mouseSensitivity ~surf  ~opt ~serverData  =
    
    let dirsign = if p.hp_playsInTopmostCourtHalf then -. 1.0 else 1.0 

    and mouse2 = { mouse with m_secondsSinceLastMouseMotion = mouse.m_secondsSinceLastMouseMotion +. dt} in

    
    let findNeededPower ~impact ~htOverNet ~spin ~bounceDesired = 
	assert (htOverNet > 0.0);
	assert ( bounceDesired.z2 *. impact.z3 < 0.0);
	assert  (spin >= 0.0 || abs_float spin < abs_float g );

	
	let flipxz3 a = vec3dCreate  (-. a.x3) a.y3  (  -. a.z3)  in

	let playingAbove = impact.z3 < bounceDesired.z2 in
	(* i calcoli assumono q0 positivo e qr negativo, quindi sono
	   OBBLIGATO a flippare se gioca in alto! *)
	let impact = if playingAbove then  flipxz3 impact else
	    impact 
	and bounceDesired = if playingAbove then  flipxz2 bounceDesired else
	    bounceDesired in

	let impact2d = projection2d impact in 
	let pointOverNet =
	    let netLine = {sl_a = 0.0 ; sl_b = 1.0; sl_c = 0.0} 
	    and shotDir = straightLineBetween impact2d bounceDesired in 
	    intersectionOfStraightLines  netLine shotDir in
	let q0 = distance2d impact2d pointOverNet (* cosi' rendo q0 positivo, quindi mi restringo al caso in cui gioca in basso *)  in
	let qr = -. distance2d pointOverNet bounceDesired (* idem qui, che rendo qr negativo *) in
	let y0 = impact.y3 in
	let yn = htOverNet in
	let qr2 = qr *. qr in
	let q02 = q0 *. q0 in
	let g = abs_float g in
	let s2 = spin *. 0.5 in
	let s1 = (spin +. g )*. 0.5 in
	let q1 = q0 -. qr in
	let y1 = y0 -. yn in

	let denom = ( -. s2 *. y0 +. s1  *. q1 ) *. 
	    ( s1 *. q1 -. s1 *. q0+. s2 *. y1 -. s2*. y0 ) in
	assert (denom != 0.0);

	let discr = -. ( ( -. s2 *. y1  +. s1 *. q0 ) *.
			     ( -. y0 *. q0 +. y1 *. q1 ) ) 
	    /. denom in
	if discr  < 0.0 then
	    None
	else
	    let tn = sqrt discr in
	    assert  (tn != 0.0) ; 
	    let vq = ( s2 *. tn *. tn -. q0 ) /. tn in
	    let vy = ( s1 *. tn *. tn -. y1 ) /. tn in
	    let v0 = vec2dCreate vq vy in
	    let modv0 = length2d v0 in
	    assert (classify_float modv0 != FP_nan);
	    Some modv0
    in (* findNeededPower *)

    let theShotIsNotTooVertical ~impact ~aim ~ht ~spin  = 
	let may = buildTrajFromTwoPointsAndHeight ~impact ~htOverNet:ht
	    ~spin ~bounceDesired:aim ~targetRect:None in
	match may with
	    | None -> None (* traj impossible *)
	    | Some tr ->
		  let vertical = vec3dCreate 0.0 1.0 0.0 in
		  let ang =
		      smallestAngleBetween3d vertical tr.startVel 
		  and minAngle = 
		      if spin < -. abs_float (2.0 *. spinForVolee )
			  (* it is a backspin ground stroke *)
		      then
			  75.0 (* prevent dropshot from being too easy *)
		      else
			  
			  (* with 40.0, drop volees are totally impossible *)
			  40.0 in
		  Some (ang > degToRad minAngle)
    in



    let doAllTests ~impact ~aim ~ht ~spin ~reject
	    ~minPowerAvailable ~maxPowerAvailable  =

	let maybeVert = theShotIsNotTooVertical ~impact ~aim ~ht ~spin 
	in
	match maybeVert with
	    | None -> (* impossible traj *)
		  ( match reject with
			| None -> assert false
			| Some r -> r)
	    | Some isNotTooVert ->
		  if isNotTooVert then
		      
		      let maybePow = findNeededPower ~impact ~htOverNet:ht
			  ~spin ~bounceDesired:aim in
		      match maybePow with
			  | None -> assert false
			  | Some pow ->
				if pow > maxPowerAvailable then
				    ( print_endline "rejected at 2nd level because too powerful";
				      ( match reject with
					    | None -> assert false
					    | Some r -> r ))
				else if pow < minPowerAvailable then
				    ( print_endline "rejected at 2nd level because too weak";
				      ( match reject with
					    | None -> (* @@ happened playing volee with mats against ivan ... *) assert false
					    | Some r -> r ))
				else
				    (aim, ht)
		  else
		      (print_endline "rejected at 2nd level because too vert";
		       ( match reject with
			     | None -> assert false
			     | Some r -> r ))


    in



    let rec shortestBounceSuchThatPowerIsNotTooLittle ~bou ~count ~minPowerAvailable ~impact 
	    ~htOverNet ~spin = 
	
	if count = 0 then
	    None 
	else
	    let maybePow =  findNeededPower ~impact ~htOverNet
		~spin ~bounceDesired:bou in
	    match maybePow with
		| None -> None
		| Some pow ->
		      if pow >= minPowerAvailable then
			  Some bou
		      else
			  let newCandidateBounceLonger = 
			      let deltaXZ = 
				  vec2dMulScalar 10.0 (normalize2d (vec2dSub bou (projection2d impact))) in
			      vec2dAdd bou deltaXZ in
			  shortestBounceSuchThatPowerIsNotTooLittle 
			      ~bou:newCandidateBounceLonger ~count:(count -1) 
			      ~minPowerAvailable ~impact ~htOverNet ~spin
    in
    let rec shortestBounceSuchThatAngleIsNotTooVert ~bou ~count ~impact 
	    ~htOverNet ~spin = 
	
	if count = 0 then
	    None 
	else
	    let maybeNotTooVert = theShotIsNotTooVertical ~impact ~ht:htOverNet
		~spin ~aim:bou  in
	    match maybeNotTooVert with
		| None -> None
		| Some isNotTooVert ->
		      if isNotTooVert then
			  Some bou
		      else
			  let newCandidateBounceLonger = 
			      let deltaXZ = 
				  vec2dMulScalar 10.0 (normalize2d (vec2dSub bou (projection2d impact))) in
			      vec2dAdd bou deltaXZ  in
			  shortestBounceSuchThatAngleIsNotTooVert  
			      ~bou:newCandidateBounceLonger ~count:(count -1) 
			      ~impact ~htOverNet ~spin
    in
    let rec longestBounceSuchThatPowerIsNotTooMuch 
	    ~bou ~count ~maxPowerAvailable
	    ~impact ~htOverNet ~spin = 
	
	if count = 0 then
	    None 
	else
	    let maybePow =  findNeededPower ~impact ~htOverNet
		~spin ~bounceDesired:bou in
	    match maybePow with
		| None -> None
		| Some pow ->
		      if pow <= maxPowerAvailable then
			  Some bou
		      else
			  let newCandidateBounceShorter = 
			      let deltaXZ = 
				  vec2dMulScalar 10.0 (normalize2d (vec2dSub (projection2d impact) bou)) in
			      vec2dAdd bou deltaXZ in
			  if newCandidateBounceShorter.z2 *. impact.z3 > 0.0 then
			      None
			  else
			      longestBounceSuchThatPowerIsNotTooMuch 
				  ~bou:newCandidateBounceShorter ~count:(count -1) 
				  ~maxPowerAvailable ~impact ~htOverNet ~spin
    in

    
    let rec highestHtSuchThatTheAngleIsNotVertAndPowerIsNotTooSmall ~ht ~count ~aim 
	    ~impact ~spin ~minPowerAvailable  = 

	if count = 0 then
	    None 
	else
	    let maybeVert = theShotIsNotTooVertical ~impact ~aim ~ht
		~spin  in

	    match maybeVert with
		| None -> None (* impossible to decrease ht keeping the traj possible *)
		| Some isNotTooVert ->
		      if isNotTooVert then
			  let maybePow = findNeededPower ~impact ~htOverNet:ht ~spin ~bounceDesired:aim in
			  match maybePow with
			      | None -> None
			      | Some pow ->
				    if pow > minPowerAvailable then
					Some ht
				    else
					highestHtSuchThatTheAngleIsNotVertAndPowerIsNotTooSmall ~ht:(ht -. 1.0) 
					    ~count:(count -1 ) ~aim ~spin ~impact ~minPowerAvailable
					    
		      else
			  highestHtSuchThatTheAngleIsNotVertAndPowerIsNotTooSmall ~ht:(ht -. 1.0) 
			      ~count:(count -1 ) ~aim ~spin ~impact ~minPowerAvailable
			      
    in

    let rec highestHtSuchThatTheAngleIsNotTooSmall ~ht ~count ~aim ~impact ~spin = 
	if count = 0 then
	    None 
	else
	    let maybeVert = theShotIsNotTooVertical 
		~impact ~aim ~ht ~spin in
	    match maybeVert with
		| None -> None (* impossible to decrease ht keeping the traj possible *)
		| Some isNotTooVert ->
		      if isNotTooVert then
			  Some ht
		      else
			  highestHtSuchThatTheAngleIsNotTooSmall ~ht:(ht -. 1.0) 
			      ~count:(count -1 ) ~aim ~spin ~impact 
			      
    in

    let rec lowestHtSuchThatThePowerIsNotTooMuchAndTheParabolaIsPossible ~ht ~count ~impact ~desiredAim
	    ~spin ~maxPowerAvailable =
	if count = 0 then
	    None (* happens on dive *)
	else
	    let maybePow =  findNeededPower ~impact ~htOverNet:ht ~spin ~bounceDesired:desiredAim in
	    match maybePow with
		| None -> 
		      None
		| Some pow ->
		      if pow <= maxPowerAvailable then
			  Some ht
		      else
			  lowestHtSuchThatThePowerIsNotTooMuchAndTheParabolaIsPossible
			      ~ht:(ht +. 1.0) ~count:(count -1)
			      ~spin ~impact ~desiredAim ~maxPowerAvailable

    in
    let rec lowestHtSuchThatTheParabolaIsPossible ~ht ~count ~impact ~desiredAim ~spin  =
	if count = 0 then
	    None (* happens on dive *)
	else
	    let maybePow =  findNeededPower ~impact ~htOverNet:ht ~spin ~bounceDesired:desiredAim in
	    match maybePow with
		| None -> 
		      lowestHtSuchThatTheParabolaIsPossible
			  ~ht:(ht +. 1.0) ~count:(count -1)
			  ~spin ~impact ~desiredAim 
		| Some _ -> (* parabola possible *)
		      Some ht

    in


    let createStateASBO ~p ~r  =

	
	let minPowerAvailable = calcMinShotPower ~ballVelZ:r.brh_ballVelAtImpact.z3
	    ~exploit:p.hp_pc.pc_exploitationOfOpponentsPower
	    
	and maxPowerAvailable = calcMaxShotPower ~search:r.brh_researchKind 
	    ~maxPow:p.hp_pc.pc_maxShotPower ~exploit:p.hp_pc.pc_exploitationOfOpponentsPower
	    ~ballVelZ:r.brh_ballVelAtImpact.z3
	in

	let mAimAndHt = 


	    assert ( p.hp_playsInTopmostCourtHalf = (r.brh_impact.z3 < 0.0));



	    let spin = spinOfResearchKind ~p ~r:r.brh_researchKind  in
	    let idealAim = 
		let idealZAim = 
		    if not p.hp_playsInTopmostCourtHalf then
			(-. courtHt4 -. 300.0) 
		    else
			courtHt4 +. 300.0 in
		let xAim = r.brh_impact.x3 in
		vec2dCreate xAim idealZAim in

	    (* what can be wrong now? Everything: the parabola can be
	       impossible (e.g. impact high, low htOverNet, long
	       bounce); the needed power can be too small or too big;
	       the parabola angle with the y axis can be too small
	       (too vertical). *)

	    let maybeHigherHt = 


		let idealHtOverNet = 
		    match r.brh_researchKind with
			| RKH_Dive _ | RKH_Smash _ -> p.hp_startHtOverNetForVolleys
			| RKH_StretchForward  _ -> p.hp_startHtOverNetForTopSpinGround
			| RKH_Normal vt ->
			      (match vt with
				   | VOT_Volee ->
					 p.hp_startHtOverNetForVolleys 
				   | VOT_NotVolee topSpin ->
					 match topSpin with
					     | Topspin -> p.hp_startHtOverNetForTopSpinGround 
					     | Backspin -> p.hp_startHtOverNetForBackSpinGround )

		in


		lowestHtSuchThatTheParabolaIsPossible ~ht:idealHtOverNet ~count:1000
		    ~impact:r.brh_impact ~desiredAim:idealAim ~spin in
	    match maybeHigherHt with 
		| None -> None
		| Some higherHt ->

		      (* the traj is now possible. It may still be that
		         the needed power is too little (drop volley), 
			 too much (stretch near the ground line), 
			 or that the angle is too vertical (ivan volley).

			 If the needed power is too much, try to make the shot shorter.

			 If the needed power is too little, try to decrease the ht, 
			 checking that the parabola does not become impossible again.

			 If the angle is too vertical, make the shot longer.
		      *)

		      let angleTooVertical = 
			  let mNTV = theShotIsNotTooVertical ~impact:r.brh_impact ~aim:idealAim ~ht:higherHt ~spin
			  in
			  match mNTV with
			      | None -> assert false (* we know it's not impossible *)
			      | Some notTooVert -> not notTooVert 

		      and neededPowerTooLittle, neededPowerTooMuch =
			  let mP = findNeededPower ~impact:r.brh_impact 
			      ~spin ~htOverNet:higherHt ~bounceDesired:idealAim in
			  match mP with
			      | None -> assert false (* we know it's not impossible *)
			      | Some pow -> (pow  < minPowerAvailable, pow > maxPowerAvailable)
		      in

		      if angleTooVertical then
			  let mLongerBounce = shortestBounceSuchThatAngleIsNotTooVert
			      ~bou:idealAim ~count:1000
			      ~impact:r.brh_impact ~spin ~htOverNet:higherHt in
			  match mLongerBounce with
			      | None -> None (* @@ happened once. ivan at net *) 
			      | Some longerBounce ->
				    Some (doAllTests ~impact:r.brh_impact ~spin ~ht:higherHt ~aim:longerBounce ~minPowerAvailable
					      ~maxPowerAvailable ~reject:None )
		      else if neededPowerTooLittle then
			  let mLowerHt = highestHtSuchThatTheAngleIsNotVertAndPowerIsNotTooSmall
			      ~ht:higherHt ~count:1000 ~aim:idealAim ~impact:r.brh_impact ~spin
			      ~minPowerAvailable   in
			  match mLowerHt with
			      | None -> None
			      | Some lowerHt ->
				    Some (doAllTests ~impact:r.brh_impact ~spin ~ht:lowerHt ~aim:idealAim ~minPowerAvailable
					      ~maxPowerAvailable ~reject:None  )
		      else if neededPowerTooMuch then
			  let mShorterBounce = longestBounceSuchThatPowerIsNotTooMuch ~bou:idealAim ~count:1000
			      ~impact:r.brh_impact ~spin ~htOverNet:higherHt ~maxPowerAvailable in
			  match mShorterBounce with
			      | None -> None
			      | Some shorterBounce ->
				    Some (doAllTests ~impact:r.brh_impact ~spin ~ht:higherHt ~aim:shorterBounce ~minPowerAvailable
					      ~maxPowerAvailable ~reject:None)
		      else
			  (* no need to do 2nd level check *)
			  Some ( idealAim, higherHt) 
			      


			      
	in
	match mAimAndHt with
	    | None -> None

	    | Some ( aim, htOverNet ) ->
		  assert ( (aim.z2 > 0.0) = p.hp_playsInTopmostCourtHalf);
		  assert ( aim.z2 *. r.brh_impact.z3 < 0.0);
		  Some
		      { asbo_TimeToRunBeforeOpening = r.brh_tChange -. r.brh_t0;
			asbo_RunSpeedBeforeOpening = r.brh_runSpeedBeforeOpening;
			asbo_BallVelAtImpact = r.brh_ballVelAtImpact;
			asbo_HtOverNet = htOverNet;
			asbo_Impact = r.brh_impact;
			asbo_FootTarget = r.brh_footTarget;
			asbo_ModulusOfRunSpeedAtImpactTime = r.brh_modulusOfRunSpeedAtImpactTime;
			asbo_CurAim = aim;
			asbo_UniformMotionData = 
			      { umd_startPos =(curPosOfHumanPlayer p);
				umd_startVel = r.brh_runSpeedBeforeOpening;
				umd_timer = 0.0 };
			asbo_TimeToRunFromOpeningToImpact = r.brh_t1 -.  r.brh_tChange;
			asbo_RunSpeedFromOpeningToImpact = r.brh_runSpeedAfterOpening;
			asbo_Forehand = r.brh_forehand;
			asbo_researchKind = r.brh_researchKind;
			
		      } 
    in (* end createStateASBO *)
    
    

    let updateHtOverNetByReadingInput ~p ~dt ~htOverNet ~impact ~spin ~bounceDesired 
	    ~mouse ~minPowerAvailable ~maxPowerAvailable  =
	assert(impact.z3 *. bounceDesired.z2 < 0.0);
	let stepSmall = 2.5 in
	let stepMid = 5.0 in
	let stepBig =  8.0 (*  10.0 is too easy to do lob *) in
	let up = mouse.m_rightButtonPressed in
	let down = mouse.m_leftButtonPressed in
	let step =
	    if up then
		stepBig
	    else if htOverNet < 160.0 then
		stepSmall 
	    else if htOverNet < 300.0 then
		stepMid 
	    else stepBig in
	let rejected = ( bounceDesired, htOverNet) in
	if up then 
	    (* if up, 3 things can happen: the angle can become too vertical,
	       the power can become too much, the power can become too little *)
	    let desiredHt = htOverNet +. step *. dt  *. 55.0 in
	    let maybeVer = theShotIsNotTooVertical ~impact ~aim:bounceDesired  
		~spin ~ht:desiredHt in
	    match maybeVer with
		| None -> rejected (* reject impossible parabola *)
		| Some isNotTooVer ->
		      if isNotTooVer then
			  (* increasing the height, the power may have become too much OR too little *)
			  
			  let maybePow = findNeededPower ~impact ~htOverNet:desiredHt ~spin ~bounceDesired in
			  match maybePow with
			      | None -> assert false (* we already know it is not impossible *)
			      | Some pow ->
				    if pow > maxPowerAvailable then
					let mBounceShorter = longestBounceSuchThatPowerIsNotTooMuch 
					    ~bou:bounceDesired ~count:1000 ~htOverNet:desiredHt ~spin 
					    ~maxPowerAvailable ~impact in
					match mBounceShorter with
					    | None -> rejected
					    | Some bounceShorter -> 

						  doAllTests ~impact ~spin
						      ~ht:desiredHt ~aim:bounceShorter
						      ~minPowerAvailable ~maxPowerAvailable
						      ~reject:(Some rejected) 
						      
				    else if pow < minPowerAvailable then
					let mBounceLonger = shortestBounceSuchThatPowerIsNotTooLittle 
					    ~bou:bounceDesired ~count:1000 ~htOverNet:desiredHt ~spin 
					    ~minPowerAvailable ~impact in
					match mBounceLonger with
					    | None -> rejected
					    | Some bounceLonger ->

						  doAllTests ~impact ~spin 
						      ~ht:desiredHt ~aim:bounceLonger ~minPowerAvailable ~maxPowerAvailable
						      ~reject:(Some rejected)
				    else
					doAllTests ~impact ~spin 
					    ~ht:desiredHt ~aim:bounceDesired ~minPowerAvailable ~maxPowerAvailable
					    ~reject:(Some rejected)
		      else
			  let mBounceLonger = shortestBounceSuchThatAngleIsNotTooVert ~bou:bounceDesired
			      ~impact ~spin ~count:1000 ~htOverNet:desiredHt  in
			  match mBounceLonger with
			      | None -> rejected
			      | Some bounceLonger ->
				    doAllTests ~impact ~spin ~aim:bounceLonger ~ht:desiredHt 
					~reject:(Some rejected) 
					~minPowerAvailable ~maxPowerAvailable 

	else if down then
	    (* what can happen when I push down? THe parabola can 
	       become impossible, and the needed power can become too much *)
	    let desiredHt = htOverNet -. step *. dt *.  55.0 in
	    if desiredHt < 10.0 then
		rejected
	    else
		let necessaryPower = findNeededPower ~impact
		    ~htOverNet:desiredHt ~spin ~bounceDesired in
		match necessaryPower with
		    | None -> rejected (* reject impossible parabola *)
		    | Some pow ->
			  (* now the only thing that can happen is for the shot to be too 
			     powerful. Shorten the shot until it is not so *)
			  let mShorterBounce = longestBounceSuchThatPowerIsNotTooMuch ~bou:bounceDesired
			      ~count:1000 ~maxPowerAvailable ~impact ~spin ~htOverNet:desiredHt
			  in
			  match mShorterBounce with
			      | None -> rejected
			      | Some shorterBounce ->
				    doAllTests ~impact ~spin ~aim:shorterBounce ~ht:desiredHt
					
					~reject:(Some rejected) ~minPowerAvailable ~maxPowerAvailable 
	else
	    rejected

    in	    (* end updateHtOverNetByReadingInput *)



    let updateAimByReadingInput ~p ~dt ~idealHtOverNet ~impact ~spin ~maxPowerAvailable
	    ~curAim ~mouse ~mouseSensitivity ~iCanIncreaseTheHt ~minPowerAvailable 
	    = 

	assert(curAim.z2 *. impact.z3 < 0.0);
	let remoteSign = 
	    match serverData with 
		| Client _ -> -. 1.0
		| Server _ | NeitherServerNorClient -> 1.0 
	in
	let dx = remoteSign *. float_of_int mouse.m_xRel *. dt *.  mouseSensitivity in
	let dz = remoteSign *. float_of_int mouse.m_yRel *. dt *.  mouseSensitivity in
	let updateRejected = (curAim, idealHtOverNet) in
	if dx = 0.0 && dz = 0.0 || mouse.m_secondsSinceLastMouseMotion > mouseRefresh then
	    updateRejected
	else
	    let desiredAim = vec2dAdd curAim (vec2dCreate dx dz) in
	    if desiredAim.z2 *. impact.z3 >= 0.0 then
		updateRejected
	    else
		let minDistanceOfBounceFromNet = 
		    (* this influences how much it is possible to do
		       backspin that bounces backwards  *)
		    42.0 in
		if p.hp_playsInTopmostCourtHalf && desiredAim.z2 < minDistanceOfBounceFromNet then
		    updateRejected
		else if (not p.hp_playsInTopmostCourtHalf) && desiredAim.z2 > -. minDistanceOfBounceFromNet then
		    updateRejected
		else
		    let theShotWasShortened =
			let imp = projection2d impact in
			length2d (vec2dSub curAim imp) > length2d (vec2dSub desiredAim imp) in

		    if theShotWasShortened then
			(* the needed power may have become too much or too little; the angle may have
			   become too vertical *)
			let maybePow = findNeededPower ~impact ~htOverNet:idealHtOverNet 
			    ~spin ~bounceDesired:desiredAim in
			match maybePow with
			    | None -> assert false (* the parabola should not become impossible in this case *)
			    | Some pow ->
				  if pow > maxPowerAvailable then
				      updateRejected (* sic! We could also decrease the ht *)
				  else 
				      let mNewHt = highestHtSuchThatTheAngleIsNotVertAndPowerIsNotTooSmall
					  ~impact ~aim:desiredAim ~spin ~count:1000 ~minPowerAvailable 
					  ~ht:idealHtOverNet  in
				      match mNewHt with
					  | None -> updateRejected
					  | Some newHt ->
						doAllTests ~impact ~spin ~maxPowerAvailable ~minPowerAvailable
						    ~ht:newHt ~aim:desiredAim ~reject:(Some updateRejected)
						    
						    
						    
						    
		    else
			(* the needed power might have become too much; the parabola may have become
			   impossible *)	
			let maybeNewHt = lowestHtSuchThatThePowerIsNotTooMuchAndTheParabolaIsPossible
			    ~impact ~spin ~count:1000 ~maxPowerAvailable ~desiredAim ~ht:idealHtOverNet in
			match maybeNewHt with
			    | None -> updateRejected
			    | Some newHt ->
				  doAllTests ~impact ~spin ~maxPowerAvailable ~minPowerAvailable
				      ~ht:newHt ~aim:desiredAim ~reject:(Some updateRejected) 
				      
    in (* end updateAimByReadingInput *)
    

    let p = 
	let newObjLead = updateAnim p.hp_objLeading dt in
	let newObjSlave = updateAnim p.hp_objSlave dt in
	{p with hp_objLeading = newObjLead ; hp_objSlave = newObjSlave} in


    let judgeImpactPoint ~isVolley ~attackIntention ~opponentCurPos 
	    ~impact ~footTarget  ~runSpeed  ~playerCurPos = 
	let qual = 
	    if impact.y3 < 3.0 then 
		3 
	    else  
		if isVolley then
		    if attackIntention then 1 else 2
		else
		    if attackIntention then 2 else 1
	and passingShot = ((abs_float opponentCurPos.z2) <  courtHt2 -. 100.0) 
	and myMaxSpeed = 
	    if AI.isAttacking playerCurPos then
		p.hp_pc.pc_maxSpeedInFreeRunUnderNet 
	    else p.hp_pc.pc_maxSpeedInFreeRunStayBack in
	
	if attackIntention then
	    ( qual,
	      (AI.voteImpactHtTheHigherTheBetter impact p.hp_pc.pc_maxSmashHt )
	      *. 0.2 +. (AI.voteClosenessToNet footTarget) *. 0.7
	      +. (AI.voteRunSpeed runSpeed myMaxSpeed) *. 0.1 )
	else
	    ( qual,
	      (AI.voteImpactHtCloseTo 170.0 impact p.hp_pc.pc_maxSmashHt) *. 0.5 +.
		  ( AI.voteNotTooMuchBehindGroundLine footTarget) *. 0.5)
    in (* end judgeImpactPoint *)
    


    let newState , newObjLead2 , newObjSlave2,  newBall, newMouse, fatigueData', soundIds =

	
	let chooseBestResearch ~isTopSpin ~playerCurPos ~opponentCurPos
		~ballPos ~ballDir ~ball ~surf ~attackIntention ~bsm ~p =



	    let judgeAllResearches ~p ~isTopSpin ~playerCurPos ~opponentCurPos
		    ~ballPos ~ballDir ~ball ~bsm ~surf  ~attackIntention = 



		let createResearchAndJudgeIt ~p  ~isTopSpin ~impact ~playerCurPos ~ballPos
			~ballDir ~ball ~s ~opponentCurPos ~surf ~attackIntention =


		    let impactIsOutSideOfMyCourtSide = 
			if p.hp_playsInTopmostCourtHalf then
			    impact.z2 > -. 50.0
			else
			    impact.z2 < 50.0 in
		    
		    if impactIsOutSideOfMyCourtSide then
			None
		    else
			
			let arrives =  whenWillTheBallArriveAtZ ~z:impact.z2 ~s ~surf in
			
			match arrives with
			    | None -> None
			    | Some iaba ->
				  let impact3d = vec3dCreate impact.x2 iaba.iaba_whatYItWillHave impact.z2 in
				  let isVolley = s.bsm_bouncesSoFar = 0 && not iaba.iaba_itWillBounceFirst in 
				  if isVolley && s.bsm_lastShotWasAService then
				      None
				  else
				      let t1 = iaba.iaba_timeFromImpactToArrival in
				      let t0 = s.bsm_curTimer in
				      let deltaT = iaba.iaba_timeFromImpactToArrival -. s.bsm_curTimer in


				      let speedFreeRun = 
					  (if AI.isAttacking playerCurPos then 
					       p.hp_pc.pc_maxSpeedInFreeRunUnderNet
					   else
					       p.hp_pc.pc_maxSpeedInFreeRunStayBack)
					  /. p.hp_fatigueData.fatigueDivisor 
				      and speedNormalResearch = 
					  (if AI.isAttacking playerCurPos then
					       p.hp_pc.pc_maxSpeedInNormalResearchUnderNet
					   else
					       p.hp_pc.pc_maxSpeedInNormalResearchStayBack)
					  /. p.hp_fatigueData.fatigueDivisor 
				      and isForehand = 
					  if p.hp_playsInTopmostCourtHalf then
					      impact.x2 < playerCurPos.x2
					  else
					      impact.x2 > playerCurPos.x2 in
				      let footPosAtImpactTime ~deltaXFootRacket ~deltaZ = 
					  if p.hp_playsInTopmostCourtHalf then
					      let z = impact.z2 -. deltaZ in
					      if isForehand then
						  vec2dCreate (impact.x2 +. deltaXFootRacket) z
					      else
						  vec2dCreate (impact.x2 -. deltaXFootRacket ) z
					  else
					      let z = impact.z2 +. deltaZ in
					      if isForehand then
						  vec2dCreate (impact.x2 -. deltaXFootRacket) z
					      else
						  vec2dCreate (impact.x2 +. deltaXFootRacket) z
				      in



				      let footPosImpSmash = footPosAtImpactTime ~deltaXFootRacket:0.0 ~deltaZ:80.0 in
				      let runDirSmash = vec2dSub footPosImpSmash playerCurPos in
				      let runAngleSmash = smallestAngleWithNegativeZAxis runDirSmash in
				      let signXSmash = if runDirSmash.x2 > 0.0 then 1.0 else -. 1.0 in
				      let deltaXSmash = distance2d footPosImpSmash playerCurPos in
				      assert (deltaXSmash != 0.0);


				      let computeDeltaOpening  ~isForehand  ~researchKind= 
					  let prefix =
					      match serverData with
						  | Client _ -> gfxDir ^ "/B"
						  | Server _ | NeitherServerNorClient ->
							if p.hp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B" in


					  let animName = 

					      match researchKind with
						  | RKH_Normal vt  ->
							let core = 
							    if isForehand then 
								match vt with
								    | VOT_Volee -> "drittov"
								    | VOT_NotVolee sp ->
									  match sp with
									      | Topspin -> "dritto"
									      | Backspin -> "drittoback"
							    else
								match vt with
								    | VOT_Volee -> "rovesciov"
								    | VOT_NotVolee sp ->
									  match sp with
									      | Topspin -> "rovescio"
									      | Backspin -> "rovescioback"
							in
							prefix ^ core
						  | RKH_Smash _ ->
							prefix ^ "smash"
						  | RKH_Dive _ ->
							prefix ^ if isForehand then "drittoallungov" else "rovescioallungov"
						  | RKH_StretchForward _ ->
							prefix ^ if isForehand then "drittoforwardstretch" else "rovescioforwardstretch"

					  in
					  let an = 
					      try
						  StringMap.find animName p.hp_objLeading.o3d_animations 
					      with Not_found -> 
						  (print_endline ("Not_found " ^ animName);
						   raise Not_found)
					  in
					  match an with 
					      | ServiceAnimation s ->s.serviceAnim_TimeFromLaunchToImpact
					      | ShotAnimation s ->s.shotAnim_TimeFromOpeningToImpact 
					      | RunAnimation _ -> assert(false)
				      in
				      

				      let deltaOpeningSmash =  computeDeltaOpening ~isForehand
					  ~researchKind:(RKH_Smash isVolley) in

				      
				      let tChangeSmash = t1 -. deltaOpeningSmash in
				      assert (tChangeSmash <= t1);
				      let v2Smash = (deltaXSmash  +. speedFreeRun  *. 
							 (t0 -. tChangeSmash) ) /. ( t1 -. tChangeSmash) in




				      let calcVoteAndResearch ~researchKind ~tChange ~v2 ~footPosImp ~runAngle ~signX ~deltaX=
					  let qual, vote = 
					      judgeImpactPoint ~attackIntention ~isVolley ~playerCurPos
						  ~opponentCurPos ~impact:impact3d ~footTarget:footPosImp
						  ~runSpeed:(if v2 < 0.0 then 0.0 else v2) in

					  let speedBefOpening = 
					      if v2 < 0.0 then
						  let v1 = deltaX /. (tChange -. t0) in
						  vec2dCreate (signX *. v1 *. sin runAngle) 
						      (-. v1 *. cos runAngle)
					      else
						  vec2dCreate (signX *. speedFreeRun *. sin runAngle) 
						      (-. speedFreeRun *. cos runAngle)    in		
					  let speedAftOpening = 
					      if v2 < 0.0 then
						  vec2dCreate 0.0 0.0 
					      else
						  vec2dCreate (signX *. v2 *. sin runAngle) 
						      (-. v2 *. cos runAngle)    in		

					  let research = 
					      { brh_forehand = isForehand;
						brh_runSpeedBeforeOpening = speedBefOpening;
						brh_runSpeedAfterOpening = speedAftOpening;
						brh_researchKind = researchKind;
						brh_t1 = t1;
						brh_t0 = t0;
						brh_tChange = tChange;
						brh_ballVelAtImpact =iaba.iaba_ballVelWhenItArrives;
						brh_impact = impact3d;
						brh_footTarget = footPosImp;
						brh_modulusOfRunSpeedAtImpactTime = if v2 < 0.0 then 0.0 else v2
					      } in
					  Some ( research, (qual, vote) )
				      in
				      


				      let canDoSmash = 
					  p.hp_pc.pc_minSmashHt < impact3d.y3 && impact3d.y3 <
					      p.hp_pc.pc_maxSmashHt && tChangeSmash >= 0.0 && deltaT >=
					      deltaOpeningSmash &&  v2Smash <= speedNormalResearch in

				      if canDoSmash then
					  calcVoteAndResearch ~researchKind:(RKH_Smash isVolley) ~v2:v2Smash ~tChange:tChangeSmash
					      ~footPosImp:footPosImpSmash ~runAngle:runAngleSmash ~signX:signXSmash ~deltaX:deltaXSmash
				      else

					  
					  let vot = 
					      if isVolley then
						  VOT_Volee 
					      else
						  if isTopSpin then
						      VOT_NotVolee Topspin
						  else
						      VOT_NotVolee Backspin
					  in
					  let deltaOpening =
					      computeDeltaOpening ~researchKind:(RKH_Normal vot) 
						  ~isForehand in
					  let tChangeNormalShot = t1 -. deltaOpening in
					  assert (tChangeNormalShot <= t1);
					  let footPosImpNorm = footPosAtImpactTime ~deltaXFootRacket:110.0 ~deltaZ:80.0 in
					  let runDirNorm = vec2dSub footPosImpNorm playerCurPos in
					  let runAngleNorm = smallestAngleWithNegativeZAxis runDirNorm in
					  let signXNorm = if runDirNorm.x2 > 0.0 then 1.0 else -. 1.0 in
					  let deltaXNorm = distance2d footPosImpNorm playerCurPos in
					  assert (deltaXNorm != 0.0);
					  let v2NormalShot = 
					      (deltaXNorm  +. speedFreeRun  *.
						   (t0-. tChangeNormalShot) ) /. ( t1 -. tChangeNormalShot) in
					  let canDoNormalShot = 
					      0.0 < impact3d.y3 && impact3d.y3 < p.hp_pc.pc_maxShotHt &&
						  tChangeNormalShot >= 0.0 && 
						  deltaT >= deltaOpening &&  v2NormalShot <= speedNormalResearch in

					  if  canDoNormalShot then


					      calcVoteAndResearch ~researchKind:(RKH_Normal vot) ~runAngle:runAngleNorm ~signX:signXNorm
						  ~v2:v2NormalShot ~tChange:tChangeNormalShot ~footPosImp:footPosImpNorm ~deltaX:deltaXNorm
					  else
					      (* try stretch forward *)

					      let footPosImpStr = footPosAtImpactTime ~deltaXFootRacket:(50.0) 
						  ~deltaZ:301.0 in
					      let runDirStr = vec2dSub footPosImpStr playerCurPos in
					      let runAngleStr = smallestAngleWithNegativeZAxis runDirStr in
					      let signXStr = if runDirStr.x2 > 0.0 then 1.0 else -. 1.0 in
					      let deltaXStr = distance2d footPosImpStr playerCurPos in
					      assert (deltaXStr != 0.0);


					      let deltaOpeningStr =  computeDeltaOpening 
						  ~isForehand  ~researchKind:(RKH_StretchForward isVolley) in

					      
					      let tChangeStr = t1 -. deltaOpeningStr in
					      assert (tChangeStr <= t1);
					      let v2Str = (deltaXStr  +. speedFreeRun  *. 
							       (t0 -. tChangeStr) ) /. ( t1 -. tChangeStr) in

					      

					      let canDoStr = 
						  0.0 < impact3d.y3 && 
						      impact3d.y3 < p.hp_pc.pc_maxShotHt && 
						      tChangeStr >= 0.0 && 
						      deltaT >= deltaOpeningStr &&
						      v2Str <= speedNormalResearch in

					      if canDoStr then
						  calcVoteAndResearch ~researchKind:(RKH_StretchForward isVolley)
						      ~v2:v2Str ~tChange:tChangeStr
						      ~footPosImp:footPosImpStr ~runAngle:runAngleStr ~signX:signXStr ~deltaX:deltaXStr
					      else


						  (* try dive *)

						  let footPosImpD = footPosAtImpactTime ~deltaXFootRacket:(271.0) 
						      ~deltaZ:20.0 in
						  let runDirD = vec2dSub footPosImpD playerCurPos in
						  let runAngleD = smallestAngleWithNegativeZAxis runDirD in
						  let signXD = if runDirD.x2 > 0.0 then 1.0 else -. 1.0 in
						  let deltaXD = distance2d footPosImpD playerCurPos in
						  assert (deltaXD != 0.0);


						  let deltaOpeningDive =  computeDeltaOpening 
						      ~isForehand  ~researchKind:(RKH_Dive isVolley) in

						  
						  let tChangeDive = t1 -. deltaOpeningDive in
						  assert (tChangeDive <= t1);
						  let v2Dive = (deltaXD  +. speedFreeRun  *. 
								    (t0 -. tChangeDive) ) /. ( t1 -. tChangeDive) in

						  

						  let canDoDive = 
						      0.0 < impact3d.y3 && 
							  impact3d.y3 < p.hp_pc.pc_maxShotHt && 
							  tChangeDive >= 0.0 && 
							  deltaT >= deltaOpeningDive &&
							  v2Dive <= speedNormalResearch in

						  if canDoDive then
						      calcVoteAndResearch ~researchKind:(RKH_Dive isVolley)
							  ~v2:v2Dive ~tChange:tChangeDive
							  ~footPosImp:footPosImpD ~runAngle:runAngleD ~signX:signXD ~deltaX:deltaXD
						  else
						      None


							  

		in (* createResearchAndJudgeIt *)



		let theBallHasAlreadyCrossedTheNet =
		    if p.hp_playsInTopmostCourtHalf then
			(curBallPos ball).z3 < -. 50.0
		    else
			(curBallPos ball).z3 > 50.0
		in
		let numPointsToJudge = 180 in
		
		let secondPointForResearch = whereWillTheBallMakeSecondBounce ~b:ball ~bsm
		    ~surf in
		let mayb = 
		    if theBallHasAlreadyCrossedTheNet  then
			Some (projection2d (curBallPos ball))
		    else
			let fifty = if p.hp_playsInTopmostCourtHalf then -. 20.0 else 20.0 in
			let may =
			    whenWillTheTrajArriveAtZ ~z:fifty ~t:bsm.bsm_trajectory  in
			match may with 
			    | None -> None
			    | Some iata ->
				  Some (vec2dCreate  iata.iata_x fifty)
		in
		match mayb with
		    | None -> []
		    | Some firstPointforResearch ->
			  let stepT = 1.0 /. (float_of_int numPointsToJudge) in
			  let l = (listFromTo 0 (numPointsToJudge - 1) ) in
			  let factors = List.map (fun x -> stepT *. (float_of_int x) ) l in
			  let impacts = List.map
			      (fun fac -> vec2dAdd firstPointforResearch (vec2dMulScalar fac (vec2dSub secondPointForResearch firstPointforResearch))) 
			      factors 
			  in
			  List.map (fun x -> createResearchAndJudgeIt ~p ~isTopSpin ~impact:x
					~playerCurPos ~ballPos ~ballDir ~ball ~s:bsm ~opponentCurPos
					~surf ~attackIntention ) impacts
	    in (* judgeAllResearches *)



	    let researches' = 
		let researches = judgeAllResearches ~p ~isTopSpin ~playerCurPos
		    ~opponentCurPos ~ball ~ballPos ~ballDir ~bsm ~surf ~attackIntention in
		select_some researches in


	    let smashVolleys =
		let isSmashVolley (res, _) =
		    match res.brh_researchKind with
			| RKH_Smash v ->
			      v
			| RKH_Dive _ | RKH_StretchForward _ | RKH_Normal _ -> false
		in

		List.filter isSmashVolley researches' in
	    
	    let better (r1, (q1, v1)) (r2, (q2, v2)) =
		if q1 < q2 then
		    (r1, (q1, v1))
		else if q2 < q1 then
		    (r2, (q2, v2))
		else
		    if v1 > v2 then r1, (q1, v1) else r2, (q2, v2)
	    in
	    
	    match smashVolleys with
		| [] ->

		      let voleesCloseToNet = 
			  let isVoleeCloseToNet (res, _) =
			      if abs_float res.brh_impact.z3 < courtHt4  then
				  match res.brh_researchKind with
				      | RKH_Smash  _   -> false (* already dealt with *)
				      | RKH_Normal VOT_Volee -> true
				      | RKH_Normal VOT_NotVolee _ -> false
				      | RKH_StretchForward _ -> false (* later *)
				      | RKH_Dive _ -> false (* later *)
			      else
				  false
			  in
			  
			  List.filter isVoleeCloseToNet researches' in

		      (match voleesCloseToNet with
			   | [] ->
				 
				 ( let normalShots =
				       let isNormalShot (res, _) =
					   match res.brh_researchKind with RKH_Normal _ -> true 
					       | RKH_Dive _ | RKH_StretchForward _ | RKH_Smash _ -> false in
				       List.filter isNormalShot researches' in
				   
				   match normalShots with
				       | [] ->
					     ( let smashGrounds = 
						   let isSmashGround (res,  _) =
						       match res.brh_researchKind with 
							   | RKH_Smash v -> not v
							   | RKH_StretchForward _ | RKH_Normal _ | RKH_Dive _ -> false
						   in
						   List.filter isSmashGround researches' in
					       
					       match smashGrounds with
						   | [] -> 
							 
							 let stretches = 
							     let isStretch (res, _) =
								 match res.brh_researchKind with
								     | RKH_StretchForward _ -> true
								     | RKH_Smash _ | RKH_Normal _ | RKH_Dive _ -> false in
							     
							     List.filter isStretch researches' in
							 (match stretches with
							      | [] -> 
								    let diveVolleys =
									let isDiveVolley (res,_) =
									    match res.brh_researchKind with
										| RKH_Dive v -> v
										| RKH_Smash _ | RKH_StretchForward _ | RKH_Normal _ -> false
									in
									List.filter isDiveVolley researches' in
								    ( match diveVolleys with
									  | [] -> None
									  | l -> Some (fst (findBestElement l better)))
							      | l -> Some (fst (findBestElement l better)))
							     
						   | l -> Some (fst (findBestElement l better)) )
				       | l -> Some (fst  (findBestElement l better)))
			   | l -> Some (fst (findBestElement l better)))
		| l -> Some (fst (findBestElement l better))
		      
		      
	in

	let prefixSlave = 
	    match serverData with
		| Client _ ->
		      gfxDir ^ "/A"
		| Server _ ->
		      gfxDir ^ "/A"
		| NeitherServerNorClient ->
		      if p.hp_playsInTopmostCourtHalf then gfxDir ^ "/B" else gfxDir ^ "/A"
	and prefixLead = 
	    match serverData with
		| Client _ ->
		      gfxDir ^ "/B"
		| Server _ ->
		      gfxDir ^ "/B"
		| NeitherServerNorClient ->
		      if p.hp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B"
	in


	let ifBallIsTowardsYouThenStartAutoSearch ~newObLead ~newObSlave ~newPos ~newSounds 
		~bsm ~realizationState ~newFatigueData ~diveHasEverBeenPossible ~newSprint = 
	    let ballIsComingTowardsMe  = 
		if p.hp_playsInTopmostCourtHalf then
		    bsm.bsm_trajectory.startVel.z3 < 0.0
		else
		    bsm.bsm_trajectory.startVel.z3 > 0.0 in
	    
	    let ballIsAboutToHitNet = 
		let maybeT = whenWillTheTrajectoryHitTheNet bsm.bsm_trajectory in
		match maybeT with
		    | None -> false
		    | Some _ -> (bsm.bsm_bouncesSoFar = 0) in
	    let ballWillGoOut = 
		bsm.bsm_bouncesSoFar = 0 &&
		(match bsm.bsm_trajectory.targetRect with None -> false | _ -> true) &&
		not (theTrajectoryFallsInTheTargetRect bsm.bsm_trajectory) in
	    if
		ballIsComingTowardsMe && bsm.bsm_isItGoodSoFar &&
		    (not ballIsAboutToHitNet) && (not ballWillGoOut)
	    then
		match  realizationState with
		    | NotRealized ->
			  (HPS_RealizingWhereTheBallIs {umd_timer = 0.0;
							umd_startPos = newPos;
							umd_startVel=(vec2dCreate 0.0 0.0)},
			   newObLead,
			   newObSlave,
			   newSounds)
			      
		    | Realized ->
			  
			  let attackIntention =
			      abs_float newPos.z2 < courtHt2	*. 3.0 /. 4.0   in
			  let res = chooseBestResearch ~isTopSpin:true
			      ~playerCurPos:newPos ~opponentCurPos
			      ~ballPos:(projection2d (curBallPos b) )
			      ~ballDir:(projection2d (curBallVel bsm) ) ~ball:b ~surf
			      ~attackIntention ~bsm ~p in
			  let doFakeDive ~reasonForMiss  = 
			      let diveAngle =
				  let diveDir = 
				      let fakeImpact =
					  let ballDir = 
					      
					      straightLineFromPointAndDir (projection2d (curBallPos b))
						  (projection2d (curBallVel bsm)) in
					  
					  let horizLine = 
					      let pz = newPos.z2 in
					      {sl_a = 0.0 ; sl_b = 1.0; sl_c = -. pz} in
					  
					  intersectionOfStraightLines ballDir horizLine in
				      vec2dSub fakeImpact newPos
				  in
				  let leftAxis =
				      vec2dCreate (dirsign *. (-. 1.0)) 0.0 in
				  smallestAngleBetween leftAxis diveDir
			      in
			      
			      let core = 
				  
				  if diveAngle < pi_2 then
				      "rovescioallungov"
				  else
				      "drittoallungov"
			      in
			      let obl = 
				  setAnim ~o:newObLead ~animName:(prefixLead ^ core)
				      ~restartIfSameAnimation:true in
			      let obs = 
				  setAnim ~o:newObSlave ~animName:(prefixSlave ^ core)
				      ~restartIfSameAnimation:true in
			      let umd = 
				  let vx = 
				      let lrSign = 
					  if diveAngle < pi_2 then
					      -. 1.0
					  else
					      1.0 in
				      lrSign *. dirsign *. 
					  (if AI.isAttacking newPos then 
					       p.hp_pc.pc_maxSpeedInFreeRunUnderNet 
					   else p.hp_pc.pc_maxSpeedInFreeRunStayBack)
				      /. newFatigueData.fatigueDivisor
				  in
				  { umd_timer = 0.0;
				    umd_startPos = newPos;
				    umd_startVel = vec2dCreate vx 0.0 }
			      in
			      (HPS_DivingFake (umd, reasonForMiss) ,
			       obl, obs, SoundSprintJustBegun::newSounds)
			  in
			  match res with 
			      | None ->
				    let divePossib = 
					if diveHasEverBeenPossible = DivePossibilityUnknown then
					    DiveHasNeverBeenPossible
					else
					    diveHasEverBeenPossible in
				    if Sdlkey.is_key_pressed Sdlkey.KEY_d then
					doFakeDive ~reasonForMiss:(match divePossib with 
								       | DiveHasNeverBeenPossible  -> 
									     DiveOnShotTooFar
								       | DiveHasBeenPossible -> DiveTooLate
								       | DivePossibilityUnknown -> assert false)
				    else
					(HPS_ManualSearch {hpsms_pos = newPos;
							   hpsms_realizing =  Realized;
							   hpsms_askedToSprintInPrevFrame = newSprint;
							   hpsms_diveIsPossibleNow = DiveNotPossible;
							   hpsms_diveHasEverBeenPossible = divePossib}, 
					 newObLead,
					 newObSlave,
					 newSounds)
			      | Some r ->
				    let onlyDive = 
					match r.brh_researchKind with | RKH_Dive _ -> true | 
						RKH_Normal _ | RKH_StretchForward _ | RKH_Smash _ -> false in
				    if not onlyDive &&  Sdlkey.is_key_pressed Sdlkey.KEY_d  then
					doFakeDive ~reasonForMiss:DiveWithNoNeed (* punished for diving without need *)
				    else if onlyDive && not (Sdlkey.is_key_pressed Sdlkey.KEY_d) then
					(* dive must be explicitely requested: do nothing *)
					(HPS_ManualSearch {hpsms_pos = newPos;
							   hpsms_realizing = Realized;
							   hpsms_askedToSprintInPrevFrame = newSprint;
							   hpsms_diveIsPossibleNow = DivePossible;
							   hpsms_diveHasEverBeenPossible = DiveHasBeenPossible},
					 newObLead,
					 newObSlave,
					 newSounds)
				    else
					
					let walkAni = 
					    computeWalkAnim ~footTarget:r.brh_footTarget 
						~curPos:newPos
						~dirsign  in
					let newObLead = 
					    let animName = prefixLead ^ walkAni in
					    setAnim ~o:newObLead ~animName ~restartIfSameAnimation:false in
					let newObSlave = 
					    let animName = prefixSlave ^ walkAni in
					    setAnim ~o:newObSlave ~animName ~restartIfSameAnimation:false in
					

					let mAsbo = createStateASBO ~p ~r in
					match mAsbo with
					    | None ->
						  (HPS_ManualSearch { 
						       hpsms_diveHasEverBeenPossible = diveHasEverBeenPossible;
						       hpsms_pos = newPos;
						       hpsms_realizing = realizationState;
						       hpsms_askedToSprintInPrevFrame = newSprint;
						       hpsms_diveIsPossibleNow =  DiveNotNeeded}, 
						   newObLead, newObSlave,
						   newSounds)

					    | Some asb ->
						  (  HPS_AutoSearchBeforeOpening asb ,
						     newObLead, newObSlave,
						     newSounds)
						      
	    else
		(HPS_ManualSearch { 
		     hpsms_diveHasEverBeenPossible = diveHasEverBeenPossible;
		     hpsms_pos = newPos;
		     hpsms_realizing = realizationState;
		     hpsms_askedToSprintInPrevFrame = newSprint;
		     hpsms_diveIsPossibleNow =  DiveNotNeeded}, 
		 newObLead, newObSlave,
		 newSounds)
	in




	match p.hp_state with
	    | HPS_RealizingWhereTheBallIs u ->
		  ( match b.b_state with
			| BS_Moving bsm ->
			      let newT =  u.umd_timer +. dt in
			      if newT < reflexDeltaT then
				  (HPS_RealizingWhereTheBallIs { u with umd_timer =  newT },
				   p.hp_objLeading, p.hp_objSlave,  b, mouse, p.hp_fatigueData, [])
			      else
				  let newst, newObLead, newObSlave, sounds =
				      ifBallIsTowardsYouThenStartAutoSearch ~newObSlave:p.hp_objSlave
					  ~newObLead:p.hp_objLeading ~newPos:(curPosOfHumanPlayer p)
					  ~newSounds:[] ~bsm ~realizationState:Realized ~newFatigueData:p.hp_fatigueData
					  ~diveHasEverBeenPossible:DivePossibilityUnknown ~newSprint:HasNotAsked
					  
				  in
				  (newst, newObLead, newObSlave, b, mouse, p.hp_fatigueData, sounds)
			| BS_Still _ ->
			      (HPS_ManualSearch {hpsms_pos = curPosOfHumanPlayer p;
						 hpsms_realizing = Realized;
						 hpsms_askedToSprintInPrevFrame =  HasNotAsked;
						 hpsms_diveIsPossibleNow = DiveNotNeeded;
						 hpsms_diveHasEverBeenPossible = DivePossibilityUnknown},
			       p.hp_objLeading,p.hp_objSlave, b, mouse, p.hp_fatigueData, []) )
		      
	    | HPS_AutoSearchBeforeOpening h ->
		  let h = 
		      match b.b_state with
			  | BS_Still _ -> assert(false)
			  | BS_Moving bsm ->
				let backSpinMakesSense  = 
				    match h.asbo_researchKind with
					| RKH_Normal vt ->
					      (match vt with
						   | VOT_Volee -> false
						   | VOT_NotVolee ts ->
							 (match ts with
							      | Topspin -> true
							      | Backspin -> false))
					| RKH_Dive _ -> false
					| RKH_StretchForward _ -> false
					| RKH_Smash _ -> false
				in
				if Sdlkey.is_key_pressed Sdlkey.KEY_z && backSpinMakesSense  then
				    let attackIntention =
					abs_float (curPosOfHumanPlayer p).z2 < courtHt2 *. 3.0 /. 4.0   in
				    let res = chooseBestResearch ~isTopSpin:false
					~playerCurPos:(curPosOfHumanPlayer p) ~opponentCurPos
					~ballPos:(projection2d (curBallPos b) )
					~ballDir:(projection2d (curBallVel bsm) ) ~ball:b ~surf
					~attackIntention ~bsm ~p in
				    begin
					match res with
					    | None ->
						  h
					    | Some r ->
						  { asbo_HtOverNet = p.hp_startHtOverNetForBackSpinGround;
						    asbo_CurAim = h.asbo_CurAim;
						    (* the rest of the fields is trivial: *)
						    asbo_TimeToRunBeforeOpening = r.brh_tChange -. r.brh_t0;
						    asbo_RunSpeedBeforeOpening = r.brh_runSpeedBeforeOpening;
						    asbo_BallVelAtImpact = r.brh_ballVelAtImpact;
						    asbo_Impact = r.brh_impact;
						    asbo_FootTarget = r.brh_footTarget;
						    asbo_ModulusOfRunSpeedAtImpactTime = r.brh_modulusOfRunSpeedAtImpactTime;
						    asbo_UniformMotionData = 
							  { umd_startPos =(curPosOfHumanPlayer p);
							    umd_startVel = r.brh_runSpeedBeforeOpening;
							    umd_timer = 0.0 };
						    asbo_TimeToRunFromOpeningToImpact = r.brh_t1 -.  r.brh_tChange;
						    asbo_RunSpeedFromOpeningToImpact = r.brh_runSpeedAfterOpening;
						    asbo_Forehand = r.brh_forehand;
						    asbo_researchKind = r.brh_researchKind }
				    end
				else
				    h
		  in
		  
		  let spin =
		      match h.asbo_researchKind with
			  | RKH_Normal vt ->
				(match vt with
				     | VOT_Volee -> spinForVolee
				     | VOT_NotVolee topS ->
					   match topS with
					       | Topspin -> p.hp_pc.pc_topSpin
					       | Backspin -> p.hp_pc.pc_backSpin
				)
			  | RKH_Dive _ -> spinForVolee
			  | RKH_StretchForward _ -> spinForVolee
			  | RKH_Smash _ -> spinForVolee
		  in
		  let newBounce, htOverNet = 
		      let  minPowerAvailable = calcMinShotPower ~ballVelZ:h.asbo_BallVelAtImpact.z3
			  ~exploit:p.hp_pc.pc_exploitationOfOpponentsPower
		      and maxPowerAvailable = calcMaxShotPower ~search:h.asbo_researchKind
			  ~maxPow:p.hp_pc.pc_maxShotPower ~exploit:p.hp_pc.pc_exploitationOfOpponentsPower
			  ~ballVelZ:h.asbo_BallVelAtImpact.z3
		      in


		      let newBounce, htOverNet = updateHtOverNetByReadingInput ~dt ~p
			  ~htOverNet:h.asbo_HtOverNet
			  ~impact:h.asbo_Impact ~spin  
			  ~bounceDesired:h.asbo_CurAim ~maxPowerAvailable
			  ~minPowerAvailable ~mouse  in
		      updateAimByReadingInput ~p ~dt ~idealHtOverNet:htOverNet
			  ~iCanIncreaseTheHt:true
			  ~impact:h.asbo_Impact ~spin 
			  ~minPowerAvailable ~maxPowerAvailable
			  ~curAim:newBounce ~mouse ~mouseSensitivity  in
		  let newT =  h.asbo_UniformMotionData.umd_timer +. dt in
		  if newT >= h.asbo_TimeToRunBeforeOpening then
		      let newst = HPS_AutoSearchAfterOpening
			  { 
			      asao_HtOverNet = htOverNet;
			      asao_CurAim = newBounce;
			      asao_UniformMotionData = 
				  { umd_startPos = (curPosOfHumanPlayer p);
				    umd_startVel = h.asbo_RunSpeedFromOpeningToImpact;
				    umd_timer = 0.0 } ;
			      (* unchanged: *)
			      asao_BallVelAtImpact = h.asbo_BallVelAtImpact;
			      asao_Impact = h.asbo_Impact;
			      asao_FootTarget = h.asbo_FootTarget;
			      asao_ModulusOfRunSpeedAtImpactTime = h.asbo_ModulusOfRunSpeedAtImpactTime;
			      asao_TimeToRunFromOpeningToImpact = h.asbo_TimeToRunFromOpeningToImpact;
			      asao_RunSpeedFromOpeningToImpact = h.asbo_RunSpeedFromOpeningToImpact;
			      asao_Forehand = h.asbo_Forehand;
			      asao_researchKind = h.asbo_researchKind
			  } in
		      let nam = 
			  match h.asbo_researchKind with
			      | RKH_Smash _ ->
				    "smash"
			      | RKH_Normal vt ->
				    if h.asbo_Forehand then
					match vt with
					    | VOT_Volee -> "drittov"
					    | VOT_NotVolee Topspin -> "dritto"
					    | VOT_NotVolee Backspin -> "drittoback"
				    else
					(match vt with
					     | VOT_Volee -> "rovesciov"
					     | VOT_NotVolee Topspin -> "rovescio"
					     | VOT_NotVolee Backspin -> "rovescioback")

			      | RKH_Dive _ ->
				    if h.asbo_Forehand then
					"drittoallungov"
				    else
					"rovescioallungov"
			      | RKH_StretchForward _ ->
				    if h.asbo_Forehand then "drittoforwardstretch" else "rovescioforwardstretch"
		      in
		      let newLead = 
			  let animName = 
			      prefixLead ^ nam in

			  setAnim ~o:p.hp_objLeading ~animName ~restartIfSameAnimation:true in
		      let newSlave = 
			  let animName = 


			      prefixSlave ^ nam in
			  setAnim ~o:p.hp_objSlave ~animName ~restartIfSameAnimation:true in

		      let sounds = 
			  match h.asbo_researchKind with
			      |  RKH_Dive _ ->  [ SoundSprintJustBegun ]
			      | RKH_StretchForward _ | RKH_Normal _ | RKH_Smash _ -> []
		      in

		      ( newst, newLead, newSlave,  b,mouse2, p.hp_fatigueData, sounds)
		  else
		      let newst = HPS_AutoSearchBeforeOpening
			  { h with 
				asbo_HtOverNet = htOverNet;
				asbo_CurAim = newBounce;
				asbo_UniformMotionData = { h.asbo_UniformMotionData with 
							       umd_timer = newT }
			  }
		      in
		      ( newst, p.hp_objLeading, p.hp_objSlave, b, mouse2, p.hp_fatigueData, [])

	    | HPS_AutoSearchAfterOpening h ->
		  
		  let spin =
		      match h.asao_researchKind with
			  | RKH_Normal vt ->
				(match vt with
				     | VOT_Volee -> spinForVolee
				     | VOT_NotVolee topS ->
					   match topS with
					       | Topspin -> p.hp_pc.pc_topSpin
					       | Backspin -> p.hp_pc.pc_backSpin
				)
			  | RKH_Dive _ -> spinForVolee
			  | RKH_StretchForward _ -> spinForVolee
			  | RKH_Smash _ -> spinForVolee
		  in

		  let newBounce2, htOverNet2 = 

		      let  minPowerAvailable = calcMinShotPower
			  ~ballVelZ:h.asao_BallVelAtImpact.z3 
			  ~exploit:p.hp_pc.pc_exploitationOfOpponentsPower 
			  
		      and maxPowerAvailable = calcMaxShotPower ~search:h.asao_researchKind 
			  ~maxPow:p.hp_pc.pc_maxShotPower ~exploit:p.hp_pc.pc_exploitationOfOpponentsPower
			  ~ballVelZ:h.asao_BallVelAtImpact.z3
		      in

		      let newBounce, htOverNet = updateHtOverNetByReadingInput ~dt ~p
			  ~htOverNet:h.asao_HtOverNet 
			  ~impact:h.asao_Impact ~spin 
			  ~bounceDesired:h.asao_CurAim
			  ~maxPowerAvailable ~minPowerAvailable ~mouse in
		      assert((newBounce.z2 > 0.0) = p.hp_playsInTopmostCourtHalf);
		      updateAimByReadingInput ~p ~dt ~idealHtOverNet:htOverNet
			  ~iCanIncreaseTheHt:true (*(h.asao_researchKind != DiveResearch) *)
			  ~impact:h.asao_Impact ~spin ~maxPowerAvailable ~minPowerAvailable
			  ~curAim:newBounce ~mouse ~mouseSensitivity 
			  
		  in
		  let newT =  h.asao_UniformMotionData.umd_timer +. dt in
		  assert((newBounce2.z2 > 0.0) = p.hp_playsInTopmostCourtHalf);
		  if newT >= h.asao_TimeToRunFromOpeningToImpact then
		      let maybeTr = 
			  buildTrajFromTwoPointsAndHeight
			      ~impact:h.asao_Impact ~htOverNet:htOverNet2 ~spin
			      ~bounceDesired:newBounce2 
			      ~targetRect:(Some (if p.hp_playsInTopmostCourtHalf then lowerHalfOfCourt else upperHalfOfCourt))
		      in
		      match maybeTr with
			  | None-> assert(false)
			  | Some traj ->
				assert (p.hp_playsInTopmostCourtHalf = (traj.impact.z3 < 0.0));
				let bb = createRunningBall ~traj
				    ~scoreIndexOfLastPlayerWhoHit:p.hp_scoreIndex
				    ~polyBall:b.b_polygon ~polyRedCross:b.b_redCrossPolygon
				    ~polyShadow:b.b_shadowPolygon ~service:false
				in
				let umd ={umd_timer = 0.0;
					  umd_startVel = h.asao_RunSpeedFromOpeningToImpact; 
					  umd_startPos = (curPosOfHumanPlayer p) }in
				let newst =
				    let dive = (match h.asao_researchKind with RKH_Dive _ -> true 
						    | RKH_Smash _ | RKH_StretchForward _ | RKH_Normal _ -> false) in

				    HPS_AutoSearchAfterImpactWaitingForAnimToEnd (umd, dive) in
				let pow = 
				    let mpow  = findNeededPower ~impact:h.asao_Impact ~spin ~bounceDesired:newBounce2
					~htOverNet:htOverNet2 in
				    match mpow with | None -> assert false | Some po -> po

				in
				print_endline ("human player -  vel z of opponent shot at impact: Kmh " ^
						   string_of_float (kmH_of_cmPerSec (length3d h.asao_BallVelAtImpact))
					       ^ ", cmpersec = " ^ string_of_float (length3d h.asao_BallVelAtImpact) ^
						   ", power = " ^ string_of_float pow);

				let sounds = 
				    match h.asao_researchKind with
					| RKH_Normal vt ->
					      ( match vt with
						    | VOT_Volee -> [SoundNormalShot]
						    | VOT_NotVolee Backspin -> [ SoundLightShot; SoundNormalShot]
						    | VOT_NotVolee Topspin ->
							  
							  let powerIsCloseToMaxAndAboveThreshold = 
							      let po = length3d traj.startVel in
							      po > 3000.0 in
							  let fatigueIsHigh = 
							      p.hp_fatigueData.fatigueDivisor > 1.35 in
							  if powerIsCloseToMaxAndAboveThreshold then
							      [ SoundAhh; SoundNormalShot]
							  else if fatigueIsHigh then
							      [SoundHff; SoundNormalShot]
							  else 
							      [SoundNormalShot]
					      )

					| RKH_Dive _ ->
					      [  SoundNormalShot]
					| RKH_Smash _ ->
					      [  SoundNormalShot ; SoundAhh]
					| RKH_StretchForward _ ->
					      [ SoundNormalShot ; SoundHff]
				in

				(newst, p.hp_objLeading, p.hp_objSlave  ,bb, mouse2, p.hp_fatigueData, sounds )
		  else
		      let newst = 
			  let hh = { h with
					 asao_HtOverNet = htOverNet2;
					 asao_CurAim = newBounce2;
					 asao_UniformMotionData = {h.asao_UniformMotionData with umd_timer = newT}
				   } in
			  HPS_AutoSearchAfterOpening hh in
		      (newst, p.hp_objLeading, p.hp_objSlave, b, mouse2, p.hp_fatigueData, [])

	    | HPS_GettingUpAfterDive (pos, timer, tooLate) ->
		  let t' = timer +. dt in
		  let tim = 2.0 in
		  let newst = 
		      if t' > tim then
			  HPS_ManualSearch {hpsms_pos = pos;
					    hpsms_realizing = NotRealized;
					    hpsms_askedToSprintInPrevFrame = HasNotAsked;
					    hpsms_diveIsPossibleNow = DiveNotNeeded;
					    hpsms_diveHasEverBeenPossible = DivePossibilityUnknown}
		      else
			  HPS_GettingUpAfterDive (pos, t', tooLate)
		  in
		  let newObLead = 
		      if t' > tim then
			  setAnim ~o:p.hp_objLeading ~animName:(prefixLead ^ "saltello")
			      ~restartIfSameAnimation:true 
		      else
			  p.hp_objLeading in

		  let newObSlave =
		      if t' > tim then
			  setAnim ~o:p.hp_objSlave ~animName:(prefixSlave ^ "saltello")
			      ~restartIfSameAnimation:true 
		      else
			  p.hp_objSlave in
		  ( newst, newObLead, newObSlave, b, mouse2, p.hp_fatigueData, [])

	    | HPS_DivingFake( u, reas) ->

		  let newT =  u.umd_timer +. dt in

		  let newst = 
		      if p.hp_objLeading.o3d_animState = NotAnimated then
			  HPS_GettingUpAfterDive (curPosOfHumanPlayer p, 0.0,
						  match reas with
						      | DiveTooLate -> true 
						      | DiveOnShotTooFar -> false
						      | DiveWithNoNeed -> false) (* @@ this happened diving. 2p emanuele sopra *)
		      else
			  HPS_DivingFake ( { u with umd_timer = newT}, reas)
		  in
		  ( newst, p.hp_objLeading, p.hp_objSlave, b, mouse2, p.hp_fatigueData, [])
		      
		      
	    | HPS_AutoSearchAfterImpactWaitingForAnimToEnd (u, dive) ->
		  let t' = u.umd_timer +. dt in
		  let newst = 
		      if p.hp_objLeading.o3d_animState = NotAnimated then
			  if dive then
			      HPS_GettingUpAfterDive ( curPosOfHumanPlayer p , 0.0, false)
			  else
			      HPS_ManualSearch {hpsms_pos = curPosOfHumanPlayer p;
						hpsms_realizing = NotRealized;
						hpsms_askedToSprintInPrevFrame = HasNotAsked;
						hpsms_diveIsPossibleNow = DiveNotNeeded;
						hpsms_diveHasEverBeenPossible = DivePossibilityUnknown}
		      else
			  HPS_AutoSearchAfterImpactWaitingForAnimToEnd ({ u with umd_timer = t'}, dive) in
		  let p = {p with hp_state = newst} in

		  let newObLead = 
		      if dive then
			  p.hp_objLeading
		      else
			  if p.hp_objLeading.o3d_animState = NotAnimated then
			      setAnim ~o:p.hp_objLeading ~animName:(prefixLead ^ "saltello")
				  ~restartIfSameAnimation:true 
			  else
			      p.hp_objLeading in

		  let newObSlave =
		      if dive then
			  p.hp_objSlave
		      else
			  if p.hp_objLeading.o3d_animState (* sic *) = NotAnimated then
			      setAnim ~o:p.hp_objSlave ~animName:(prefixSlave ^ "saltello")
				  ~restartIfSameAnimation:true 
			  else
			      p.hp_objSlave in
		  
		  ( newst, newObLead, newObSlave, b, mouse2, p.hp_fatigueData, [])
		      

	    | HPS_ManualSearch m  ->

		  let newPos, newFatigueData, newSprint, newSounds, newObLead, newObSlave = 

		      let wouldLikeToSprint =  Sdlkey.is_key_pressed Sdlkey.KEY_x in
		      

		      let maybeVelDir = 

			  let remoteSign = match serverData with Client _ -> -. 1.0 | 
				  Server _ | NeitherServerNorClient -> 1.0 in

			  let mouseVec =
			      let dx = remoteSign *. float_of_int mouse.m_xRel 
			      and dz = remoteSign *. float_of_int mouse.m_yRel in
			      (normalize2d (vec2dCreate dx dz )) in
			  let maybeDxDz = 
			      if mouse.m_secondsSinceLastMouseMotion > mouseRefresh then
				  None
			      else
				  if mouse.m_xRel = 0 && mouse.m_yRel = 0 then
				      None
				  else
				      Some mouseVec

			  in

			  if wouldLikeToSprint then
			      match m.hpsms_askedToSprintInPrevFrame with
				  | HasAskedAndObtained dirLocked -> 
					(* if you are sprinting, your direction is locked,
					   however you can change it a bit *)
					if mouse.m_xRel != 0 then
					    Some (normalize2d  (vec2dAdd (dirLocked) 
								    (vec2dMulScalar (dt *. 1.35)  mouseVec)))
					else
					    Some dirLocked

				  | HasNotAsked | HasAskedAndWasDenied -> maybeDxDz
			  else
			      maybeDxDz

		      in
		      match maybeVelDir with
			  | None ->
				let newObLead = 
				    let animName = prefixLead ^ "saltello" in
				    setAnim ~o:p.hp_objLeading ~animName ~restartIfSameAnimation:false in
				let newObSlave = 
				    let animName = prefixSlave ^ "saltello" in
				    setAnim ~o:p.hp_objSlave ~animName ~restartIfSameAnimation:false in
				(* 			  print_endline ("isNull2d velDir"); *)
				( m.hpsms_pos, p.hp_fatigueData, HasNotAsked, [], newObLead, newObSlave )
			  | Some velDir ->
				assert (not (isNull2d velDir));
				
				let ds , fatigueData', askedSprintPrev', sounds = 
				    let speedFreeRun = 
					if AI.isAttacking m.hpsms_pos then
					    p.hp_pc.pc_maxSpeedInFreeRunUnderNet
					else
					    p.hp_pc.pc_maxSpeedInFreeRunStayBack
				    in
				    (* formula: ds = v * dt *)
				    let canSprint cm fatigueData =
					if cm < fatigueData.fatigueAvailableSprintDistance then
					    Some {fatigueData with 
						      fatigueAvailableSprintDistance = 
						    fatigueData.fatigueAvailableSprintDistance -. cm }
					else
					    None in
				    
				    let v = (speedFreeRun  /. p.hp_fatigueData.fatigueDivisor) in

				    if wouldLikeToSprint then
					let dsDesired = 
					    let beta = 
						if p.hp_playsInTopmostCourtHalf then
						    smallestAngleWithZAxis velDir
						else
						    smallestAngleWithNegativeZAxis velDir in
					    dt *. sprintSpeed  ~beta ~speedFreeRun in

					let maybe = canSprint dsDesired   p.hp_fatigueData in

					let sou = 
					    match m.hpsms_askedToSprintInPrevFrame with
						| HasNotAsked ->
						      (match maybe with
							   | None ->
								 [SoundSprintCantBegin ]
							   | _ ->
								 [SoundSprintJustBegun])
						| HasAskedAndWasDenied ->
						      []
						| HasAskedAndObtained _ ->
						      (match maybe with
							   | None ->
								 [SoundSprintJustFinished ]
							   | _ ->
								 [])
					in

					match maybe with 
					    | None -> 
						  (v *. dt, p.hp_fatigueData, HasAskedAndWasDenied, sou)
					    | Some fatigueData' ->
						  (dsDesired, fatigueData', HasAskedAndObtained velDir, sou)
				    else
					(v *. dt, p.hp_fatigueData, HasNotAsked, []) 
				in
				let newPos3 = 
				    let newPos2 = 
					let newPos =
					    let angle = smallestAngleWithNegativeZAxis velDir in
					    let spostamentoX = 
						let signX = if velDir.x2 > 0.0 then 1.0 else -. 1.0 in
						signX *. ds *. (sin angle) in
					    let spostamentoZ = ds *. (cos angle) in
					    vec2dCreate (m.hpsms_pos.x2 +. spostamentoX) (m.hpsms_pos.z2-. spostamentoZ) in
					if p.hp_playsInTopmostCourtHalf && newPos.z2 > -. 50.0 then
					    vec2dCreate newPos.x2 (-. 50.0) 
					else
					    newPos in
				    if (not p.hp_playsInTopmostCourtHalf) && newPos2.z2 <  50.0 then
					vec2dCreate newPos2.x2 50.0
				    else
					newPos2 in
				let walkAni = 
				    match askedSprintPrev' with
					| HasAskedAndObtained _ -> 
					      computeWalkAnim ~footTarget:newPos3 ~curPos:m.hpsms_pos ~dirsign  
					| HasNotAsked | HasAskedAndWasDenied -> "attesa"
				in
				
				let newObLead = 
				    let animName = prefixLead ^ walkAni in
				    setAnim ~o:p.hp_objLeading ~animName ~restartIfSameAnimation:false in
				let newObSlave = 
				    let animName = prefixSlave ^ walkAni in
				    setAnim ~o:p.hp_objSlave ~animName ~restartIfSameAnimation:false in


				(newPos3, fatigueData', askedSprintPrev', sounds, newObLead, newObSlave)
		  in

		  let newst, newObLead, newObSlave, sounds =
		      match b.b_state with

			  | BS_Still _ ->
				( HPS_ManualSearch { hpsms_pos = newPos;
						     hpsms_realizing = Realized;
						     hpsms_askedToSprintInPrevFrame  = newSprint;
						     hpsms_diveIsPossibleNow = DiveNotNeeded;
						     hpsms_diveHasEverBeenPossible = DivePossibilityUnknown}, 
				  newObLead,
				  newObSlave, 
				  newSounds)

			  | BS_Moving bsm ->
				ifBallIsTowardsYouThenStartAutoSearch ~newObSlave ~newObLead ~newPos ~newSounds
				    ~bsm ~realizationState:m.hpsms_realizing ~newFatigueData
				    ~diveHasEverBeenPossible:m.hpsms_diveHasEverBeenPossible ~newSprint
		  in
		  (newst, newObLead, newObSlave, b, mouse2, newFatigueData, sounds)
		      
	    | HPS_ServingBeforeLaunch (right, pos) ->
		  
		  let newst, mouse2 =
		      if p.hp_objLeading.o3d_animState = PausedDuringService then
			  let angle = 
			      if right then
				  let dir = 
				      let src = curPosOfHumanPlayer p in
				      let dest = 
					  vec2dCreate (-. dirsign *. courtWt4) 
					      (-. dirsign *. (courtHt4 +. 50.0)) in
				      vec2dSub dest  src in
				  if p.hp_playsInTopmostCourtHalf then
				      -. (smallestAngleWithZAxis dir)
				  else
				      -. (smallestAngleWithNegativeZAxis dir)
			      else
				  let dir = 
				      let src = curPosOfHumanPlayer p in
				      let dest = 
					  vec2dCreate ( dirsign *. courtWt4) 
					      (-. dirsign *. (courtHt4 +. 50.0)) in
				      vec2dSub dest  src in
				  if p.hp_playsInTopmostCourtHalf then
				      smallestAngleWithZAxis dir
				  else
				      smallestAngleWithNegativeZAxis dir
			  in
			  (HPS_ServingAfterLaunchAndBeforePressingButton (right, angle, pos),
			   {mouse with m_secondsSinceLastMouseMotion = mouse.m_secondsSinceLastMouseMotion +.dt})
		      else
			  (HPS_ServingBeforeLaunch (right, pos), mouse) in
		  let b2 = 
		      if p.hp_objLeading.o3d_animState = PausedDuringService then
			  let traj =
			      { impact = vec3dCreate pos.x2 171.0 (dirsign*.(courtHt2 +. 20.0));
				startVel = vec3dCreate 0.0 610.0 0.0;
				spin = vec3dNull;
				targetRect = None}in
			  (
			   createRunningBall ~traj
			       ~scoreIndexOfLastPlayerWhoHit:b.b_siolpwhtb
			       ~polyBall:b.b_polygon ~service:true
			       ~polyRedCross:b.b_redCrossPolygon ~polyShadow:b.b_shadowPolygon)
		      else
			  b
		  in
		  (newst, p.hp_objLeading, p.hp_objSlave , b2, mouse2, p.hp_fatigueData, [])

	    | HPS_ServingAfterLaunchAndBeforePressingButton (right, ang, pos) ->
		  
		  let state2, objLead2,objSlave2, b2 =
		      let ang = 
			  let spostam =
			      let deltaX = 

				  let forseFlippato x = 
				      let remoteSign = 
					  match serverData with 
					      | Client _ -> -. 1.0
					      | Server _ | NeitherServerNorClient -> 1.0 
				      in

				      if p.hp_playsInTopmostCourtHalf then
					  -. x *. remoteSign
				      else
					  x *. remoteSign 
				  in
				  forseFlippato (float_of_int mouse.m_xRel) in
			      deltaX *. dt *. 0.09 in
			  let filtrato ang = 
			      if ang > pi_2 then
				  pi_2
			      else if ang < -. pi_2 then
				  -. pi_2
			      else
				  ang in
			  filtrato (ang +. spostam) in
		      let bally = (curBallPos b).y3 in
		      let ballIsGoingDownOrStill = 
			  match b.b_state with
			      | BS_Still _ -> true
			      | BS_Moving bsm ->
				    (curBallVel bsm).y3 < 0.0 in
		      if bally < 170.0 && ballIsGoingDownOrStill  then
			  let state2, objLead2, objSlave2 = startServiceHuman ~scoreIsEven:right ~h:p ~serverData in
			  let invisiblePos = vec3dCreate 0.0 (6200.0) 6200.0 in
			  state2, objLead2, objSlave2, { b with b_state = BS_Still invisiblePos}
		      else if mouse.m_leftButtonPressed || mouse.m_rightButtonPressed then
			  let h = {
			      hpssapb_FirstService = mouse.m_leftButtonPressed;
			      hpssapb_ToTheRight = right;
			      hpssapb_AimAngle = ang;
			      hpssapb_Timer = 0.0;
			      hpssapb_pos = pos } in
			  let oLead2 = {p.hp_objLeading with o3d_animState = Animated 0.0;
					    o3d_curFrameIdx = p.hp_objLeading.o3d_curFrameIdx +1}in
			  let oSlave2 = {p.hp_objSlave with o3d_animState = Animated 0.0;
					     o3d_curFrameIdx = p.hp_objSlave.o3d_curFrameIdx +1}in
			  (HPS_ServingAfterPressingButton h, oLead2, oSlave2, b)
		      else
			  (HPS_ServingAfterLaunchAndBeforePressingButton (right, ang, pos), p.hp_objLeading, p.hp_objSlave, b)
		  in
		  let mouse2 = {mouse with m_secondsSinceLastMouseMotion = mouse.m_secondsSinceLastMouseMotion +. dt } in
		  (state2, objLead2, objSlave2, b2, mouse2, p.hp_fatigueData, [])
		      

	    | HPS_ServingAfterPressingButton h ->
		  let newT = h.hpssapb_Timer +. dt in
		  let state2 = 
		      if newT >= durationOfCurAnimUpToImpactFrame ~o:p.hp_objLeading then
			  HPS_ServingAfterHittingBall  h.hpssapb_pos
		      else
			  HPS_ServingAfterPressingButton 
			      {h with
				   hpssapb_Timer = newT}
		  in
		  let b2 , sounds= 
		      if newT >= durationOfCurAnimUpToImpactFrame ~o:p.hp_objLeading then
			  let traj =
			      let angX = 
				  if h.hpssapb_FirstService then
				      degToRad p.hp_pc.pc_firstServiceXAngleInDeg
				  else
				      degToRad p.hp_pc.pc_secondServiceXAngleInDeg in
			      let velservizio = 
				  if h.hpssapb_FirstService then
				      cmPerSecondOfKmh p.hp_pc.pc_firstServiceSpeedKmh
				  else
				      cmPerSecondOfKmh p.hp_pc.pc_secondServiceSpeedKmh in
			      let velxz = velservizio *. (cos angX) in
			      let vely = velservizio *. (sin angX) in
			      let ang = h.hpssapb_AimAngle in
			      let velx = velxz *. (sin ang) *. dirsign in
			      let velz = -. velxz *. (cos ang) *. dirsign in
			      let rett = 
				  if not p.hp_playsInTopmostCourtHalf then
				      if h.hpssapb_ToTheRight then
					  servizioInAltoSulPari
				      else
					  servizioInAltoSulDispari
				  else
				      if h.hpssapb_ToTheRight then
					  servizioInBassoSulPari
				      else
					  servizioInBassoSulDispari in		    
			      let spi = if h.hpssapb_FirstService then
				  p.hp_pc.pc_firstServiceSpin
			      else
				  p.hp_pc.pc_secondServiceSpin in
			      { impact = curBallPos b;
				startVel = vec3dCreate velx (-. vely) velz;
				spin = vec3dCreate
				      ( spi *. dirsign *. (sin h.hpssapb_AimAngle)) 
				      spi
				      (-. spi *. (cos h.hpssapb_AimAngle) *. dirsign);
				targetRect = Some rett
			      } in

			  let sou = 
			      if h.hpssapb_FirstService then
				  [ SoundNormalShot; SoundAhh] 
			      else 
				  [ SoundNormalShot; SoundHff]
			  in
			  print_endline ("Service impact height: " ^ string_of_float traj.impact.y3 );

			  (createRunningBall ~traj
			       ~scoreIndexOfLastPlayerWhoHit:p.hp_scoreIndex
			       ~polyBall:b.b_polygon
			       ~polyRedCross:b.b_redCrossPolygon
			       ~polyShadow:b.b_shadowPolygon ~service:true, 
			   sou)
			      
		      else
			  b, []
		  in
		  (state2, p.hp_objLeading, p.hp_objSlave, b2, mouse, p.hp_fatigueData, sounds)

	    | HPS_ServingAfterHittingBall  pos ->

		  let state2 =
		      if p.hp_objLeading.o3d_animState = NotAnimated then
			  HPS_ManualSearch {hpsms_pos = pos;
					    hpsms_realizing = NotRealized;
					    hpsms_askedToSprintInPrevFrame = HasNotAsked;
					    hpsms_diveIsPossibleNow =  DiveNotNeeded;
					    hpsms_diveHasEverBeenPossible = DivePossibilityUnknown}
		      else
			  HPS_ServingAfterHittingBall  pos in
		  let oLead2 = 
		      if p.hp_objLeading.o3d_animState = NotAnimated then
			  setAnim ~o:p.hp_objLeading ~animName:(prefixLead ^ "saltello")
			      ~restartIfSameAnimation:true
		      else
			  p.hp_objLeading in

		  let oSlave2 = 
		      if p.hp_objSlave.o3d_animState = NotAnimated then

			  setAnim ~o:p.hp_objSlave ~animName:(prefixSlave ^ "saltello")
			      ~restartIfSameAnimation:true
		      else
			  p.hp_objSlave in
		  (state2, oLead2, oSlave2, b, mouse, p.hp_fatigueData, [])
		      


    in
    let newPlayer = { p with
			  hp_objLeading = newObjLead2;
			  hp_objSlave = newObjSlave2;
			  hp_state = newState;
			  hp_fatigueData = fatigueData'
		    } in
    let newPlayer2 =
	{ newPlayer with
	      hp_fatigueData = (updateFatigue newPlayer.hp_fatigueData (curPosOfHumanPlayer newPlayer))
	} in
    (newBall, HP newPlayer2, newMouse, soundIds)

(* se l'avversario ha lockato, i dati che mi interessano sono il punto
   in cui colpira' e quando.  Se invece non ha lockato, stimero' il
   punto. *)
type lockInfo = HasLocked of vec2d * float | HasNotLocked 





type impactVote2 = VoteNonZero of float * volleyOrIntention 
		  | VoteZero



type trajData = { td_distance:float; td_speedAtBounce:float; td_bounce:vec2d; 
		  td_neededPower:float; td_netHt:float; td_maxHt:float;
		  td_impact:vec3d}


let offsFromCenterToJustifyKillingDownTheLIne = 180.0 




let calcTrajData ~tr ~iAmOpening ~opp ~oppDir ~availablePowerForMyShot = 

    let opp = 
	let foreseenMovement = 
	    (* distance the opponent will presumably cover while I am doing the gesture *)
	    228.0 in
	let deltaToApplyPrevision = 170.0 in
	assert(deltaToApplyPrevision < offsFromCenterToJustifyKillingDownTheLIne);
	if iAmOpening then
	    if opp.x2 < -. deltaToApplyPrevision then
		{ opp with x2 = opp.x2 +. foreseenMovement}
	    else if opp.x2 > deltaToApplyPrevision then
		{ opp with x2 = opp.x2 -. foreseenMovement}
	    else
		opp
	else
	    opp
    in
    let maybe = whenWillTheTrajectoryHitTheNet tr in
    match maybe with
	| Some _ ->  
	      None
	| None ->
	      
	      let neededPower = length3d tr.startVel in
	      if availablePowerForMyShot < neededPower then
		  None
	      else
		  let distance = 
		      (* @@ wrong. If the ball is a dropshot, the
			 calculation is wrong. We should take the second
			 bounce into account *)
		      let opponentImpact = 
			  let ballLine = 
			      straightLineFromPointAndDir (projection2d tr.impact)
				  (projection2d tr.startVel) in
			  intersectionOfStraightLines oppDir ballLine in
		      
		      distance2d opp opponentImpact in
		  let speedAtBounce = 
		      let t = whenWillItBounce tr in
		      let vx = 
			  tr.startVel.x3 -. tr.spin.x3 *. t in
		      let vz = 
			  tr.startVel.z3 -. tr.spin.z3 *. t in
		      length2d (vec2dCreate vx vz ) in
		  let bounce = whereWillItBounce tr in
		  let deltaNet =
			  let may = whenWillTheTrajArriveAtZ ~z:0.0 ~t:tr in
			  match may with
			      | None -> assert false
			      | Some iata ->
				    let ballYOverNet = tr.impact.y3 +. tr.startVel.y3 *. iata.iata_t +.
					0.5 *. ( -. tr.spin.y3 -. abs_float g) *. iata.iata_t *. iata.iata_t in
 				    let netHtThere = netHtAtX(iata.iata_x) in
				    abs_float (netHtThere -. ballYOverNet) in


		  let maxHt = 
		      let tmaxht =
			  let whenWillReachMaximumHt ~y0 ~v0y ~sy = v0y /. ( sy +. abs_float g) in
			  whenWillReachMaximumHt ~y0:tr.impact.y3 ~v0y:tr.startVel.y3 ~sy:tr.spin.y3 in

		      let tmaxht = if tmaxht < 0.0 then 0.0 else tmaxht in

		      tr.impact.y3 +. tr.startVel.y3 *. tmaxht +.
			  0.5 *. ( -. tr.spin.y3 -. abs_float g) *. tmaxht *. tmaxht in


		  Some { td_maxHt = maxHt;
			 td_netHt = deltaNet;
			 td_impact = tr.impact;
			 td_bounce = bounce;
			 td_speedAtBounce = speedAtBounce;
			 td_distance = distance;
			 td_neededPower = neededPower}





type qvd = {qvd_quality: int; qvd_vote:float; qvd_descr: string}

let judgeTrajectory ~p ~td   ~surf ~iAmOpening ~researchKind
	~canDoTopSpin ~opp ~myPosAtImpact =


    let truncateBetween0And10 v = 
	if v > 10.0 then 10.0 else if v < 0.0 then 0.0 else v in
    let voteDistance = 
	truncateBetween0And10(AI.interpolateVote td.td_distance 200.0 courtWt 0.0 10.0) in

    let voteSpeed = 
	truncateBetween0And10 (AI.interpolateVote td.td_speedAtBounce 0.0 4500.0 0.0 10.0) in
    let voteSloth = 10.0 -. voteSpeed in

    let voteLen = 
	truncateBetween0And10(AI.interpolateVote (abs_float td.td_bounce.z2) 0.0 courtHt2 0.0 10.0)in
    let voteNetHt = 
	truncateBetween0And10(AI.interpolateVote td.td_netHt 0.0 60.0 10.0 0.0 ) in
    
    let voteMaxHt =  
	truncateBetween0And10(AI.interpolateVote td.td_maxHt 0.0 (td.td_impact.y3 +. 200.0) 10.0 0.0) 
    in
    
    match researchKind with
	| RKC_Smash _ ->
 	      {qvd_quality = 1; 
	       qvd_vote = voteSpeed *. 0.8 +. voteDistance *. 0.2;
	       qvd_descr = "RKC_Smash" }
	      
	| RKC_StretchForward Volley | RKC_Normal VOTI_Volee ->

	      let quality = if abs_float td.td_bounce.z2 <= courtHt4 then 2 else 1 in
 	      if AI.isInNoMansLand opp then 
 		   {qvd_quality = quality; 
		    qvd_vote = voteMaxHt *. 0.5 +. voteLen *. 0.3 +.  voteDistance *. 0.2;
		    qvd_descr = "I am doing a volley, opponentIsInNoMansLand" }
	      else if abs_float myPosAtImpact.z2 > courtHt4 *. 0.68 then
 		   {qvd_quality = quality; 
		    qvd_vote = voteMaxHt *. 0.5 +. voteLen *. 0.3 +.  voteDistance *. 0.2;
		    qvd_descr = "I am doing a volley, but I am far from the net" }
	      else
		   {qvd_quality = quality;
		    qvd_vote = voteMaxHt *. 0.6 +.  voteDistance *. 0.4;
		    qvd_descr = "I am doing a volley, I am close to the net . maxHt = " ^ string_of_float td.td_maxHt ^ ",voteMaxHt = " ^
			   string_of_float voteMaxHt ^ ", impactht = " ^ string_of_float td.td_impact.y3 ^
			   ", speedAtBounce = " ^ string_of_float td.td_speedAtBounce ^ ", impact.z = " ^
			   string_of_float td.td_impact.z3}
		      

	| RKC_StretchForward NotVolley intent | RKC_Normal VOTI_NotVolee (_, intent) ->

	      if AI.isAttacking opp then

		  if td.td_distance < 290.0 || td.td_speedAtBounce < 2500.0 then
		       {qvd_quality =  2;
			qvd_vote = voteNetHt *. 0.07 +. voteSpeed *. 0.91 +. voteDistance *. 0.02;
			qvd_descr = "passingShot, low distance "}
		  else
		      (* voteNetHt is dangerous here: the player risks to ignore a 
			 passingshot that travels outside, preferring diagonal *)
		       {qvd_quality = 1;
			qvd_vote = voteSpeed *. 0.95 +. voteDistance *. 0.05 ;
			qvd_descr = "passingShot, distance good. speedatbounce = " ^ string_of_float td.td_speedAtBounce}


	      else (* both players are back *)
		  
		  ( match intent with
			| StayBack ->

			      let mayb = pick [Left;Right] (fun d -> AI.isQuiteDecentered d myPosAtImpact) in

			      (match mayb with

				   | None -> (* I am centered *)

					 if AI.isVeryDecenteredBackwards opp then
					     if AI.isABitDecentered Left opp then
						 let qual = if td.td_bounce.x2 <= 200.0 then 2 else 1 in
						 {qvd_quality = qual;
						  qvd_vote = voteDistance *. 0.5 +. voteSpeed *. 0.5;
						  qvd_descr = "debugPlace 25"}
					     else if AI.isABitDecentered Right opp then
						 let q = if td.td_bounce.x2 >= -. 200.0 then 2 else 1 in
						 {qvd_quality = q;
						  qvd_vote = voteDistance *. 0.5 +. voteSpeed *. 0.5;
						  qvd_descr = "debugPlace 27"}
					     else
						 {qvd_quality = 1;
						  qvd_vote = voteDistance *. 0.4 +. voteSpeed *. 0.6;
						  qvd_descr = "opponent is backwards but centered"}

					 else if AI.isInNoMansLand opp then
					      {qvd_quality = 1;
					       qvd_vote = voteDistance *. 0.3 +. voteSpeed *. 0.7;
					       qvd_descr = "opponentIsInNoMansLand: distance and speed"}
					 else
					     (* both players are centered *)

					     let isClay = 
						 match surf with Clay -> true | Cement | Grass -> false in
					     if not isClay && td.td_distance > courtWt2 &&
						 td.td_speedAtBounce > 2900.0 (*check whether it is > @@ *) &&
						 (not p.cp_pc.pc_prefersVolleysToGroundShots)
					     then
						 
						 {qvd_quality = 1;
						  qvd_vote = voteDistance *. 0.4 +. voteSpeed *. 0.6;
						  qvd_descr = "both centered, debugPlace 30"}
					     else
						 (match p.cp_name with
						      | Pete ->
							    let q = if td.td_distance <= 210.0 then 2 else 1 in
							    {qvd_quality = q;
							     qvd_vote = voteDistance *. 0.1 +. voteSpeed *. 0.9 ;
							     qvd_descr = "pete interlocutory shot"}


						      | Ivan ->
							    let q = if td.td_distance <= 210.0 then 2 else 1 in
							    {qvd_quality = q;
							     qvd_vote =( match surf with 
									     | Clay ->
										   voteDistance *. 0.05 +. voteSpeed *. 0.95
									     | Grass| Cement -> 
										   voteDistance *. 0.3 +. voteSpeed *. 0.7);
							     qvd_descr = "ivan normal interlocutory shot"}

						      | Mats ->
							    
							    let q = if abs_float td.td_bounce.z2 <= courtHt4 *. 1.5 then 2 else 1 in
							    (* no voteLen, because it clashed with distance *)
							    {qvd_quality = q;
							     qvd_vote = voteDistance *. 0.5 +. voteSpeed *. 0.5;
							     qvd_descr = "both are centered, mats interlocutory shot"}
						 )

				   | Some dir -> (* I am decentered in direction dir *)
					 
					 let lrSign = match dir with Left -> 1.0 | Right -> -. 1.0 in

					 if AI.isABitDecentered dir opp || 
					     (AI.isVeryDecenteredBackwards opp  && 
						  not (AI.isABitDecentered (oppositeDir dir) opp))
					 then
					     (* diagonale a chiudere *)
					     let q = if lrSign *. td.td_bounce.x2 -. 200.0 < 0.0 then 2 else 1 in
					     (* e' diag *)
					     {qvd_quality = q;
					      qvd_vote = voteDistance *. 0.7 +. voteSpeed *. 0.3;
					      qvd_descr = "I am decentered left, opponent decentered left or back: power diagonal "}

					 else if AI.isQuiteDecentered  (oppositeDir dir) opp then
					     (* downTheLine a chiudere *)

					     let q = if lrSign *. td.td_bounce.x2 +. 200.0 > 0.0  then 2 else 1 in
					     {qvd_quality = q;
					      qvd_vote = voteDistance *. 0.7 +. voteSpeed *. 0.3;
					      qvd_descr = "i am decentered left, opponent quite decentered right: downTheLine "}
					 else
					     (* I am decentered and the opponent centered *)
					     let doLongLowDiag = 

						 let isDiagonal = 
						     (* You may think not too diagonal, because I give a lot of angle.
							This is wrong, because I am decentered in "dir", so I give angle
							to aim where I already am *)
						     lrSign *. td.td_bounce.x2 -. 202.0 > 0.0 in

						 if not isDiagonal then
						     {qvd_quality = 3;
						      qvd_vote = 1.0 ;
						      qvd_descr = ">>>>>WARNING: Not diagonal should not have been chosen<<<<"}
						 else
						     (* This is difficult to balance. it must surely
							be long, in order not to give angle. It must be low, for the same reason.

							but long and slow tends to produce fast balls. And this is bad.
							It should not be too fast, 
							to give me time to get back to center. But not too slow either:
							otherwise the opponent plays a killing volee. So, how slow? Found by trials:
							the lowest speed such that the player did not hit a volee.*)

						     if td.td_speedAtBounce < 1800.0 then
							 {qvd_quality = 2;
							  qvd_vote = td.td_speedAtBounce;
							  qvd_descr = ">>>WARNING: too slow, should not have been chosen <<<<"}

						     else if td.td_speedAtBounce > 3500.0 then
							 {qvd_quality = 2;
							  qvd_vote = td.td_speedAtBounce;
							  qvd_descr = ">>>WARNING: too fast, should not have been chosen <<<<"}
						     else
							 let fores = 
							     if iAmOpening then
								 " - foreseen opponent x = " ^ string_of_float opp.x2
							     else
								 " - final opponent x = " ^ string_of_float opp.x2
							 in
							 {qvd_quality = 1;
							  qvd_vote = voteLen *. 0.8 +. voteMaxHt *. 0.2;
							  qvd_descr = "iAmDecentered, opponent is centered. Do long, low diagonal. speedAtBounce = " ^ 
								 string_of_float td.td_speedAtBounce ^ fores}

					     in
					     let killingShotOrSlowDiag = 
						 let tryTheKillingDownTheLine  (* @@ move outside! *)=
						     let iAmCloseToTheSideLine = 
							 lrSign *. myPosAtImpact.x2 +. courtWt2 +. 50.0 > 0.0 in
						     
						     canDoTopSpin &&
							 lrSign *. opp.x2 -. offsFromCenterToJustifyKillingDownTheLIne > 0.0 && 
							 iAmCloseToTheSideLine in
						 
						 if tryTheKillingDownTheLine then
						     let isKilling =
							 lrSign *. td.td_bounce.x2 < 0.0
							 && (abs_float (abs_float td.td_bounce.x2 -. courtWt2)) < 150.0
						     in
						     
						     if not isKilling then
							 {qvd_quality = 2;
							  qvd_vote = 1.0;
							  qvd_descr = ">>>WARNING: not isKilling, should not have been chosen <<<"}
						     else
							 let str =
							     if iAmOpening then
								 " foreseen opponent x = " ^ string_of_float opp.x2
							     else
								 " opponent  x = " ^ string_of_float opp.x2
							 in
							 {qvd_quality = 1;
							  qvd_vote = voteLen *. 0.15 +. voteDistance *. 0.05 +. voteSpeed *. 0.8;
							  qvd_descr = "iAmDecenteredLeft,  not opponentIsDecenteredLeft, killingshot." ^ str}
						 else
						     doLongLowDiag

					     in

					     ( match p.cp_name with
						   | Mats ->
							 killingShotOrSlowDiag
						   | Ivan ->

							 killingShotOrSlowDiag



						   | Pete ->
							 killingShotOrSlowDiag

					     )
			      )



			| AttackApproach ->
			      (* slow is better, but not slower than a certain value *)
			      let quality = if td.td_speedAtBounce <= 1400.0 (* 2200.0 originally *)  then 2 else 1 in
			      {qvd_quality = quality;
			       qvd_vote = voteDistance *. 0.1 +. voteLen *. 0.25 +. voteSloth *. 0.4 +. voteNetHt *. 0.25;
			       qvd_descr = "AttackApproach. speedAtBounce = " ^ string_of_float td.td_speedAtBounce}

			| AttackWithPowerShot ->
			      {qvd_quality = 1;
			       qvd_vote = voteDistance *. 0.2 +. voteSpeed *. 0.8;
			       qvd_descr = "AttackWithPowerShot"}
		  )
				


let chooseTrajectory ~p  ~impact ~opponentPos ~ballVelAtImpact
	~canIDoTopSpin ~canIDoBackSpin ~surf ~targetRect ~myPosAtImpact ~iAmOpening 
	~aidebug ~researchKind =
    


    assert(canIDoBackSpin || canIDoTopSpin);
    
    let myPos = curPosOfComputerPlayer p in
    let opponentDir = {sl_a = 0.0 ; sl_b = 1.0; sl_c = -. opponentPos.z2} in


    let triples (*  a triple is (bounc, (spin, ht))   *)=
	let pairsSpinAndHt = 
	    let possibleSpins = 
		match researchKind with
		    | RKC_Smash _ -> [spinForVolee]
		    | RKC_StretchForward _ -> [spinForVolee]
		    | RKC_Normal VOTI_Volee -> [spinForVolee]
		    | RKC_Normal VOTI_NotVolee _ ->
			  let backpart = if canIDoBackSpin then [p.cp_pc.pc_backSpin] else [] in
			  let toppart = if canIDoTopSpin then [p.cp_pc.pc_topSpin] else [] in
			  append backpart toppart 
	    in
	    
	    let lowestHt = netHtBorder -. 9.0 in
	    let possibleHeightsLow = 
		let htOfIndex i = 
		    lowestHt +. i *. 8.0 
		and indices = List.map (fun x -> float_of_int x ) (listFromTo 0 10 ) in
		List.map htOfIndex indices in
	    
	    let possibleHeightsHi = 
		let htOfIndex i = 
		    lowestHt +. 80.0  +. i *. 25.0 
		and indices = List.map (fun x -> float_of_int x ) (listFromTo 0 8 ) in
		List.map htOfIndex indices in
	    
	    allPairs possibleSpins (append possibleHeightsLow possibleHeightsHi) in

	let bouncePoints =
	    let listOfLists = 
		let listOfDist dis =
		    let veryLongBounce = 
			let y = min ( courtHt4 +. 500.0) ( courtHt2 -. dis) in
			[ vec2dCreate (   courtWt2 -. dis) y;
			  vec2dCreate (-. courtWt2 +. dis) y ]
		    in
		    append veryLongBounce
			[ vec2dCreate (   courtWt2 -. dis) (courtHt4 -. 320.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 -. 320.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 -. 240.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 -. 240.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 -. 160.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 -. 160.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 -. 80.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 -. 80.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 -. 0.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 -. 0.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 +. 100.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 +. 100.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 +. 200.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 +. 200.0);
			  
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 +. 300.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 +. 300.0);
			  
			  vec2dCreate (   courtWt2 -. dis) (courtHt4 +. 400.0);
			  vec2dCreate (-. courtWt2 +. dis) (courtHt4 +. 400.0);
			  
			  
			  
			  (* central *)
			  vec2dCreate 0.0 (courtHt2 -. dis);
			  vec2dCreate     100.0  (courtHt2 -. dis);
			  vec2dCreate (-. 100.0) (courtHt2 -. dis);
			  
			  vec2dCreate     200.0  (courtHt2 -. dis);
			  vec2dCreate (-. 200.0) (courtHt2 -. dis)
			]
		in
		if aidebug then
		    
		    List.map listOfDist [p.cp_distanceOfBounceFromLine  ; 
					 p.cp_distanceOfBounceFromLine /. 2.0;
					 p.cp_distanceOfBounceFromLine /. 3.0;
					 p.cp_distanceOfBounceFromLine *. 2.0;
					 p.cp_distanceOfBounceFromLine *. 1.5] 
		else
		    let dis = 
			if AI.isAttacking opponentPos then
			    min 135.0 ( p.cp_distanceOfBounceFromLine *. 1.5 )
			else if AI.isAttacking myPos then
			    min 140.0 ( p.cp_distanceOfBounceFromLine *. 1.5 )
			else
			    p.cp_distanceOfBounceFromLine
		    in
		    List.map listOfDist [dis]
	    in
	    List.flatten listOfLists
	in (* end bouncePoints *)
	
    	let maybeFlipY l =
    	    if impact.z3 > 0.0 then  List.map (fun x -> vec2dCreate x.x2 (-. x.z2) ) l
    	    else l in
	allPairs (maybeFlipY bouncePoints)  pairsSpinAndHt
    in (* end triples *)

    let trajs = select_some (List.map 
				 (fun (bounc, (spin, ht)) -> 
				      buildTrajFromTwoPointsAndHeight ~impact
					  ~htOverNet:ht ~spin ~bounceDesired:bounc ~targetRect) 
				 triples) in
    
    let availablePowerForMyShot = 
	let po = 
	    match researchKind with
		| RKC_Normal VOTI_Volee -> p.cp_pc.pc_maxShotPower  *. powerAttenuationForVolee
		| RKC_Normal VOTI_NotVolee _  | RKC_Smash _ -> p.cp_pc.pc_maxShotPower 
		| RKC_StretchForward _ -> p.cp_pc.pc_maxShotPower *. powerAttenuationForStretchForwardAndDive
	in
 	po  +. (abs_float ballVelAtImpact.z3) *. p.cp_pc.pc_exploitationOfOpponentsPower 
    in
    let maybeTrajDatas = 
	List.map
	    (fun tr -> calcTrajData ~tr ~iAmOpening ~opp:opponentPos ~oppDir:opponentDir
		 ~availablePowerForMyShot  ) 
	    trajs in
    
    
    let trajsWithVote = 
	
	let trajAndVoteOfPair (tr, td) = 
	    (match td with
		 | None -> None
		 | Some td' ->
		       let qvd = 
			   judgeTrajectory ~p ~td:td' 
			       ~opp:opponentPos ~iAmOpening ~canDoTopSpin:canIDoTopSpin
			       ~surf ~myPosAtImpact ~researchKind in
		       Some ( qvd , tr) ) 
	and pairs = List.combine trajs maybeTrajDatas in
	select_some (List.map trajAndVoteOfPair pairs) in

    if length trajsWithVote = 0 then 
	None
    else
	let (qvdBest, trBest) = 
		   
	    let better (qvd1, x1) (qvd2, x2) = 
		if qvd1.qvd_quality < qvd2.qvd_quality then
		    (qvd1, x1)
		else if qvd1.qvd_quality > qvd2.qvd_quality then
		    (qvd2, x2)
		else
		    if qvd1.qvd_vote > qvd2.qvd_vote then qvd1, x1 else qvd2, x2
	    in
	    
	    (findBestElement trajsWithVote better) in
	Some ( qvdBest.qvd_descr  , trBest)


type impactVote = IV_Zero | IV_NonZero of int * float * volleyOrIntention

let judgeImpactPointComp ~cp ~isVolley ~opponentCurPos ~impact
	~footTarget ~mat  ~isService ~iWantToAttack =



    if impact.y3 <= 0.1 then
	IV_Zero
    else if cp.cp_playsInTopmostCourtHalf  && impact.z3 > -. minZIcanHitTheBall then
	IV_Zero
    else if (not cp.cp_playsInTopmostCourtHalf) && impact.z3 < minZIcanHitTheBall then
	IV_Zero
    else
	let iAmForcedToAttack = 
	    abs_float impact.z3 < courtHt4 *. 1.2 in
	if isVolley then
	    let v = AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt 
	    and q =
		if iAmForcedToAttack then
		    if iWantToAttack then 1 else (* give a chance to groundstrokes *) 2
		else
		    3 
	    in
	    IV_NonZero (q, v, Volley)
	else
	    if AI.isAttacking opponentCurPos || 
		isService (* @@ and he often attacks after service *) 
	    then
		let v = ((AI.voteNotTooMuchBehindGroundLine footTarget ) *. 0.26 +.
			     (AI.voteHorizontalCentering footTarget) *. 0.15 +.
			     (AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt) *. 0.59) in
		IV_NonZero (1, v, NotVolley StayBack)
	    else 
		(* the impact is a groundstroke and the opponent is not attacking *)
		let quality = 
		    if iWantToAttack  || iAmForcedToAttack then 2 else 1
		in
		let ac = 
		    let chooseAttackForImpactPoint ~cp  ~impact ~opponent ~mat  =

			let decideZtest = 
			    let maybeAttack = 
				let chooseKindOfAttack = 
				    let maybMe = pick [Right;Left] (fun d -> AI.isABitDecentered d footTarget) 
				    and maybeHim = pick [Right;Left] (fun d -> AI.isABitDecentered d opponent) 
				    in
				    match maybMe with
					| None -> 
					      (match maybeHim with
						   | None -> AttackApproach  
						   | Some _ -> AttackWithPowerShot)
					| Some d ->
					      (match maybeHim with
						   | None -> AttackApproach 
						   | Some q -> if q = d then AttackWithPowerShot else AttackApproach )
				in
				if iAmForcedToAttack then
				    AttackWithPowerShot
				else if iWantToAttack then
				    chooseKindOfAttack
				else
				    StayBack
			    in
			    if abs_float impact.z3  < courtHt2 -. 100.0 then
				maybeAttack
			    else
				StayBack
			in
			(* first, test the impact height : *)
			if abs_float impact.z3 > courtHt2 *. 0.68 then
			    if impact.y3 < 25.0 then 
				StayBack
			    else
				(* impact ht is enough, test the z *)
				decideZtest
			else
			    (* impact ht irrelevant, test the z *)
			    decideZtest

		    in (* chooseAttack *)

		    chooseAttackForImpactPoint  ~impact ~cp ~opponent:opponentCurPos ~mat in

		match ac with 
		    | StayBack ->
			  (* @@ ignoring pc_tendsToAnticipateGroundShots .

			     Strange. Sometimes, on grass, responding to service, the player 
			     goes very much backwards. Even if voteClosenessToGroundLine is the 
			     only parameter.
			  *)
			  (* 			  let q = if iWantToAttack then 3 else 1 in *)
			  let v = (AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt) *. 0.7 +.
			      (AI.voteClosenessToGroundLine footTarget) *. 0.3 in
			  IV_NonZero (  quality, v, NotVolley StayBack)

		    | AttackWithPowerShot ->
			  (* 			  let q =  *)
			  (* 			      if iWantToAttack then *)
			  
			  (* 				  if cp.cp_pc.pc_prefersVolleysToGroundShots (\*@@ *\)then *)
			  (* 				      2 *)
			  (* 				  else *)
			  (* 				      1 *)
			  (* 			      else *)
			  (* 				  3 *)
			  (* 			  in *)
			  let v = AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt *. 0.6 +.
			      (AI.voteClosenessToNet footTarget) *. 0.4 in
			  IV_NonZero ( quality, v, NotVolley AttackWithPowerShot)
			      
		    | AttackApproach ->
			  (* 			  let q =  *)
			  (* 			      if iWantToAttack then *)
			  (* 				  if cp.cp_pc.pc_prefersVolleysToGroundShots (\*@@ *\)then *)
			  (* 				      1 *)
			  (* 				  else *)
			  (* 				      2 *)
			  (* 			      else *)
			  (* 				  3 *)
			  (* 			  in *)
			  let v = AI.voteClosenessToNet footTarget in
			  IV_NonZero ( quality, v , NotVolley AttackApproach)


type researchJudgement = { rj_rbdts : rbdts; rj_quality: int; rj_vote:float}

(* for the computer player *)
let chooseBestResearch ~playerCurPos ~opponentCurPos
	~ballPos ~ballDir ~ball ~surf ~bsm ~p =

    let researches = 

	let judgeAllResearches ~p  ~playerCurPos ~opponentCurPos
		~ballPos ~ballDir ~ball ~bsm ~surf   = 

	    let theBallHasAlreadyCrossedTheNet =
		if p.cp_playsInTopmostCourtHalf then
		    (curBallPos ball).z3 < -. minZIcanHitTheBall
		else
		    (curBallPos ball).z3 > minZIcanHitTheBall
	    in
	    let numPointsToJudge = 180 in
	    
	    let secondPointForResearch = whereWillTheBallMakeSecondBounce ~b:ball ~bsm
		~surf in
	    let maybeFirstPointforResearch = 
		if theBallHasAlreadyCrossedTheNet  then
		    Some (projection2d (curBallPos ball))
		else
		    let fifty = if p.cp_playsInTopmostCourtHalf then -. minZIcanHitTheBall else
			minZIcanHitTheBall in
		    let may = whenWillTheTrajArriveAtZ ~z:fifty ~t:bsm.bsm_trajectory  in
		    match may with
			| None -> (* happens on short dropshot that bounces backwards :-) *) 
			      None
			| Some iata -> Some (vec2dCreate iata.iata_x fifty)
	    in
	    match maybeFirstPointforResearch with
		| None -> []
		| Some firstPointforResearch ->
		      let stepT = 1.0 /. (float_of_int numPointsToJudge) in
		      let l = (listFromTo 0 (numPointsToJudge - 1) ) in
		      let factors = List.map (fun x -> stepT *. (float_of_int x) ) l in
		      let impacts = List.map
			  (fun fac -> vec2dAdd firstPointforResearch
			       (vec2dMulScalar fac (vec2dSub secondPointForResearch firstPointforResearch))) 
			  factors 
		      in
		      let createResearchAndJudgeIt ~p ~impact ~playerCurPos ~ballPos 
			      ~ballDir ~ball ~s ~opponentCurPos ~surf ~iWantToAttack = 

			  let impactIsOutSideOfMyCourtSide = 
			      if p.cp_playsInTopmostCourtHalf then
				  impact.z2 > -. minZIcanHitTheBall
			      else
				  impact.z2 < minZIcanHitTheBall 
			  and speedFreeRunNoFatigue = 
			      (if AI.isAttacking playerCurPos then 
				   p.cp_pc.pc_maxSpeedInFreeRunUnderNet
			       else
				   p.cp_pc.pc_maxSpeedInFreeRunStayBack)
			  and speedNormSearchNoFatigue = 
			      (if AI.isAttacking playerCurPos then 
				   p.cp_pc.pc_maxSpeedInNormalResearchUnderNet
			       else
				   p.cp_pc.pc_maxSpeedInNormalResearchStayBack)
			  in

			  
			  if impactIsOutSideOfMyCourtSide then
			      None
			  else

			      let arrives =  whenWillTheBallArriveAtZ ~z:impact.z2 ~s ~surf in
			      
			      match arrives with
				  | None -> 
					None
				  | Some iaba ->

					let impact3d = vec3dCreate impact.x2 iaba.iaba_whatYItWillHave impact.z2 in
					let deltaT = iaba.iaba_timeFromImpactToArrival -. s.bsm_curTimer in
					let isVolley = s.bsm_bouncesSoFar = 0 && not iaba.iaba_itWillBounceFirst in 
					let t1 = iaba.iaba_timeFromImpactToArrival in
					let t0 = s.bsm_curTimer in
					let speedFreeRun = speedFreeRunNoFatigue
					    /. p.cp_fatigueData.fatigueDivisor in
					let speedNormalResearch = speedNormSearchNoFatigue
					    /. p.cp_fatigueData.fatigueDivisor in


					let isForehand = 
					    if p.cp_playsInTopmostCourtHalf then
						impact.x2 < playerCurPos.x2
					    else
						impact.x2 > playerCurPos.x2 in
					let footPosAtImpactTime ~deltaXFootRacket ~deltaZ= 
					    if p.cp_playsInTopmostCourtHalf then
						let z = impact.z2 -. deltaZ in
						if isForehand then
						    vec2dCreate (impact.x2 +. deltaXFootRacket) z
						else
						    vec2dCreate (impact.x2 -. deltaXFootRacket ) z
					    else
						let z = impact.z2 +. 80.0 in
						if isForehand then
						    vec2dCreate (impact.x2 -. deltaXFootRacket) z
						else
						    vec2dCreate (impact.x2 +. deltaXFootRacket) z
					in


					

					let footPosImpSmash = footPosAtImpactTime ~deltaXFootRacket:0.0 ~deltaZ:80.0 in
					let runDirSmash = vec2dSub footPosImpSmash playerCurPos in
					let runAngleSmash = 
					    (* NON CAMBIARE! Potresti pensare che se gioca in alto
					       devi usare l'asse Z negato, come fai per
					       l'umano. Invece no!  La cerca male se lo fai. *)
					    smallestAngleWithZAxis runDirSmash in
					let signXSmash = if runDirSmash.x2 > 0.0 then 1.0 else -. 1.0 in
					let deltaXSmash = distance2d footPosImpSmash playerCurPos in
					assert (deltaXSmash != 0.0);
					

					let voteAndIntentSmash = 

					    judgeImpactPointComp ~cp:p ~isVolley ~opponentCurPos ~impact:impact3d
						~footTarget:footPosImpSmash ~mat:surf.s_material
						~isService:s.bsm_lastShotWasAService  ~iWantToAttack

					in


					let computeDeltaOpening ~isForehand ~researchKind= 
					    let prefix =
						if p.cp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B" in
					    let animName = 
						match researchKind with
						    | RKC_Smash _ ->
							  prefix ^ "smash"
						    | RKC_StretchForward _ ->
							  prefix ^ if isForehand then "drittoforwardstretch" else "rovescioforwardstretch"

						    | RKC_Normal vt  ->
							  let core = 
							      if isForehand then 
								  match vt with
								      | VOTI_Volee -> "drittov"
								      | VOTI_NotVolee ( sp, _ )->
									    match sp with
										| Topspin -> "dritto"
										| Backspin -> "drittoback"
							      else
								  match vt with
								      | VOTI_Volee -> "rovesciov"
								      | VOTI_NotVolee ( sp, _) ->
									    match sp with
										| Topspin -> "rovescio"
										| Backspin -> "rovescioback"
							  in
							  prefix ^ core	      
					    in
					    let an = StringMap.find animName p.cp_obj.o3d_animations in
					    match an with 
						| ServiceAnimation s ->s.serviceAnim_TimeFromLaunchToImpact
						| ShotAnimation s ->s.shotAnim_TimeFromOpeningToImpact 
						| RunAnimation _ -> assert(false)
					in
					let calcResearch ~researchKind ~tChange ~v2 ~footPosImp ~runAngle ~signX ~deltaX
						~volleyOrInten =
					    let speedBefOpening = 
						if v2 < 0.0 then
						    let v1 = deltaX /. (tChange -. t0) in
						    vec2dCreate (signX *. v1 *. sin runAngle) 
							(v1 *. cos runAngle) (* @@ it is - v1 for humans. investigate. *)
						else
						    vec2dCreate (signX *. speedFreeRun *. sin runAngle) 
							(speedFreeRun *. cos runAngle)  (* @@ it is - speedFreeRun for humans*)  in		
					    let speedAftOpening = 
						if v2 < 0.0 then
						    vec2dCreate 0.0 0.0 
						else
						    vec2dCreate (signX *. v2 *. sin runAngle) 
							( v2 *. cos runAngle (* it is - v2 for humans *) )    in		
					    
					    { rbdts_forehand = isForehand;
					      rbdts_timeToRunBeforeOpening = tChange -. t0;
					      rbdts_timeToRunFromOpeningToImpact = t1 -. tChange;
					      rbdts_runSpeedBeforeOpening = speedBefOpening;
					      rbdts_runSpeedFromOpeningToImpact = speedAftOpening;
					      rbdts_footTarget = footPosImp;
					      rbdts_impact = impact3d;
					      rbdts_researchKind = researchKind;
					      rbdts_ballVelAtImpact = iaba.iaba_ballVelWhenItArrives
					    }
					in

					

					let tryNormalSh =

					    let tryStretchSh =

						let footPosImpStretch = footPosAtImpactTime ~deltaXFootRacket:50.0 ~deltaZ:301.0 in
						let runDirStretch = vec2dSub footPosImpStretch playerCurPos in
						let runAngleStretch = 
						    (* NON CAMBIARE! Potresti pensare che se gioca in alto
						       devi usare l'asse Z negato, come fai per
						       l'umano. Invece no!  La cerca male se lo fai. *)
						    smallestAngleWithZAxis runDirStretch in
						let signXStretch = if runDirStretch.x2 > 0.0 then 1.0 else -. 1.0 in
						let deltaXStretch = distance2d footPosImpStretch playerCurPos in
						assert (deltaXStretch != 0.0);
						

						let voteAndIntentStretch = 
						    
						    judgeImpactPointComp ~cp:p ~isVolley ~opponentCurPos ~impact:impact3d
							~footTarget:footPosImpStretch ~mat:surf.s_material
							~isService:s.bsm_lastShotWasAService
							~iWantToAttack
						in
						match voteAndIntentStretch with
						    | IV_Zero -> 
							  None
							      
						    | IV_NonZero  (qualStr, voteStr, volleyOrIntStr) ->

							  let deltaOpening =  computeDeltaOpening 
							      ~researchKind:(RKC_StretchForward volleyOrIntStr) ~isForehand
							  in
							  let tChangeStretchShot = t1 -. deltaOpening in
							  assert (tChangeStretchShot <= t1);
							  let v2StretchShot = 
							      (deltaXStretch  +. speedFreeRun  *.
								   (t0-. tChangeStretchShot) ) /. ( t1 -. tChangeStretchShot) in
							  let canDoStretchShot = 
							      0.0 < impact3d.y3 && impact3d.y3 < p.cp_pc.pc_maxShotHt &&
								  tChangeStretchShot >= 0.0 && 
								  deltaT >= deltaOpening &&  v2StretchShot <= speedNormalResearch in
							  
							  if not canDoStretchShot then
							      None
							  else
							      Some { rj_rbdts = calcResearch
								      ~footPosImp:footPosImpStretch
								      ~researchKind:(RKC_StretchForward volleyOrIntStr)
								      ~v2:v2StretchShot ~tChange:tChangeStretchShot
								      ~signX:signXStretch ~deltaX:deltaXStretch
								      ~runAngle:runAngleStretch
								      ~volleyOrInten:volleyOrIntStr;
								     rj_quality = qualStr;
								     rj_vote = voteStr}

					    in



					    let footPosImpNormal = footPosAtImpactTime ~deltaXFootRacket:110.0 ~deltaZ:80.0 in
					    let runDirNormal = vec2dSub footPosImpNormal playerCurPos in
					    let runAngleNormal = 
						(* NON CAMBIARE! Potresti pensare che se gioca in alto
						   devi usare l'asse Z negato, come fai per
						   l'umano. Invece no!  La cerca male se lo fai. *)
						smallestAngleWithZAxis runDirNormal in
					    let signXNormal = if runDirNormal.x2 > 0.0 then 1.0 else -. 1.0 in
					    let deltaXNormal = distance2d footPosImpNormal playerCurPos in
					    assert (deltaXNormal != 0.0);
					    

					    let voteAndIntentNormal = 
						
						judgeImpactPointComp ~cp:p ~isVolley ~opponentCurPos ~impact:impact3d
						    ~footTarget:footPosImpNormal ~mat:surf.s_material
						    ~isService:s.bsm_lastShotWasAService ~iWantToAttack
					    in
					    match voteAndIntentNormal with
						| IV_Zero -> 
						      tryStretchSh
							  
						| IV_NonZero  (qualNor, voteNor, volleyOrIntNorm) ->

						      let vot = 
							  if isVolley then
							      VOTI_Volee 
							  else
							      
							      match volleyOrIntNorm with 
								  | Volley -> assert false
								  | NotVolley int ->
									let isTopSpin = (* @@ wrong. I should have already decided whether to do backspin or topspin...
											   but it's very expensive. *) true in 
									if isTopSpin then
									    VOTI_NotVolee (Topspin, int)
									else
									    VOTI_NotVolee (Backspin, int)
						      in
						      let deltaOpening =  computeDeltaOpening 
							  ~researchKind:(RKC_Normal vot) ~isForehand
						      in
						      let tChangeNormalShot = t1 -. deltaOpening in
						      assert (tChangeNormalShot <= t1);
						      let v2NormalShot = 
							  (deltaXNormal  +. speedFreeRun  *.
							       (t0-. tChangeNormalShot) ) /. ( t1 -. tChangeNormalShot) in
						      let canDoNormalShot = 
							  0.0 < impact3d.y3 && impact3d.y3 < p.cp_pc.pc_maxShotHt &&
							      tChangeNormalShot >= 0.0 && 
							      deltaT >= deltaOpening &&  v2NormalShot <= speedNormalResearch in
						      
						      if not canDoNormalShot then
							  tryStretchSh
						      else
							  Some { rj_rbdts = calcResearch
								  ~footPosImp:footPosImpNormal
								  ~researchKind:(RKC_Normal vot)
								  ~v2:v2NormalShot ~tChange:tChangeNormalShot
								  ~signX:signXNormal ~deltaX:deltaXNormal
								  ~runAngle:runAngleNormal
								  ~volleyOrInten:volleyOrIntNorm;
								 rj_quality = qualNor;

								 rj_vote = voteNor}

					in

					match voteAndIntentSmash with
					    | IV_Zero -> tryNormalSh

					    | IV_NonZero  (qualSm , voteSm, volleyOrIntSm) ->


						  let deltaOpeningSmash =  computeDeltaOpening 
						      ~researchKind:(RKC_Smash volleyOrIntSm) ~isForehand  in

						  let tChangeSmash = t1 -. deltaOpeningSmash in
						  assert (tChangeSmash <= t1);
						  let v2Smash = (deltaXSmash  +. speedFreeRun  *. (t0 -. tChangeSmash) )
						      /. ( t1 -. tChangeSmash) in

						  
						  let canDoSmash = 
						      p.cp_pc.pc_minSmashHt < impact3d.y3 && impact3d.y3 <
							  p.cp_pc.pc_maxSmashHt && tChangeSmash >= 0.0 && deltaT >=
							  deltaOpeningSmash &&  v2Smash <= speedNormalResearch in

						  if canDoSmash then
						      Some {rj_rbdts = calcResearch ~researchKind:(RKC_Smash volleyOrIntSm)
							      ~v2:v2Smash ~tChange:tChangeSmash  
							      ~footPosImp:footPosImpSmash ~runAngle:runAngleSmash
							      ~signX:signXSmash ~deltaX:deltaXSmash
							      ~volleyOrInten:volleyOrIntSm;
							    rj_quality = qualSm;
							    rj_vote = voteSm}
						  else
						      tryNormalSh


		      in (* end createResearchAndJudgeIt *)

		      let iWantToAttack = shouldAttack ~p in
		      List.map (fun x -> createResearchAndJudgeIt ~p ~impact:x
				    ~playerCurPos ~ballPos ~ballDir ~ball ~s:bsm 
				    ~opponentCurPos ~surf ~iWantToAttack ) impacts

	in (* end judgeAllResearches *)


	judgeAllResearches ~p  ~playerCurPos
	    ~opponentCurPos ~ball ~ballPos ~ballDir ~bsm ~surf  in


    let researches'  = select_some researches in

    let better x1 x2 =
	if x1.rj_quality < x2.rj_quality then
	    x1
	else if x2.rj_quality  < x1.rj_quality then
	    x2
	else
	    if x1.rj_vote > x2.rj_vote then x1 else x2
    in

    let smashVolleys = 
	let isSmashVolley res =
	    match res.rj_rbdts.rbdts_researchKind with
		| RKC_Smash voi -> 
		      (match voi with
			   | Volley -> true
			   | NotVolley _ -> false)
		| RKC_Normal _ | RKC_StretchForward _ -> false
	in

	List.filter isSmashVolley researches' in
    match smashVolleys with
	| [] ->
	      let tryNormalShot = 
		  let normalShots = 
		      let isNormalShot res =
			  match res.rj_rbdts.rbdts_researchKind with
			      | RKC_Normal _ -> true
			      | RKC_StretchForward _ | RKC_Smash _ -> false
		      in
		      List.filter isNormalShot researches' in
		  ( match normalShots with
			| [] ->
			      let smashGrounds = 
				  let isSmashGround res =
				      match res.rj_rbdts.rbdts_researchKind with
					  | RKC_Smash v -> 
						(match v with
						     | Volley -> false
						     | NotVolley _ -> true )
					  | RKC_Normal _ | RKC_StretchForward _ -> false in
				  List.filter isSmashGround researches' in
			      ( match smashGrounds with
				    | [] -> 

					  let stretches = 
					      let isStretch res =
						  match res.rj_rbdts.rbdts_researchKind with
						      | RKC_StretchForward _ -> true
						      | RKC_Normal _ | RKC_Smash _ -> false in
					      List.filter isStretch researches' in
					  ( match stretches with
						| [] -> None
						| l ->Some ((findBestElement l better).rj_rbdts))

				    | l -> Some ((findBestElement l better).rj_rbdts))
			| l ->
			      Some (  (findBestElement l better).rj_rbdts))

	      in
	      (* 	      if not (shouldAttack ~p) &&  *)
	      (* 		  abs_float playerCurPos.z2 > courtHt2 *. 0.7  *)
	      (* 		  (\* @@ is differentiating still needed, given  *)
	      (* 		     judgeImpactPointComp already discriminates? *\) *)
	      (* 	      then *)
	      
	      tryNormalShot
		  (* 	      else *)
		  
	(* 		  let voleesCloseToNet =  *)
	(* 		      let isVoleeCloseToNet res = *)
	(* 			  if abs_float res.rj_rbdts.rbdts_impact.z3 < courtHt4  then *)
	(* 			      match res.rj_rbdts.rbdts_researchKind with *)
	(* 				  | RKC_Smash  NotVolley _  -> false *)
	(* 				  | RKC_Smash  Volley -> false (\* already dealt with *\) *)
	(* 				  | RKC_Normal VOTI_Volee -> true *)
	(* 				  | RKC_Normal VOTI_NotVolee _ -> false *)
	(* 				  | RKC_StretchForward _ -> false (\* later *\) *)
		  
	(* 			  else *)
	(* 			      false *)
	(* 		      in *)

	(* 		      List.filter isVoleeCloseToNet researches' in *)
	(* 		  ( *)
	(* 		   match voleesCloseToNet with *)
	(* 		       | [] -> *)
	(* 			     tryNormalShot *)
	(* 		       | l -> *)
	(* 			     Some (  (findBestElement l better).rj_rbdts)) *)
		  
	| l -> Some ( (findBestElement l better).rj_rbdts)
	      




type circle = { c_center:vec2d; c_radius:float}


let doStepTowardsCenter ~p ~volleyOrInt ~lockInfo ~dt ~opponentZ ~ball ~prefix = 

    let newAttackChoice = 
	match volleyOrInt with
	    | Volley ->
		  Volley
	    | NotVolley _ ->
		  if AI.isAttacking (curPosOfComputerPlayer p) then
		      NotVolley AttackApproach
		  else
		      volleyOrInt

    and speedFreeRunNoFatigue = 
	if AI.isAttacking (curPosOfComputerPlayer p) then
	    p.cp_pc.pc_maxSpeedInFreeRunUnderNet
	else
	    p.cp_pc.pc_maxSpeedInFreeRunStayBack
    in
    let dirsign = if p.cp_playsInTopmostCourtHalf then 1.0 else -. 1.0 in
    let pivot = 
	let aggressive = 
	    if p.cp_playsInTopmostCourtHalf then
		vec2dCreate 0.0 (-. courtHt4 -. 424.0)
	    else
		vec2dCreate 0.0 (courtHt4 +. 424.0)in
	match newAttackChoice with
	    | NotVolley a ->
		  (match a with
		       | StayBack ->
			     if p.cp_playsInTopmostCourtHalf then
				 vec2dCreate 0.0 (-. courtHt4 -. 394.35)
			     else
				 vec2dCreate 0.0 (courtHt4 +. 394.35)
		       | AttackApproach | AttackWithPowerShot -> aggressive )
	    | Volley ->
		  aggressive
    in
    let hotLine = 
	let opponentImpact = 
	    match lockInfo with
		| HasNotLocked  ->
		      ( match ball.b_state with
			    | BS_Moving bsm ->
				  let ballLine = 
				      straightLineFromPointAndDir (projection2d (bsm.bsm_trajectory.impact))
					  (projection2d (bsm.bsm_trajectory.startVel)) 
				  in
				  let oppLine = { sl_a = 0.0; sl_b = 1.0; sl_c = -. opponentZ} in
				  intersectionOfStraightLines oppLine ballLine
			    | BS_Still _ ->
				  assert(false)
		      )
		| HasLocked (pos, _) ->
		      pos
	in

	straightLineBetween pivot opponentImpact 
    in

    let optimalPosition = 
	(* la z e' fissa e dipende solo dall'intenzione di attacco. la
	   x invece dev'essere sulla retta calda. *)
	let optY =
	    match newAttackChoice with
		| NotVolley a ->
		      (match a with
			   | StayBack -> 
				 dirsign *. (-. courtHt2 -. 180.0)
			   | AttackApproach | AttackWithPowerShot ->
				 dirsign *. (-. 100.0) )
		| Volley ->
		      dirsign *. (-. 100.0)

	in
	vec2dCreate
	    (( -. hotLine.sl_b *. optY -. hotLine.sl_c) /. hotLine.sl_a)
	    optY 
    and myPos =  (curPosOfComputerPlayer p) in 

    let youCanStop = 
	let o2 = 
	    let animName = prefix ^ "saltello" in
	    setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false
	in 
	(CPS_GetBackToCenterDuringGame (newAttackChoice, optimalPosition, optimalPosition),
	 { umd_timer = 0.0;
	   umd_startVel = vec2dNull;
	   umd_startPos = myPos},
	 p.cp_fatigueData,
	 o2)
    in
    if distance2d optimalPosition myPos < 3.0 then
	youCanStop
    else
	let getBackSpeed, fatigueData', sprinting =

	    let normalSpeed = speedFreeRunNoFatigue /. p.cp_fatigueData.fatigueDivisor in
	    let trySprint = 
		let speedWithSprint = 
		    let speedFreeRun = 
			if AI.isAttacking myPos then
			    p.cp_pc.pc_maxSpeedInFreeRunUnderNet
			else
			    p.cp_pc.pc_maxSpeedInFreeRunStayBack
		    
		    and beta = 
			let velDir = vec2dSub optimalPosition myPos in
			if p.cp_playsInTopmostCourtHalf then
			    smallestAngleWithZAxis velDir
			else
			    smallestAngleWithNegativeZAxis velDir in
		    (* 1.63 (*  found empirically *) *. speedFreeRunNoFatigue in *)
		    sprintSpeed ~beta ~speedFreeRun
		in
		let mayb =
		    let canSprint cm fatigueData =
			if cm < fatigueData.fatigueAvailableSprintDistance then
			    Some {fatigueData with 
				      fatigueAvailableSprintDistance = 
				    fatigueData.fatigueAvailableSprintDistance-. cm }
			else
			    None in
		    let deltaSWithSprint = 
			speedWithSprint *. dt in
		    canSprint deltaSWithSprint p.cp_fatigueData in
		let finalSp, finalFat = 
		    match mayb with
			| None ->
			      (  normalSpeed, p.cp_fatigueData)
			| Some fatig' ->
			      ( speedWithSprint , fatig')
		in
		( finalSp, finalFat, (match mayb with None -> false | _ -> true)) 
	    in
	    match newAttackChoice with
		| NotVolley intent ->
		      (match intent with
			   | StayBack ->
				 if abs_float myPos.x2 > courtWt2 *. 0.6 && 
				      not (shouldAttack ~p) (* spare the sprint for attacking later *)
				 then
				     trySprint
				 else
				     ( normalSpeed, p.cp_fatigueData, false)
			   | AttackWithPowerShot | AttackApproach -> 
				 trySprint
		      )
		| Volley ->
		      if abs_float myPos.x2 > courtWt2 *. 0.6 then
			  trySprint
		      else
			  (normalSpeed, p.cp_fatigueData, false)

	in
	let getBackPosition =
	    match lockInfo with
		| HasNotLocked  ->
		      optimalPosition
		| HasLocked (pos, time) ->
		      

    		      let maxDistanceICanSpan = getBackSpeed *. time in
    		      let distanceToArriveAtOptimal = distance2d myPos
    			  optimalPosition in
    		      if distanceToArriveAtOptimal <= maxDistanceICanSpan then
    			  optimalPosition
    		      else
    			  let circ = { c_center =  myPos;
    				       c_radius =  maxDistanceICanSpan} in
    			  let intersectCircle ci line =
    			      let q = -. line.sl_b in
    			      let a = line.sl_a in
    			      assert(a != 0.0);
    			      let b = line.sl_b in
    			      let c = line.sl_c in
    			      let xc = ci.c_center.x2 in
    			      let zc = ci.c_center.z2 in
    			      let r = ci.c_radius in
    			      let a_eq  = q *. q +. a *. a in
    			      let b_eq = -. 2.0 *. c *. q -. 2.0 *. a *. xc *. q -. 2.0 *. zc *. a *. a in
    			      let c_eq = c*.c +. xc *. xc *. a*.a +. 2.0 *. a*.xc *.c +. zc *. zc *. a *. a
    				  -. r *. r*. a*. a in
			      let may =  solve2ndDegreeEquat a_eq b_eq c_eq in
			      match may with
				  | None -> None
				  | Some ( z1, z2) ->
    					let x1 = (-. c -. b *. z1 ) /. a in
    					let x2 = (-. c -. b *. z2 ) /. a in
    					Some (vec2dCreate x1 z1, vec2dCreate x2 z2)
    			  in
			  let may = intersectCircle circ hotLine in
			  match may with
			      | Some (p1, p2) ->
    				    let closestToOptimum ~p1 ~p2 ~optimalPosition =
    					if distance2d p1 optimalPosition < distance2d p2 optimalPosition then
    					    p1
    					else
    					    p2
    				    in
    				    let closestToNet ~p1 ~p2 =
    					if abs_float p1.z2 < abs_float p2.z2 then p1 else p2 in
				    let farthestFromNet p1 p2 =
					if abs_float p1.z2 > abs_float p2.z2 then p1 else p2 in
    				    if p.cp_playsInTopmostCourtHalf then
    					(* se sono entrambi nella mia meta' campo... *)
					

    					if p1.z2 < -.200.0 && p2.z2 < -.200.0 then
    					    match newAttackChoice with
    						| NotVolley _ ->
    						      closestToOptimum ~p1 ~p2 ~optimalPosition
    						| Volley ->
    						      closestToNet ~p1 ~p2
    					else
    					    (* scelgo quella nella mia meta' campo *)
    					    if p1.z2 < -. 200.0 then
    						p1
    					    else if p2.z2 < -. 200.0 then
    						p2
    					    else
    						farthestFromNet p1 p2
    				    else
    					if p1.z2 > 200.0 && p2.z2 > 200.0 then
    					    match newAttackChoice with
    						| NotVolley _ ->
    						      closestToOptimum ~p1 ~p2 ~optimalPosition
    						| Volley ->
    						      closestToNet ~p1 ~p2
    					else
    					    if p1.z2 > 200.0 then
    						p1
    					    else if p2.z2 > 200.0 then
    						p2
    					    else
    						farthestFromNet p1 p2
			      | None ->

    				    (* se la retta e la circonferenza non si
    				       intersecano, non arriveremo mai alla
    				       retta calda, indipendentemente da in
    				       che direzione corriamo.  In tal caso
    				       il punto di rientro e' il piu' vicino
    				       alla retta calda (in direzione
    				       ortogonale).  vedi quadernino *)
    				    let xc = pos.x2 in
    				    let zc = pos.z2 in
    				    let xh = pivot.x2 in
    				    let zh = pivot.z2 in
    				    let xp = myPos.x2 in
    				    let zp = myPos.z2 in
    				    let a = hotLine.sl_a in
    				    assert ( a != 0.0);
    				    let b = hotLine.sl_b in
    				    let c = hotLine.sl_c in
    				    let d = xc -. xh in
    				    let e = zc -. zh in
    				    let zq = ( d *. c  +. d *. a *. xp +. e *. a *. zp) /. (e *. a -. d *. b) in
    				    let xq = ( -. c -. b *. zq ) /. a in
    				    vec2dCreate xq zq
	in 
	let o2 = 
	    if sprinting then

		let walkAni = 
		    computeWalkAnim ~footTarget:getBackPosition 
			~curPos:myPos ~dirsign:(-. dirsign)  in
		
		let animName = prefix ^ walkAni in
		setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false

	    else

		let animName = prefix ^ "attesa" in
		setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false
	and getBackDir = vec2dSub getBackPosition   myPos in
	let ang = smallestAngleWithZAxis getBackDir in
	let signX = if getBackDir.x2 > 0.0 then 1.0 else -. 1.0 in


	let velx = signX *. getBackSpeed *. sin ang in
	let velz =  getBackSpeed *. cos ang in
	let st = 
	    let deltaX = distance2d getBackPosition myPos in
	    CPS_GetBackToCenterDuringGame ( newAttackChoice, getBackPosition, optimalPosition) in
	let umd = {umd_timer = 0.0;
		   umd_startVel= vec2dCreate velx velz;
		   umd_startPos = myPos} in

	( st, umd, fatigueData', o2)







let updateComputerPlayer  ~p ~dt ~b ~opponentCurPos ~surf ~opponentLock ~mouse ~sounds ~opt ~aidebug 
	~nextServiceIsFirst = 

    let prefix =
	if p.cp_playsInTopmostCourtHalf then gfxDir ^ "/A"else gfxDir ^ "/B"in

    let dirsign = if p.cp_playsInTopmostCourtHalf then -. 1.0 else 1.0 in

    let p = 
	let newObj = updateAnim p.cp_obj dt in
	{p with
	     cp_obj = newObj;
	     cp_umd = {p.cp_umd with umd_timer = p.cp_umd.umd_timer +. dt}
	} in

    let startWalkingTowards ~getBackPoint ~vel = 
	let my2dpos = curPosOfComputerPlayer p in
	let getBackDir = vec2dSub getBackPoint my2dpos in
	if getBackDir.x2 = 0.0 && getBackDir.z2 = 0.0 then
	    {umd_timer = 0.0;
	     umd_startVel = vec2dCreate 0.0 0.0;
	     umd_startPos = my2dpos}
	else
	    let ang = smallestAngleWithZAxis getBackDir in
	    let signX = if getBackDir.x2 > 0.0 then 1.0 else -. 1.0 in
	    let velx = signX *. vel *. (sin ang) in
	    let velz = vel *. (cos ang) in
	    { umd_timer = 0.0;
	      umd_startPos = my2dpos;
	      umd_startVel = vec2dCreate velx velz}

    in (* end startWalkingTowards *)


    let newState, newUmd, newObj, newBall, newFatigueData, pwa, pws = 



	match p.cp_state with
	    | CPS_ServingBeforeLaunch right ->

		  let st2 =  
		      if p.cp_obj.o3d_animState = PausedDuringService then
			  CPS_ServingAfterLaunchAndBeforeStartingGesture (right ,0.0)
		      else
			  CPS_ServingBeforeLaunch right in
		  let b2 = 
		      if p.cp_obj.o3d_animState = PausedDuringService then
			  let traj =
			      { impact = vec3dCreate (curPosOfComputerPlayer p).x2 171.0 (dirsign*.(courtHt2 +. 20.0));
				startVel = vec3dCreate 0.0 750.0 0.0;
				spin = vec3dNull;
				targetRect = None}in
			  (print_endline "ball launched by computer";
			   createRunningBall ~traj
			       ~scoreIndexOfLastPlayerWhoHit:b.b_siolpwhtb
			       ~polyBall:b.b_polygon ~service:true
			       ~polyRedCross:b.b_redCrossPolygon ~polyShadow:b.b_shadowPolygon)
		      else
			  b in
		  (st2, p.cp_umd, p.cp_obj, b2, p.cp_fatigueData, 
		   p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)

	    | CPS_ServingAfterLaunchAndBeforeStartingGesture ( right , timer )->
		  

		  let st2, o2 = 
		      let timer2 = timer +. dt in
		      let delta = 
			  (* @@ ugly hack, this should be calculated dynamically given the anim *)
			  if p.cp_playsInTopmostCourtHalf then 0.2 else 0.87 in
		      if timer2 > delta then
			  ((CPS_ServingAfterStartingGesture (right, 0.0) ), 
			   {p.cp_obj with o3d_animState = Animated 0.0;
				o3d_curFrameIdx = p.cp_obj.o3d_curFrameIdx +1})
		      else
			  CPS_ServingAfterLaunchAndBeforeStartingGesture (right, timer2), p.cp_obj in
		  (st2, p.cp_umd, o2, b, p.cp_fatigueData,  p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)


	    | CPS_ServingAfterStartingGesture (right, timer) ->

		  let timer' = timer +. dt in
		  let st2 = 
		      if timer' >= durationOfCurAnimUpToImpactFrame ~o:p.cp_obj then
			  CPS_ServingAfterHittingBall 
		      else
			  CPS_ServingAfterStartingGesture (right, timer')
		  in
		  let diag = Random.int 2 = 0 in
		  let b2 = 
		      if timer' >= durationOfCurAnimUpToImpactFrame ~o:p.cp_obj then
			  ( let traj =


				let ang = 
				    let offsX = if diag then courtWt2 +. 35.0 else 16.0 in
				    if right then
					let dir = 
					    let src = curPosOfComputerPlayer p in
					    let dest = 
						vec2dCreate (-. dirsign *. offsX) 
						    (-. dirsign *. (courtHt4 +. 50.0)) in
					    vec2dSub dest  src in
					if p.cp_playsInTopmostCourtHalf then
					    -. (smallestAngleWithZAxis dir)
					else
					    -. (smallestAngleWithNegativeZAxis dir)
				    else
					let dir = 
					    let src = curPosOfComputerPlayer p in
					    let dest = 
						vec2dCreate ( dirsign *. offsX) 
						    (-. dirsign *. (courtHt4 +. 50.0)) in
					    vec2dSub dest  src in
					if p.cp_playsInTopmostCourtHalf then
					    smallestAngleWithZAxis dir
					else
					    smallestAngleWithNegativeZAxis dir
				in



				let angX = 
				    degToRad p.cp_pc.pc_firstServiceXAngleInDeg
				in
				let velservizio = 
				    cmPerSecondOfKmh p.cp_pc.pc_firstServiceSpeedKmh
				in
				let velxz = velservizio *. (cos angX) in
				let vely = velservizio *. (sin angX) in

				let velx = velxz *. (sin ang) *. dirsign in
				let velz = -. velxz *. (cos ang) *. dirsign in
				let firstService = true in
				let spi = 
				    if firstService then
					p.cp_pc.pc_firstServiceSpin
				    else
					p.cp_pc.pc_secondServiceSpin in
				let rett =
				    if not p.cp_playsInTopmostCourtHalf then
					if right then
					    servizioInAltoSulPari
					else
					    servizioInAltoSulDispari
				    else
					if right then
					    servizioInBassoSulPari
					else
					    servizioInBassoSulDispari in

				let heights = List.map (fun x -> float_of_int x) (listFromTo 270 310) in
				let goodHeights = 
				    let shotIsGood h = 
					let tr =
					    { impact = { (curBallPos b) with y3 = h };
					      startVel = vec3dCreate velx (-. vely) velz;
					      spin = vec3dCreate
						    ( spi *. dirsign *. (sin ang)) spi (-. spi *. (cos ang) *. dirsign);
					      targetRect = Some rett } in
					let notHitNet = 
					    let may = whenWillTheTrajectoryHitTheNet tr in
					    match may with None -> true | _ -> false in
					theTrajectoryFallsInTheTargetRect tr && notHitNet in
				    Array.of_list (List.filter (fun h -> shotIsGood h ) heights) in
				
				{ impact = { (curBallPos b) with y3 = goodHeights.( (Array.length goodHeights )/2) };
				  startVel = vec3dCreate velx (-. vely) velz;
				  spin = vec3dCreate
					( spi *. dirsign *. (sin ang)) 
					spi
					(-. spi *. (cos ang) *. dirsign);
				  targetRect = Some rett
				} 
			    in



			    playSoundId ~sounds ~id:SoundNormalShot;
			    print_endline ("Service impact height: " ^ string_of_float traj.impact.y3 );
			    createRunningBall ~traj
				~scoreIndexOfLastPlayerWhoHit:p.cp_scoreIndex
				~polyBall:b.b_polygon
				~polyRedCross:b.b_redCrossPolygon
				~polyShadow:b.b_shadowPolygon ~service:true
			  )
		      else
			  b
		  in
		  
		  (st2, p.cp_umd, p.cp_obj, b2, p.cp_fatigueData,  
		   p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)


	    | CPS_ServingAfterHittingBall ->

		  let st2, umd2, fat2, o2 = 
		      
		      if p.cp_obj.o3d_animState = NotAnimated then
			  let voint = 
			      
			      if shouldAttack ~p then
				  NotVolley AttackWithPowerShot
			      else
				  NotVolley StayBack
			  in
			  
			  let p =
			      let uu = 
				  let newPos = 
				      let po = curPosOfComputerPlayer p in
				      let x = po.x2 
				      and y = po.z2 (* @@ if p.cp_playsInTopmostCourtHalf then po.z2 +. 150.0 else po.z2 -. 150.0 in *) in
				      vec2dCreate x y in
				  { umd_startPos = newPos;
				    umd_startVel = vec2dCreate 0.0 0.0;
				    umd_timer = 0.0 } in
			      { p with cp_umd = uu } in
			  doStepTowardsCenter ~p ~volleyOrInt:voint
			      ~lockInfo:opponentLock ~prefix
			      ~dt ~opponentZ:opponentCurPos.z2 ~ball:b
		      else
			  CPS_ServingAfterHittingBall, p.cp_umd, p.cp_fatigueData, p.cp_obj  in 

		  (st2, umd2, o2, b, fat2,  p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)

	    | CPS_WaitingForBallToComeTowardsMe ->

		  let st2, umd2, o2 = 
		      let getBackPoint = 
			  if p.cp_playsInTopmostCourtHalf then
			      vec2dCreate 0.0 (-. courtHt2 -. 200.0)
			  else
			      vec2dCreate 0.0 (courtHt2 +. 200.0)
		      in

		      match b.b_state with 
			  | BS_Still _ ->
				CPS_WaitingForBallToComeTowardsMe, p.cp_umd, p.cp_obj
			  | BS_Moving bsm ->
				if bsm.bsm_bouncesSoFar > 1 then
				    let speedFreeRunNoFatigue = 
					if AI.isAttacking (curPosOfComputerPlayer p) then
					    p.cp_pc.pc_maxSpeedInFreeRunUnderNet
					else
					    p.cp_pc.pc_maxSpeedInFreeRunStayBack
				    in
				    let back = 
					let stopTime = 

					    let deltaX = 

						let my2dpos = curPosOfComputerPlayer p in
						distance2d getBackPoint my2dpos 
					    in
					    deltaX /. speedFreeRunNoFatigue in
					CPS_GetBackToCenterAtPointFinished stopTime
				    and o2 = 
					let animName = prefix ^ "attesa" in
					setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false 
				    and u = 
					startWalkingTowards ~getBackPoint ~vel:speedFreeRunNoFatigue in

				    back, u, o2
				else
				    let ballIsComingTowardsMe  = 
					if p.cp_playsInTopmostCourtHalf then
					    bsm.bsm_trajectory.startVel.z3 < 0.0
					else
					    bsm.bsm_trajectory.startVel.z3 > 0.0 in
				    
				    if ballIsComingTowardsMe then
					let o2 = 
					    let animName = prefix ^ "saltello" in
					    setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false 
					in
					CPS_RealizingWhereTheBallIs, p.cp_umd, o2
				    else
					p.cp_state, p.cp_umd, p.cp_obj
		  in
		  (st2, umd2, o2, b, p.cp_fatigueData,  
		   p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)

	    | CPS_RealizingWhereTheBallIs ->

		  if p.cp_umd.umd_timer <= reflexDeltaT  then
		      (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData, 
		       p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
		  else
		      let st2, u2, o2, newPWA, newPWSB = 
			  let ballGoesOutOrOntoNet bsm = 
			      let ballIsAboutToHitNet = 
				  let maybeT = whenWillTheTrajectoryHitTheNet bsm.bsm_trajectory in
				  match maybeT with
				      | None -> false
				      | Some _ -> (bsm.bsm_bouncesSoFar = 0) 
			      and ballWillGoOut = 
				  bsm.bsm_bouncesSoFar = 0 &&
				  (match bsm.bsm_trajectory.targetRect with None -> false | _ -> true) &&
				  not (theTrajectoryFallsInTheTargetRect bsm.bsm_trajectory) in

			      (not bsm.bsm_isItGoodSoFar) ||  ballIsAboutToHitNet || ballWillGoOut
				  
			  and my2dpos = curPosOfComputerPlayer p in
			  
			  let backAtPointFinished ~pwa ~pws ~b ~bsm = 
			      let stopTime, vx, vz, o2 = 
				  if bsm.bsm_bouncesSoFar < 2  && bsm.bsm_isItGoodSoFar then
				      let footTarget = 
					  let p1 =
					      let horizLine = 
						  let pz = my2dpos.z2 in
						  {sl_a = 0.0 ; sl_b = 1.0; sl_c = -. pz} in
					      let ballDir = 
						  straightLineFromPointAndDir (projection2d (curBallPos b))
						      (projection2d (curBallVel bsm)) in
					      
					      intersectionOfStraightLines horizLine ballDir
					  and p2 = (* @@ crashes if already done second bounce! *)
					      whereWillTheBallMakeSecondBounce ~b ~bsm ~surf in
					  if abs_float p2.z2 < abs_float p1.z2 then p2 else p1
				      in

				      let dir = vec2dSub footTarget my2dpos in
				      let distanceToRun = distance2d footTarget my2dpos -. 170.0 in
				      if distanceToRun > 0.0 then
					  if isNull2d dir then
					      (0.0, 0.0, 0.0, p.cp_obj)
					  else
					      let speedNormalResearch = 
						  (if AI.isAttacking my2dpos then
						       p.cp_pc.pc_maxSpeedInNormalResearchUnderNet
						   else
						       p.cp_pc.pc_maxSpeedInNormalResearchStayBack)
						  /. p.cp_fatigueData.fatigueDivisor
					      in
					      let alpha, beta = 
						  ( smallestAngleWithZAxis dir,
						    smallestAngleBetween (vec2dCreate 1.0 0.0) dir)
						      
					      and o2 =
						  let animName = 
						      let walkAni = computeWalkAnim ~footTarget ~curPos:my2dpos ~dirsign  in
					      	      prefix ^ walkAni in
						  setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false  in
					      ( 
						  distanceToRun  /. speedNormalResearch , 
						  speedNormalResearch *. cos beta,
						  speedNormalResearch *. cos alpha,
						  o2) 
				      else
					  0.0, 0.0, 0.0, p.cp_obj
				  else
				      
				      0.0 , 0.0 ,0.0, p.cp_obj
			      in
			      let  umd = 
				  { umd_timer = 0.0;
				    umd_startVel = vec2dCreate vx vz;
				    umd_startPos = my2dpos } in
			      ( CPS_GetBackToCenterAtPointFinished stopTime, umd, o2, pwa, pws)
			  in
			  match b.b_state with
			      | BS_Still _ -> 
				    assert false
					(* 				    backAtPointFinished ~pwa:p.cp_pointsWonAttacking  *)
					(* 				    ~pws:p.cp_pointsWonStayingBack ~b *)
			      | BS_Moving bsm ->
				    let ballGoesOutOrOntoNet bsm = 
					let ballIsAboutToHitNet = 
					    let maybeT = whenWillTheTrajectoryHitTheNet bsm.bsm_trajectory in
					    match maybeT with
						| None -> false
						| Some _ -> (bsm.bsm_bouncesSoFar = 0) in
					let ballWillGoOut = 
					    bsm.bsm_bouncesSoFar = 0 &&
					    (match bsm.bsm_trajectory.targetRect with None -> false | _ -> true) &&
					    not (theTrajectoryFallsInTheTargetRect bsm.bsm_trajectory) in

					(not bsm.bsm_isItGoodSoFar) ||  ballIsAboutToHitNet || ballWillGoOut
				    in
				    if ballGoesOutOrOntoNet bsm then
					let pwa, pws = 
					    if bsm.bsm_lastShotWasAService || not bsm.bsm_isItGoodSoFar then
						(p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
					    else
						(
(* 						    print_endline ("ballGoesOutOrOntoNet bsm. computer adds a point"); *)
						    updateMemoryOfPointsWonAndLost ~p ~won:true ~opponentCurPos)
						in
					backAtPointFinished ~pwa ~pws ~b ~bsm
				    else
					let res = chooseBestResearch ~playerCurPos:my2dpos ~opponentCurPos
					    ~ballPos:(projection2d (curBallPos b) ) 
					    ~ballDir:(projection2d (curBallVel bsm) ) 
					    ~ball:b ~surf ~bsm ~p in
					match res with
					    | None ->
						  let pwa, pws = updateMemoryOfPointsWonAndLost ~p
						      ~won:false ~opponentCurPos in
						  backAtPointFinished ~pwa ~pws ~b ~bsm
						      
					    | Some s ->
						  let walkAni = 
						      computeWalkAnim ~footTarget:s.rbdts_footTarget
							  ~curPos:my2dpos ~dirsign  in
						  let o2 = 
						      let animName = prefix ^ walkAni in
						      setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false in

						  ( CPS_ResearchBeforeDecidingTheShot s,
						    { umd_timer = 0.0;
						      umd_startVel = s.rbdts_runSpeedBeforeOpening;
						      umd_startPos = my2dpos},
						    o2,
						    p.cp_pointsWonAttacking,
						    p.cp_pointsWonStayingBack
						  )

		      in
		      (st2, u2, o2, b, p.cp_fatigueData, newPWA, newPWSB)
			  
	    | CPS_ResearchBeforeDecidingTheShot rbdts ->
		  
		  if p.cp_umd.umd_timer <= rbdts.rbdts_timeToRunBeforeOpening then
		      (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData,
		       p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
		  else
		      let destRect = if p.cp_playsInTopmostCourtHalf then
			  lowerHalfOfCourt else upperHalfOfCourt in
		      let st2 = 
			  let tr = 
			      let targetRect = (Some (if p.cp_playsInTopmostCourtHalf then
							  lowerHalfOfCourt else upperHalfOfCourt)) in

			      chooseTrajectory ~p 
				  ~impact:rbdts.rbdts_impact ~opponentPos:opponentCurPos ~aidebug
				  ~ballVelAtImpact:rbdts.rbdts_ballVelAtImpact
				  ~canIDoTopSpin:true ~canIDoBackSpin:true ~myPosAtImpact:rbdts.rbdts_footTarget
				  ~surf:surf.s_material ~targetRect ~iAmOpening:true
				  ~researchKind:rbdts.rbdts_researchKind in

			  match tr with
			      | None -> 
				    (* @@ happened on computer pete smashing close to net ( z = -. 60.0 )*)
				    ( print_endline ("failure finding a traj. impact = " ^ 
							 string_of_float rbdts.rbdts_impact.x3 ^ ", " ^ 
							 string_of_float rbdts.rbdts_impact.y3 ^ ", " ^ 
							 string_of_float rbdts.rbdts_impact.z3);
				      assert(false) )
			      | Some (descr, tt) ->
				    print_endline ("1: " ^descr);
				    let researchWithUpdatedSpin = 
					match rbdts.rbdts_researchKind with
					    | RKC_StretchForward v -> RKC_StretchForward v
					    | RKC_Smash v -> RKC_Smash v
					    | RKC_Normal VOTI_Volee -> RKC_Normal VOTI_Volee
					    | RKC_Normal VOTI_NotVolee (_, at) ->
						  let sp =
						      if tt.spin.y3 >= 0.0 then
							  Topspin
						      else
							  Backspin in
						  RKC_Normal (VOTI_NotVolee (sp, at))
						      
				    in{ radts_Trajectory = tt;
					radts_Forehand = rbdts.rbdts_forehand;
					radts_TimeToRunFromOpeningToImpact = rbdts.rbdts_timeToRunFromOpeningToImpact;
					radts_RunSpeedFromOpeningToImpact = rbdts.rbdts_runSpeedFromOpeningToImpact;
					radts_FootTarget= rbdts.rbdts_footTarget;
					radts_Impact = rbdts.rbdts_impact;
					radts_researchKind = researchWithUpdatedSpin;
					radts_BallVelAtImpact = rbdts.rbdts_ballVelAtImpact}
		      in
		      let my2dpos = curPosOfComputerPlayer p in
		      let u2 = {umd_timer = 0.0;
				umd_startVel = rbdts.rbdts_runSpeedFromOpeningToImpact;
				umd_startPos = my2dpos}
		      in

		      let o2 = 

			  let animName = 

			      match  st2.radts_researchKind with
				  | RKC_Normal vt  ->
					let core = 
					    if st2.radts_Forehand then 
						match vt with
						    | VOTI_Volee -> "drittov"
						    | VOTI_NotVolee (sp, _) ->
							  match sp with
							      | Topspin -> "dritto"
							      | Backspin -> "drittoback"
					    else
						match vt with
						    | VOTI_Volee -> "rovesciov"
						    | VOTI_NotVolee (sp, _) ->
							  match sp with
							      | Topspin -> "rovescio"
							      | Backspin -> "rovescioback"
					in
					prefix ^ core
				  | RKC_Smash _ ->
					prefix ^ "smash"

				  | RKC_StretchForward _ ->
					prefix ^ if rbdts.rbdts_forehand then "drittoforwardstretch" else "rovescioforwardstretch"

					    
			  in


			  
			  setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:true in
		      
		      ( CPS_ResearchAfterDecidingTheShot st2, u2, o2, b, p.cp_fatigueData,
			p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)

	    | CPS_ResearchAfterDecidingTheShot r ->
		  
		  if p.cp_umd.umd_timer <= r.radts_TimeToRunFromOpeningToImpact then
		      (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData,
		       p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
		  else
		      let destRect = if p.cp_playsInTopmostCourtHalf then
			  lowerHalfOfCourt else upperHalfOfCourt in
		      let st2 =
			  let volleyOrIntention = 
			      match r.radts_researchKind with
				  | RKC_Smash Volley -> Volley
				  | RKC_Smash NotVolley ai -> NotVolley ai
				  | RKC_StretchForward Volley -> Volley
				  | RKC_StretchForward NotVolley ai -> NotVolley ai
				  | RKC_Normal VOTI_Volee -> Volley
				  | RKC_Normal VOTI_NotVolee (_, int) -> NotVolley int
			  in

			  CPS_TheAnimationIsTerminating volleyOrIntention in
		      let my2dpos = curPosOfComputerPlayer p in
		      let u2 = {umd_timer = 0.0;
				umd_startVel = r.radts_RunSpeedFromOpeningToImpact; (* the same as now*)
				umd_startPos = my2dpos}
		      in

		      let b2 =
			  let mayb = 
			      let canIDoTopSpin = r.radts_Trajectory.spin.y3 >= 0.0 in
			      chooseTrajectory ~p 
				  ~impact:r.radts_Impact ~opponentPos:opponentCurPos ~myPosAtImpact:r.radts_FootTarget
				  ~ballVelAtImpact:r.radts_BallVelAtImpact ~iAmOpening:false
				  ~canIDoTopSpin ~canIDoBackSpin:(not canIDoTopSpin) ~aidebug
				  ~surf:surf.s_material ~targetRect:r.radts_Trajectory.targetRect
				  ~researchKind:r.radts_researchKind in
			  match mayb with
			      | None -> assert(false)
			      | Some (descr, newTraj) ->
				    (print_endline ("2: " ^ descr);
				     createRunningBall ~traj:newTraj ~service:false
					 ~scoreIndexOfLastPlayerWhoHit:p.cp_scoreIndex ~polyBall:b.b_polygon
					 ~polyRedCross:b.b_redCrossPolygon ~polyShadow:b.b_shadowPolygon)
		      in

		      playSoundId ~sounds ~id:SoundNormalShot;

		      (st2, u2, p.cp_obj, b2, p.cp_fatigueData,  
		       p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)

	    | CPS_TheAnimationIsTerminating voll ->
		  
		  if p.cp_obj.o3d_animState = NotAnimated  then
		      let st, umd, fat2, o2 =
			  doStepTowardsCenter ~p ~volleyOrInt:voll ~lockInfo:opponentLock
			      ~dt ~opponentZ:opponentCurPos.z2 ~ball:b ~prefix in
		      (st, umd, o2, b, fat2,
		       p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
		  else
		      (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData,
		       p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
			  




			  
	    | CPS_GetBackToCenterDuringGame ( voll, _, _) ->
		  let st, umd, fat2, o2 =
		      let stopUmd =     
			  let my2dpos = curPosOfComputerPlayer p in
			  {umd_timer = 0.0;
			   umd_startPos = my2dpos; 
			   umd_startVel = vec2dCreate 0.0 0.0} in
 		      let ballIsComingTowardsMe  = 
			  match b.b_state with
			      | BS_Still _ -> false
			      | BS_Moving bsm ->
				    if p.cp_playsInTopmostCourtHalf then
					bsm.bsm_trajectory.startVel.z3 < 0.0
				    else
					bsm.bsm_trajectory.startVel.z3 > 0.0 in
		      

		      if ballIsComingTowardsMe then
			  ( CPS_RealizingWhereTheBallIs, stopUmd, p.cp_fatigueData, p.cp_obj)
		      else
			  doStepTowardsCenter ~p ~volleyOrInt:voll ~lockInfo:opponentLock
			      ~dt ~opponentZ:opponentCurPos.z2 ~ball:b ~prefix in
		  (st, umd, o2, b, fat2,  p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
		      
	    | CPS_GetBackToCenterAtPointFinished t -> 

		  let my2dpos = curPosOfComputerPlayer p in
		  let st, umd = 
		      let stopUmd =     
			  {umd_timer = 0.0;
			   umd_startPos = my2dpos; 
			   umd_startVel = vec2dCreate 0.0 0.0} in
		      match b.b_state with
			  | BS_Still _ -> CPS_WaitingForBallToComeTowardsMe, stopUmd
			  | BS_Moving bsm ->
				if p.cp_umd.umd_timer > t || 
				    not bsm.bsm_isItGoodSoFar || 
				    abs_float (curBallPos b ).z3 > abs_float my2dpos.z2
				then
				    CPS_WaitingForANewPointToBegin , stopUmd (* reset fatigue *)
				else
				    (p.cp_state, p.cp_umd)
			      
		  and o2 = 
		      let stop = 
			  setAnim  ~o:p.cp_obj ~animName:(prefix ^ "saltello")
			      ~restartIfSameAnimation:false
		      in
		      match b.b_state with
			  | BS_Still _ -> stop
			  | BS_Moving bsm ->
				if p.cp_umd.umd_timer > t || 
				    not bsm.bsm_isItGoodSoFar ||
				    abs_float ( curBallPos b).z3 > abs_float my2dpos.z2
				then
				    stop
				else
				    p.cp_obj
		  in
		  (st, umd, o2, b, p.cp_fatigueData,  p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
	    | CPS_WaitingForANewPointToBegin -> 

 		  (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData,  p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
		      

    in

    let newPlayer = {p with cp_state = newState;
			 cp_umd = newUmd;
			 cp_obj = newObj;
			 cp_fatigueData = newFatigueData;
			 cp_pointsWonAttacking = pwa;
			 cp_pointsWonStayingBack = pws
		    }in
    let newPlayer =
	{ newPlayer with
	      cp_fatigueData = (updateFatigue newPlayer.cp_fatigueData (curPosOfComputerPlayer newPlayer))
	} in

    ( newBall, CP newPlayer, mouse, [] )


open Unix

(** Main starts here **)
let _ = 
    (*Caution: server, client and port are passed by ref*)
  	let parsedOptions, server, port, client = parseOptions
    in
    match parsedOptions with
	| ArgumentError e -> 
	      print_endline ( e)
	| ArgumentsOk opt ->
	      let serverData = Network.setupNetwork server client port
        in
	      (match serverData with
		   | Server (_, _, out) ->
			 (
			     Marshal.to_channel out opt.opt_surface [];
			     Marshal.to_channel out opt.opt_p0 [];
			     Marshal.to_channel out opt.opt_p1 [];
			     flush out)
		   | NeitherServerNorClient -> ()
		   | Client _ -> ());

	      let opt = 
		  match serverData with
		      | Server _ -> opt
		      | NeitherServerNorClient -> opt
		      | Client (_, inc, _) ->
			    let su = (Marshal.from_channel inc : material) in
			    let p0 = (Marshal.from_channel inc : playerName) in
			    let p1 = (Marshal.from_channel inc : playerName) in
			    { opt with
				  opt_p0 = p0;
				  opt_p1 = p1;
				  opt_surface = su}
	      in

	      let windowWt =  opt.opt_resX in
	      let windowHt =  opt.opt_resY in
	      let mouseSensitivity = 120.0  in
	      let xCamBehav = PushScroll in
	      let surface =
		  let surfOfMaterial m =
		      { s_material = m;
			s_spinAttenuationFactor = (match m with
						       | Clay -> 2.63
						       | Grass -> 3.5
						       | Cement ->2.36);
			s_velXZAttenuationFactor = (match m with
							| Clay -> 2.8
							| Grass -> 1.55 (* 1.75 troppo lento, si fanno molti scambi, ema *)
							| Cement -> 2.0 (*2.38 *));
			s_velYAttenuationFactor = (match m with
						       | Clay -> 1.62
						       | Grass -> 1.6 (* 1.62: passing shot is too difficult, ball too low *)
						       | Cement -> 1.5 (*1.6 *))} in 
		  surfOfMaterial opt.opt_surface in
	      Sdl.init [`EVERYTHING]; 
	      
	    let sounds = 
		    if opt.opt_noSound then
		      None
		    else
          Sound.loadSounds
	      in

	      Sdlgl.set_attr [Sdlgl.DOUBLEBUFFER true; Sdlgl.DEPTH_SIZE 16]  ;

	      let screen =
		  let listOfFlags =
		      let li = [`OPENGL;  (* `DOUBLEBUF is useless for opengl, see docs *) `RESIZABLE ] in
		      li
		  in
		  Sdlvideo.set_video_mode ~w:windowWt ~h:windowHt ~bpp:0 listOfFlags in
	      Sdlwm.set_caption ~title:loadingString ~icon:freeTennisString;

	      Sdlwm.set_icon (Sdlvideo.load_BMP ( gfxDir ^ "/ball-caption.bmp"));



	      
	      

	      GlDraw.shade_model `smooth;
	      let co = 
		  match surface.s_material with
		      | Clay -> (71.0 /. 255.0 , 141.0 /. 255.0, 67.0 /. 255.0)
		      | Cement -> (56.0 /. 255.0 , 90.0 /. 255.0, 70.0 /. 255.0)
		      | Grass -> (44.0 /. 255.0 , 84.0 /. 255.0, 47.0 /. 255.0)
	      in
	      GlClear.color co ~alpha:1.0;
	      GlClear.depth 1.0;
	      Gl.enable `depth_test;
	      GlFunc.depth_func `lequal;
	      GlMisc.hint `perspective_correction `nicest;
	      GlDraw.polygon_mode ~face:`both `fill;
	      GlDraw.front_face `cw;
	      Gl.enable `blend;
 	      (*Gl.enable `cull_face; *)
	      GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
	      Gl.enable `alpha_test;
	      Gl.enable `line_smooth;
	      Gl.enable `point_smooth;
	      GlFunc.alpha_func `greater ~ref:0.0 ;
	      (* 	      GlTex.parameter ~target:`texture_1d (`wrap_s `clamp); *)
	      (* 	      GlTex.parameter ~target:`texture_1d (`wrap_t `clamp); *)
	      let resizeCallback w h =
		  print_endline "resizeCallback";
		  let aspectRatio = float_of_int w /. float_of_int h in
		  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
		  GlMat.mode `projection;
		  GlMat.load_identity ();
		  GluMat.perspective ~fovy:fovY ~aspect:aspectRatio ~z:(zNear,  20000.0);
		  GlMat.mode `modelview;
		  GlMat.load_identity ();
		  (w, h)
	      in
	      
	      let windowWt, windowHt = resizeCallback windowWt windowHt in

	      let maxNumTextures = 1500 in
	      let textureHandles = GlTex.gen_textures maxNumTextures in
	      assert ( Array.length textureHandles = maxNumTextures);


	      let handleOfTexture = StringMap.empty in
	      let nextFreeTextureIndex = 0 in

	      let nomeFileCampo =
		  match surface.s_material with
		      | Clay ->gfxDir ^ "/terra.bmp.png"
		      | Cement->gfxDir ^ "/cemento.bmp.png"
		      | Grass -> gfxDir ^ "/erba.bmp.png"
	      in

	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadTextureFromFile ~fileName:nomeFileCampo
		      ~colorKey:false ~handleOfTexture ~make64x64:false
		      ~nextFreeTextureIndex ~textureHandles
	      in


	      let (handleOfTexture, nextFreeTextureIndex) =
		  accumulate ~f:(fun x (h, n) -> loadTextureFromFile ~make64x64:true 
				     ~fileName:(gfxDir ^ "/n" ^ string_of_int x ^".png")
				     ~colorKey:false ~handleOfTexture:h
				     ~nextFreeTextureIndex:n ~textureHandles) 
		      ~list:[0;1;2;3;4;5;6;7;8;9] ~state:(handleOfTexture, nextFreeTextureIndex) in

	      

	      let (handleOfTexture, nextFreeTextureIndex) =
		  let list = 
		      [ "0"; "15"; "30"; "40"; "adv"; "rete.bmp"; "too-late"; 
			"fault";
			"palla.bmp"; "croce-rossa.bmp"; "ombra-palla.bmp";
			"sprint.bmp"; "paused"; "sprint-level"; "paused-remote"]
		  in
		  accumulate
		      ~f:(fun str (h, n) -> loadTextureFromFile ~make64x64:true ~fileName:(gfxDir ^ "/" ^ str ^".png") 
			      ~colorKey:false ~handleOfTexture:h
			      ~nextFreeTextureIndex:n ~textureHandles)
		      ~state:(handleOfTexture, nextFreeTextureIndex) 
		      ~list
	      in

	      let loadAllFilesInDirAsTextures ~dir ~handleOfTexture ~nextFreeTextureIndex ~textureHandles =
    		  let makeTextureOfFile f ( ha,nextf) =
    		      print_endline ("makeTextureOfFile " ^ f);
    		      loadTextureFromFile ~make64x64:true ~fileName:f ~colorKey:true
    			  ~handleOfTexture:ha ~nextFreeTextureIndex:nextf ~textureHandles in
		  let allFilesInDir =
    		      let addPath l =
			  assert (not (mem "CVS" l));
    			  List.map (fun x -> dir ^ "/" ^ x) l in
		      let notCVS x =
			  0 != (compare x  "CVS") in
    		      addPath (filter notCVS  (Array.to_list (Sys.readdir dir))) in
    		  accumulate ~list:allFilesInDir ~f:makeTextureOfFile ~state:(handleOfTexture, nextFreeTextureIndex)


	      in



	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Aattesa")
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Asaltello") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Adestra") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Asinistra") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Agiu") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Asu") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Adritto") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Arovescio") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Arovescioback") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Adrittoback") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Adrittov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Adrittoallungov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Arovesciov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Arovescioallungov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Arovescioforwardstretch") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Brovescioforwardstretch") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Adrittoforwardstretch") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bdrittoforwardstretch") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Aservizio") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Asmash") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in


	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Battesa") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bsaltello") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bdestra") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bsinistra") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bgiu") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bsu") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bdritto") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Brovescio") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Brovescioback") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bdrittoback") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bdrittov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bdrittoallungov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Brovesciov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Brovescioallungov") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bservizio") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in
	      let (handleOfTexture, nextFreeTextureIndex) =
		  loadAllFilesInDirAsTextures ~dir: (gfxDir ^ "/Bsmash") 
		      ~handleOfTexture ~nextFreeTextureIndex ~textureHandles in







	      (* 	      Sdlttf.init(); *)
	      (* 	      let font = Sdlttf.open_font   "/usr/share/fonts/truetype/ttf-bitstream-vera/Vera.ttf" 12 in *)
	      (* 	      let letters = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p"; *)
	      (* 			     "q";"r";"s";"t";"u";"v";"w";"x";"y";"z";"1";"2";"3";"4";"5";"6";"7"; *)
	      (* 			     "8";"9";"0"] in *)
	      (* 	      let createSurf l map = *)
	      (* 		  let su = Sdlttf.render_text_solid font l (255,255,255) in *)
	      (* 		  StringMap.add l su map in *)
	      (* 	      let surfOfLetter = accumulate ~list:letters ~f:createSurf ~state:StringMap.empty in *)
	      (* 	      Sdlttf.quit (); *)





	      let score = { sc_state = NoTieBreak {points = [| 0;0 |]; games = [|0;0|]};
			    sc_finishedSets = [  ] } in

	      let court =
		  let xrepeat, yrepeat =  
		      match surface.s_material with
			  | Grass -> 5.0, 4.0 
			  | Cement -> 3.0, 4.0
			  | Clay -> 1.0, 1.0
		  in
		  let verts = [ vertexCreate leftBound 0.0 upperBound 0.0 0.0 ;
				vertexCreate rightBound 0.0 upperBound xrepeat 0.0;
				vertexCreate rightBound 0.0 lowerBound xrepeat yrepeat;
				vertexCreate leftBound 0.0 lowerBound 0.0 yrepeat ] in
		  {polyVerts = verts ;
		   polyTextureHandle = StringMap.find nomeFileCampo handleOfTexture;
		   polyColor = {r = 1.0; g = 1.0; b=1.0;a=1.0};
		   polyVisible = true }in

	      let players =
          let animdata = loadAnimdata
   		  in

		  let createHumanPlayer ~playsInTopmostCourtHalf ~plName ~scoreIndex ~startPos  =
		      let obLead =
			  match serverData with
			      | Client _ ->
				    create3dObj ~dirs:animdata  ~initialAnim:(gfxDir ^ "/Bsaltello" )
			      | Server _ | NeitherServerNorClient  ->
				    create3dObj ~dirs:animdata  ~initialAnim:(gfxDir ^ "/Bsaltello")
		      in
		      let obSlave =
			  match serverData with
			      | Client _ ->
				    create3dObj ~dirs:animdata  ~initialAnim:(gfxDir ^ "/Asaltello" )
			      | Server _ | NeitherServerNorClient ->
				    obLead
		      in
		      HP { hp_objLeading = obLead;
			   hp_objSlave = obSlave;

			   hp_pc = createPlayerCommonData ~plName;
			   hp_maxParabOpacityGroundShots = (match plName with Mats -> 0.069 | Ivan -> 0.054 | Pete -> 0.04);
			   hp_maxParabOpacityVolleys = (match plName with Mats -> 0.4 | Ivan -> 0.3 | Pete -> 0.5);

			   hp_playsInTopmostCourtHalf = playsInTopmostCourtHalf;
			   hp_fatigueData = { fatigueDivisor = 1.0;
					      fatigueStep = (match plName with Mats ->0.0001 | Ivan -> 0.00015 |Pete -> 0.0002);
					      fatiguePreviousPos = startPos;
					      fatigueAvailableSprintDistance= maxSprintCm};
			   hp_scoreIndex = scoreIndex;

			   

			   hp_state = HPS_ManualSearch { hpsms_pos = startPos;
							 hpsms_realizing = NotRealized;
							 hpsms_askedToSprintInPrevFrame = HasNotAsked;
							 hpsms_diveIsPossibleNow = DiveNotNeeded;
							 hpsms_diveHasEverBeenPossible = DivePossibilityUnknown};
			   hp_startHtOverNetForTopSpinGround = (match plName with
								    | Mats -> netHtBorder +. 65.0
								    | Pete -> netHtBorder 
								    | Ivan -> netHtBorder +. 74.0);
			   hp_startHtOverNetForBackSpinGround =
			      (match plName with Mats -> netHtBorder +. 16.0 | Pete ->
				   netHtBorder +. 6.0 | Ivan -> netHtBorder +. 6.0);
			   hp_startHtOverNetForVolleys =
			      ( match plName with 
				    | Pete -> netHtBorder
				    | Mats -> netHtBorder +. 25.0
				    | Ivan -> netHtBorder +. 35.0);
			 }
			  
			  

		  in
		  let createComputerPlayer ~playsInTopmostCourtHalf ~plName ~scoreIndex ~startPos  ~mat=

		      
		      let ob =

			  create3dObj ~dirs:animdata  ~initialAnim:(gfxDir ^ "/Asaltello")
		      and commonData = createPlayerCommonData ~plName in
		      
		      CP {
			  cp_name = plName;
			  cp_distanceOfBounceFromLine = opt.opt_skillLevel +. 1.0;
			  cp_umd = {umd_timer= 0.0;
				    umd_startPos = startPos;
				    umd_startVel = vec2dCreate 0.0 0.0 };

			  cp_obj = ob;
			  cp_pointsWonAttacking = [];
			  cp_pointsWonStayingBack = [];
			  cp_pc = commonData;

			  cp_playsInTopmostCourtHalf = playsInTopmostCourtHalf;
			  cp_fatigueData = { fatigueDivisor = 1.0;
					     fatigueStep = (match plName with Mats ->0.0001 | Ivan -> 0.00015 |Pete -> 0.0002);
					     fatiguePreviousPos = startPos;
					     fatigueAvailableSprintDistance= maxSprintCm};
			  cp_scoreIndex = scoreIndex;


			  cp_state = CPS_WaitingForBallToComeTowardsMe;

		      }
			  
			  

		  in
		  let p0 =
		      createHumanPlayer ~playsInTopmostCourtHalf:false ~plName:opt.opt_p0 ~scoreIndex:0
			  ~startPos:(vec2dCreate 0.0 (  courtHt2 +. 200.0))
		  in
		  let p1=
		      match serverData with
			  | NeitherServerNorClient ->
				createComputerPlayer ~playsInTopmostCourtHalf:true ~plName:opt.opt_p1 ~scoreIndex:1
	    			    ~mat:surface.s_material    ~startPos:(vec2dCreate 0.0 ( -. courtHt2 +. 200.0))
			  | Server _ | Client _ ->

				createHumanPlayer ~playsInTopmostCourtHalf:true ~plName:opt.opt_p1 ~scoreIndex:1
				    ~startPos:(vec2dCreate 0.0 (  -. courtHt2 +. 200.0))
		  in
		  [| p0;  p1 |] in
	      
	      
	      let ball =
		  let pol =
		      let rad = 5.2 in
		      let v1 = vertexCreate (-. rad) rad 0.0 0.0 0.0 in
		      let v2 = vertexCreate (   rad) rad 0.0 1.0 0.0 in
		      let v3 = vertexCreate rad (-.rad) 0.0 1.0 1.0 in
		      let v4 = vertexCreate (-. rad) (-.rad) 0.0 0.0 1.0 in
		      { polyVerts = [v1;v2;v3;v4];
			polyTextureHandle = StringMap.find ( gfxDir ^ "/palla.bmp.png") handleOfTexture;
			polyColor = white;
			polyVisible = true}in
		  let polyRedCross =
		      let radius = 6.0 in
		      { polyVerts = [ vertexCreate (-. radius) 0.5 (-. radius *. 2.0)
					  0.0 0.0;
				      vertexCreate radius 0.5 ( -. radius *. 2.0) 1.0
					  0.0;
				      vertexCreate radius 0.5 (radius *. 2.0) 1.0 1.0;
				      vertexCreate (-. radius) 0.5 (radius *. 2.0) 0.0
					  1.0 ];
			polyTextureHandle = (StringMap.find  (gfxDir ^ "/croce-rossa.bmp.png") handleOfTexture);
			polyColor = {r = 1.0; g = 1.0; b=1.0;a= 0.5};
			polyVisible = true} in
		  let polyBallShadow =
		      { polyRedCross with
			    polyTextureHandle = (StringMap.find  (gfxDir ^ "/ombra-palla.bmp.png") handleOfTexture) ;
			    polyColor = {r = 1.0; g = 1.0; b=1.0;a= 0.5 } }  in
		  { b_state = BS_Still (vec3dCreate (-. 620.0) 120.0 (-. courtHt2) );
		    b_polygon = pol;
		    b_redCrossPolygon = polyRedCross;
		    b_shadowPolygon = polyBallShadow;
		    b_siolpwhtb = 0 }in
	      
	      Sdlwm.set_caption ~title:pressGMessage ~icon:freeTennisString;


        (** Main loop starts here! **)
	      let rec mainLoop ~players ~ball ~score ~timer ~nextServiceIsFirst   ~vd =

		  (** Processes the queue of pending IO (mostly mouse and key) events **)
      (** This could be put on a different file, right? **)
		  let rec manageAllPendingSdlEvents vd =

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
					      manageAllPendingSdlEvents vd'

					| Sdlevent.VIDEORESIZE (w, h) ->
					      print_endline ( "Video resized" ^ string_of_int w ^
								  ", " ^ string_of_int h);
					      let w, h = resizeCallback w h in
					      manageAllPendingSdlEvents {vd with vd_windowWt = w;
									     vd_windowHt =  h}
					| Sdlevent.MOUSEBUTTONDOWN m ->
					      let mouse =
						  if m.mbe_button = Sdlmouse.BUTTON_LEFT then
						      { vd.vd_mouse with m_leftButtonPressed = true}
						  else if m.mbe_button = Sdlmouse.BUTTON_RIGHT then
						      { vd.vd_mouse with m_rightButtonPressed = true}
						  else
						      vd.vd_mouse
					      in
					      manageAllPendingSdlEvents {vd with vd_mouse = mouse}
					| Sdlevent.MOUSEBUTTONUP m ->
					      let mouse =
						  if m.mbe_button = Sdlmouse.BUTTON_LEFT then
						      { vd.vd_mouse with m_leftButtonPressed = false}
						  else if m.mbe_button = Sdlmouse.BUTTON_RIGHT then
						      { vd.vd_mouse with m_rightButtonPressed = false}
						  else
						      vd.vd_mouse
					      in
					      manageAllPendingSdlEvents {vd with vd_mouse = mouse}
					| Sdlevent.ACTIVE e ->
					      manageAllPendingSdlEvents vd

					| Sdlevent.MOUSEMOTION m ->

					      if m.mme_xrel = 0 && m.mme_yrel =0 then
						  (* @@ why does this happen sometimes? *)
						  manageAllPendingSdlEvents 
						      {vd with vd_mouse = { vd.vd_mouse with
										m_xRel = 0;
										m_yRel = 0;
										m_secondsSinceLastMouseMotion = 0.0}}
					      else

						  let warpx =
						      if m.mme_x > windowWt - 20 then
							  Some 20
						      else if m.mme_x < 20 then
							  Some (windowWt - 20)
						      else
							  None
						  in
						  let warpy =
						      if m.mme_y > windowHt - 20 then
							  Some 20
						      else if m.mme_y < 20 then
							  Some (windowHt - 20)
						      else
							  None
						  in
						  if Sdlwm.query_grab () then
						      begin
							  match warpx with
							      | None ->
								    begin
									match warpy with
									    | None -> ()
									    | Some y ->
										  Sdlmouse.warp m.mme_x y
								    end
							      | Some x ->
								    begin
									match warpy with
									    | None ->
										  Sdlmouse.warp x m.mme_y
									    | Some y ->
										  Sdlmouse.warp x y
								    end
						      end
						  else
						      ();

						  let mouse =
						      if Sdlwm.query_grab () then
							  let xr =
							      if abs m.mme_xrel < windowWt - 50 then
								  m.mme_xrel
							      else
								  vd.vd_mouse.m_xRel 
							  in
							  let yr =
							      if abs m.mme_yrel < windowHt - 50 then
								  m.mme_yrel
							      else
								  vd.vd_mouse.m_yRel 
							  in
							  {vd.vd_mouse with m_xRel = xr;
							       m_yRel = yr;
							       m_secondsSinceLastMouseMotion = 0.0} 
						      else
							  vd.vd_mouse
						  in
						  manageAllPendingSdlEvents {vd with vd_mouse = mouse}
					| JOYAXISMOTION _ | JOYBALLMOTION _ | JOYHATMOTION _ | JOYBUTTONDOWN _ | JOYBUTTONUP _
					| SYSWM  | VIDEOEXPOSE  | USER _ | KEYUP _ ->
					      manageAllPendingSdlEvents vd
				end
				    
		  in (* manageAllPendingSdlEvents *)

		  let vd = 
		      
		      manageAllPendingSdlEvents vd
		  in

		  let vd = 
		      if Sdlkey.is_key_pressed Sdlkey.KEY_i then
			  if vd.vd_deltaCamera < 42.0 then
			      { vd with vd_deltaCamera = vd.vd_deltaCamera +. 0.1}
			  else
			      vd
		      else if Sdlkey.is_key_pressed Sdlkey.KEY_k then
			  if vd.vd_deltaCamera >  7.0 then
			      { vd with vd_deltaCamera = vd.vd_deltaCamera -.  0.1}
			  else 
			      vd
		      else
			  vd
		  in

		  
		  (* send and receive quit state *)

		  (match serverData with
		       |   NeitherServerNorClient -> 
			       ()
		       | Client (_, _, out) ->
			     ()
		       | Server (_, _, out) ->
			     (Marshal.to_channel out vd.vd_mustQuit [];
			      flush out));
		  let mustQuit = 
		      match serverData with
			  | NeitherServerNorClient ->
				vd.vd_mustQuit
			  | Client (_, inp, _) ->
				
				(Marshal.from_channel inp : bool) 
			  | Server (_, inp, _) ->
				vd.vd_mustQuit
		  in

		  (* send and receive pause state *)
		  
		  (match serverData with
		       |   NeitherServerNorClient -> 
			       ()
		       | Client (_, _, out) ->
			     ( Marshal.to_channel out vd.vd_pausedWithKey [];
			       flush out)
		       | Server (_, _, out) ->
			     ( Marshal.to_channel out vd.vd_pausedWithKey [];
			       flush out)
		  );

		  let pausedOnTheOtherSide = 
		      match serverData with
			  |   NeitherServerNorClient -> 
				  false
			  | Client (_, inc, _) ->
				( Marshal.from_channel inc : bool)
			  | Server (_, inc, _) ->
				( Marshal.from_channel inc : bool)
		  in


		  if mustQuit then
		      ()
		  else
		      ( GlClear.clear [`color; `depth];
			
			let camData =
			    let fovx = fovY *. (float_of_int windowWt) /.(float_of_int windowHt) in
			    let whoPlaysBelow =
				if playsInTopmostCourtHalf players.(0) then
				    ( assert( not ( playsInTopmostCourtHalf players.(1)));
				      1 )
				else 0 in
			    let ballP =
				match ball.b_state with
				    | BS_Still _ -> vec2dCreate 0.0 0.0
				    | BS_Moving _ ->
					  (projection2d (curBallPos ball)) in

			    match serverData with
				| Server _  | NeitherServerNorClient -> 
				      let thePlayerAboveIsHuman =
					  match players.(1- whoPlaysBelow) with
					      | HP _ -> true | CP _ -> false in
				      
				      calculateCamera ~fovy:(degToRad fovY) ~fovx:(degToRad fovx) ~znear:zNear
					  ~posBottomPlayer:(curPosOfPlayer players.(whoPlaysBelow))
					  ~posTopmostPlayer:(curPosOfPlayer players.( 1- whoPlaysBelow))
					  ~posBall:ballP ~xCamBehav:PushScroll ~deltaCameraBackwards:vd.vd_deltaCamera
					  ~mustShowBottomCourtLine:thePlayerAboveIsHuman  
				| Client _ ->
				      let flippedCam = 
					  calculateCamera ~fovy:(degToRad fovY) ~fovx:(degToRad fovx) ~znear:zNear
					      ~posBottomPlayer:(flipxz2 (curPosOfPlayer players.(1 - whoPlaysBelow)))
					      ~posTopmostPlayer:(flipxz2 (curPosOfPlayer players.(  whoPlaysBelow)))
					      ~posBall:(flipxz2 ballP) ~xCamBehav:PushScroll ~deltaCameraBackwards:vd.vd_deltaCamera
					      ~mustShowBottomCourtLine:false  in
				      let flipCamData d = 
					  { d with
						eyeX = -. d.eyeX;
						eyeZ = -. d.eyeZ;
						lookatX = -. d.lookatX;
						lookatZ = -. d.lookatZ} in
				      flipCamData flippedCam
			in
			GlMat.load_identity ();
			GluMat.look_at ~eye:(camData.eyeX, camData.eyeY, camData.eyeZ)
			    ~center:(camData.lookatX, camData.lookatY, camData.lookatZ)
			    ~up:(0.0, 1.0, 0.0) ;

			

			
			let timer =
			    match serverData with
				| Server _ ->
				      updateTimer ~tim:timer ~slowMotionFactor:vd.vd_slowMotionFactor
					  
				| Client (_,  inc, _) ->
				      (
					  (Marshal.from_channel inc : timerData)
				      )
					  

				| NeitherServerNorClient ->
				      updateTimer ~tim:timer ~slowMotionFactor:vd.vd_slowMotionFactor
			in
			

			( match serverData with
			      | Server (_, _, outc) ->
				    ( 
					Marshal.to_channel outc timer [];
					flush outc
				    )
			      | Client _ | NeitherServerNorClient  -> () );

			let dt = calcDt ~timer ~slowMotionFactor:vd.vd_slowMotionFactor in

			let noOneIsServing =
			    let isNotServing p =
				match p with
				    | HP h ->
					  (match h.hp_state with
					       | HPS_ServingBeforeLaunch _ -> false
					       | HPS_ServingAfterLaunchAndBeforePressingButton _ -> false
					       | HPS_ServingAfterPressingButton _ -> false
					       | HPS_ServingAfterHittingBall _ -> false
					       | HPS_ManualSearch _ | HPS_RealizingWhereTheBallIs _ 
					       | HPS_AutoSearchAfterOpening _ | HPS_AutoSearchAfterImpactWaitingForAnimToEnd _ 
					       | HPS_AutoSearchBeforeOpening _ | HPS_GettingUpAfterDive _
					       | HPS_DivingFake _ -> true)
				    | CP c ->
					  (match c.cp_state with
					       | CPS_ServingBeforeLaunch _ -> false
					       | CPS_ServingAfterLaunchAndBeforeStartingGesture _ -> false
					       | CPS_ServingAfterHittingBall -> false
					       | CPS_ServingAfterStartingGesture _ -> false
					       | CPS_RealizingWhereTheBallIs | CPS_TheAnimationIsTerminating _ 
					       | CPS_GetBackToCenterAtPointFinished _ | CPS_ResearchAfterDecidingTheShot _ 
					       | CPS_ResearchBeforeDecidingTheShot _ | CPS_WaitingForBallToComeTowardsMe 
					       | CPS_WaitingForANewPointToBegin | CPS_GetBackToCenterDuringGame _ -> true)

			    in
			    isNotServing players.(0) && isNotServing players.(1) in

			let players =

			    let players =
				match ball.b_state with
				    | BS_Moving _ ->
					  players
				    | BS_Still _ ->

					  if noOneIsServing then
					      let transf p =
						  let p = resetFatigue p in
						  if (scoreIndex p) = whoServes score then  
						      match p with
							  | HP  h ->
								let st, ooLe, ooSl = startServiceHuman ~h ~scoreIsEven:(serviceIsToTheRight score)
								    ~serverData in
								HP { h with hp_state = st; hp_objLeading = ooLe; hp_objSlave = ooSl}
							  | CP c ->
								let attackPref = intentionsDebug c in
								print_endline ("attackPref  " ^ string_of_int attackPref);
								let st, oo, umd = startServiceComputer ~h:c ~scoreIsEven:(serviceIsToTheRight score) in
								CP { c with cp_state = st; cp_obj = oo; cp_umd = umd}
						  else
						      let preparePlayerForReceiving ~p ~scoreIsEven =
							  match p with
							      | HP  h ->
								    let prepareHumanPlayerForReceiving ~h ~scoreIsEven =
									let x, z =
									    let dirsign = if h.hp_playsInTopmostCourtHalf  then -. 1.0 else 1.0 in
									    if scoreIsEven then
										(dirsign *. (courtWt2 -. 50.0),
										 dirsign *. (courtHt2 +. 190.0))
									    else
										( dirsign *. (-. courtWt2 +. 50.0)  ,
										  dirsign *. (courtHt2 +. 190.0 )) in
									let prefixSlave = 
									    match serverData with
										| Client _ ->
										      gfxDir ^ "/A"
										| Server _ ->
										      gfxDir ^ "/A"
										| NeitherServerNorClient ->
										      if h.hp_playsInTopmostCourtHalf then gfxDir ^ "/B" else gfxDir ^ "/A"
									in
									let prefixLead = 
									    match serverData with
										| Client _ ->
										      gfxDir ^ "/B"
										| Server _ ->
										      gfxDir ^ "/B"
										| NeitherServerNorClient ->
										      if h.hp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B"
									in
									let newObLead = 
									    let animName = 
										prefixLead ^ "saltello" in
									    setAnim ~o:h.hp_objLeading ~animName ~restartIfSameAnimation:true in
									let newObSlave = 
									    let animName = 

										prefixSlave ^ "saltello" in
									    setAnim ~o:h.hp_objSlave ~animName ~restartIfSameAnimation:true in


									let m = 

									    { hpsms_pos = vec2dCreate x z;
									      hpsms_realizing = NotRealized;
									      hpsms_askedToSprintInPrevFrame = HasNotAsked;
									      hpsms_diveIsPossibleNow = DiveNotNeeded;
									      hpsms_diveHasEverBeenPossible = DivePossibilityUnknown} in
									HP { h with 
										 hp_objSlave = newObSlave;
										 hp_objLeading = newObLead;
										 hp_state = HPS_ManualSearch m} in

								    prepareHumanPlayerForReceiving ~h ~scoreIsEven
							      | CP c ->
								    let prepareComputerPlayerForReceiving ~c ~scoreIsEven =
									let x, z =
									    let dirsign = if c.cp_playsInTopmostCourtHalf  then -. 1.0 else 1.0 in
									    if scoreIsEven then
										(dirsign *. (courtWt2 -. 50.0),
										 dirsign *. (courtHt2 +. 190.0))
									    else
										( dirsign *. (-. courtWt2 +. 50.0)  ,
										  dirsign *. (courtHt2 +. 190.0 )) 
										    
									and ob = 
									    let animName = 
										let prefix = if c.cp_playsInTopmostCourtHalf 
										then gfxDir ^ "/A" else gfxDir ^ "/B" in

										prefix ^ "saltello" in
									    setAnim ~o:c.cp_obj ~animName ~restartIfSameAnimation:true in

									CP { c with
										 cp_state = CPS_WaitingForBallToComeTowardsMe ;
										 cp_umd = {umd_startVel = vec2dCreate 0.0 0.0;
											   umd_startPos = vec2dCreate x z;
											   umd_timer = 0.0};
										 cp_obj = ob;
									   } in
								    
								    let attackPref = intentionsDebug c in
								    print_endline ("attackPref  " ^ string_of_int attackPref);

								    prepareComputerPlayerForReceiving ~c ~scoreIsEven
									
						      in
						      preparePlayerForReceiving ~p ~scoreIsEven:(serviceIsToTheRight score)
					      in
					      Array.map transf players
					  else
					      players
			    in
			    
 			    match serverData with
				| Server (_, _, outc) ->
				      (
					  Marshal.to_channel outc players [];
					  flush outc;
					  players )
				| Client (_, inc, _) ->
				      ( 
					  (Marshal.from_channel inc : player array))
				| NeitherServerNorClient ->
				      players

			in

			let ball, players, mouse =
			    if vd.vd_pausedWithKey  || pausedOnTheOtherSide then
				ball, players, vd.vd_mouse
			    else
				let lockOf p =
				    match p with
					| HP h ->
					      begin
						  match h.hp_state with
						      | HPS_AutoSearchAfterOpening q ->
							    let v = q.asao_Impact in
							    let tim = q.asao_TimeToRunFromOpeningToImpact -.
								q.asao_UniformMotionData.umd_timer in
							    HasLocked (projection2d v, tim)
						      | HPS_AutoSearchBeforeOpening q ->
							    let v = q.asbo_Impact in
							    let tim = q.asbo_TimeToRunFromOpeningToImpact -.
								q.asbo_UniformMotionData.umd_timer in
							    HasLocked (projection2d v, tim)
						      | HPS_ServingBeforeLaunch _ 
						      | HPS_ServingAfterLaunchAndBeforePressingButton _
						      | HPS_ServingAfterPressingButton _
						      | HPS_ServingAfterHittingBall _
						      | HPS_ManualSearch _
						      | HPS_RealizingWhereTheBallIs _
						      | HPS_AutoSearchAfterImpactWaitingForAnimToEnd _
						      | HPS_GettingUpAfterDive _
						      | HPS_DivingFake _ ->
							    HasNotLocked
					      end
					| CP c ->
					      HasNotLocked (*@@*)
				in
				let newball, newp0, newmouse, soundIds =
				    match serverData with
					| Server _ ->
 					      (match players.(0) with
						   | HP h ->
							 updateHumanPlayer ~p:h ~dt ~b:ball ~opt ~serverData
							     ~opponentCurPos:(curPosOfPlayer players.(1)) 
							     ~mouse:vd.vd_mouse ~mouseSensitivity ~surf:surface  
						   | CP c ->
							 assert(false))

					| Client (_, inc, _) ->
					      
					      let remoteB, remoteP0, sounds =
						  (Marshal.from_channel inc : ball * player * soundId list) in
					      
					      remoteB, remoteP0, vd.vd_mouse (* leave the same mouse! *), sounds

					| NeitherServerNorClient ->
 					      (match players.(0) with
						   | HP h ->
							 updateHumanPlayer ~p:h ~dt ~b:ball ~opt ~serverData
							     ~opponentCurPos:(curPosOfPlayer players.(1)) 
							     ~mouse:vd.vd_mouse ~mouseSensitivity ~surf:surface  
						   | CP c ->
							 updateComputerPlayer ~p:c ~dt ~b:ball ~opt ~aidebug:opt.opt_aidebug
							     ~opponentCurPos:(curPosOfPlayer players.(1)) ~surf:surface
							     ~nextServiceIsFirst
							     ~opponentLock:(lockOf players.(1)) ~mouse:vd.vd_mouse ~sounds )
				in

				List.iter (fun x -> playSoundId ~sounds ~id:x) soundIds;

				( match serverData with

				      | Server (_, _, outc) ->

					    ( Marshal.to_channel outc (newball, newp0, soundIds) [] ;
					      flush outc)

				      | Client _ | NeitherServerNorClient -> ());

				let newball2, newp1, newmouse2, soundIds =
				    match serverData with
					| NeitherServerNorClient ->
					      ( match players.(1) with
						    | HP h ->
							  updateHumanPlayer ~p:h ~dt ~b:newball ~opt 
							      ~opponentCurPos:(curPosOfPlayer players.(0)) ~serverData
							      ~mouse:newmouse ~mouseSensitivity ~surf:surface 
						    | CP c ->
							  updateComputerPlayer ~p:c ~dt ~b:newball ~opt ~aidebug:opt.opt_aidebug
							      ~opponentCurPos:(curPosOfPlayer players.(0)) ~surf:surface
							      ~nextServiceIsFirst
							      ~opponentLock:(lockOf players.(0)) ~mouse:newmouse ~sounds)
					| Server (_, inc, _) ->
					      let remoteB, remoteP0, sou =
						  (Marshal.from_channel inc : ball * player * soundId list) in
					      
					      remoteB, remoteP0, newmouse, sou
					| Client _ ->

					      ( match players.(1) with
						    | HP h ->
							  updateHumanPlayer ~p:h ~dt ~b:newball ~opt 
							      ~opponentCurPos:(curPosOfPlayer players.(0)) ~serverData
							      ~mouse:newmouse ~mouseSensitivity ~surf:surface 
						    | CP c ->
							  assert(false))
				in
				List.iter (fun x -> playSoundId ~sounds ~id:x) soundIds;
				( match serverData with
				      | Client (_, _, outc)  ->
					    ( Marshal.to_channel outc (newball2, newp1, soundIds) [];
					      flush outc)
				      | Server _ | NeitherServerNorClient -> ());

				(newball2, [| newp0 ; newp1 |], newmouse2) in

			let vd = {vd with vd_mouse = mouse} in

			let score, ball , nextServiceIsFirst, players =
			    if vd.vd_pausedWithKey || pausedOnTheOtherSide then
				score, ball, nextServiceIsFirst, players
			    else
				updateBall ~b:ball ~dt ~score ~surf:surface ~sounds ~nextServiceIsFirst ~opt ~players in
			
          (* FIXME: Move this back inside render() *)
          renderPolygon court None ;
          render ~players:players ~ball:ball ~aidebug:opt.opt_aidebug ~serverData:serverData
				          ~camData:camData ~handleOfTexture:handleOfTexture
                  ~realisticParabolaOpacity:opt.opt_realisticParabolaOpacity ();
          
          (* FIXME: Move this back inside render() *)      
			    render2dStuff ~players:players ~ball:ball ~serverData:serverData 
                      ~doNotShowPause:opt.opt_doNotShowPause ~pausedOnTheOtherSide:pausedOnTheOtherSide
                      ~pausedWithKey:vd.vd_pausedWithKey  ~noOneIsServing:noOneIsServing  ~windowHt:windowHt
                      ~windowWt:windowWt ~handleOfTexture:handleOfTexture ~s:score;
			    Gl.flush ();


			Sdlgl.swap_buffers ();


			if opt.opt_showFps then
			    match timer with
				| TimerData0 -> ()
				| TimerData1 _ -> ()
				| TimerData2 t ->
				      if t.t2_numFramesSinceLastFpsUpdate = 0 then
					  print_endline (  "in " ^
							       string_of_int fpsRefreshRate ^ 
							       " milliseconds you did " ^ 
							       string_of_int (hd t.t2_frameCountList) ^ " frames." )
				      else
					  ()
			else
			    ();


			mainLoop ~players ~ball ~score ~timer  ~vd ~nextServiceIsFirst  
		      )
	      in
	      
	      mainLoop ~players ~ball ~score ~timer:TimerData0
		  ~nextServiceIsFirst:true
		  ~vd:{
		       vd_pausedWithKey =false;
		       vd_windowHt = windowHt;
		       vd_windowWt = windowWt;
		       vd_mustQuit = false;
		       vd_mouse = {
			   m_rightButtonPressed = false;
			   m_leftButtonPressed = false; m_xRel = 0; m_yRel = 0 ;
			   m_secondsSinceLastMouseMotion = 0.0} ;

		       vd_slowMotionFactor = 1.0;
		       vd_fullScreen=false;
		       vd_deltaCamera = 16.0 (* degrees *) };
	      
	      (match serverData with
		       (* the rule is to shutdown before you close,
			  but this is often automatic. see sockets
			  howto.

		       *)
		   | Server( (sock, clientSocket), inc, outc) ->
			 print_endline "Shutting down socket";
  			 Unix.shutdown clientSocket Unix.SHUTDOWN_ALL ; 
  			 Unix.shutdown sock Unix.SHUTDOWN_ALL ; 
  			 Unix.close clientSocket;  
  			 Unix.close sock;  
		   | Client ( sock, inc, outc) ->
			 print_endline "Shutting down socket";
  			 Unix.shutdown sock Unix.SHUTDOWN_ALL  ;
 			 Unix.close sock; 

		   | NeitherServerNorClient -> ());
	      Sdl.quit();
	      exit 0
