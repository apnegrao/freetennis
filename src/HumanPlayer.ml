open PlayerData

open Math

open Input

open SharedData

open BallMovement

open Sound

open Objects3D

open Network

(**--- FIXME: Ideally, the animation module should not be needed here---**)
open Animation

(** --- Functions --- **)
let createHumanPlayer ~playsInTopmostCourtHalf ~plName ~scoreIndex ~startPos 
                    ~serverData ~animdata =
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

let calcMinShotPower ~ballVelZ ~exploit = 
    (* determines how easy drop volley is. with 1.4 it is almost impossible *)
    abs_float ballVelZ *. exploit *. 1.3

let calcMaxShotPower ~ballVelZ ~exploit ~search ~maxPow = 
    let po =
	match search with
	    | RKH_Dive _ | RKH_StretchForward _ -> maxPow *. powerAttenuationForStretchForwardAndDive
	    | RKH_Normal VOT_Volee -> maxPow *. powerAttenuationForVolee
	    | RKH_Normal VOT_NotVolee _ | RKH_Smash _ -> maxPow
    in
    po +. (abs_float ballVelZ) *. exploit 

(**Service handling**)
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

(** FIXME: This is the function that matters. However, it has over 2000 lines of code,
which makes it really difficult to understand...**)
(*
The arguments of this function are:
p:	The human player (type human player)
dt:	Seconds since last frame (whatever a frame is...)
b:	The ball
opponentCurPos:	The current position of the computer player
mouse:	The mouse (type mouse)
mouseSensitivity:
surf:	Surface
serverData:	Network data.
@returns:	
*)
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
    
    
(** I think the functions so far serve to calculate the initial trajectory of the ball
	(i.e., before the user's input is taken into account. Based on its name, this next one
	seems to be the one that adjusts the height of the trajectory over the net based on the
	user's mouse-controlled input  **)
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
 	(* Right mouse button pressed; the player went for topspin or lob*)
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
	(* Left mouse button pressed. This means that the player went for a more flat trajectory *)
	else if down then
	    (* what can happen when I push down? The parabola can 
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


(** Again based on the name, this next function seems to be the one that
	adjusts the placement of the shot according to the mouse movement. **)
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
					  upperLeftServiceBox
				      else
					  upperRightServiceBox
				  else
				      if h.hpssapb_ToTheRight then
					  lowerLeftServiceBox
				      else
					  lowerRightServiceBox in		    
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

