open Math

open SharedData

open BallMovement

open Objects3D

(** --- Shared Data --- **)
let sprintSpeed ~beta ~speedFreeRun =
  (* the sprint speed is 1.2 *. normal speed if you are moving horizontally,
     plus a bonus if you are moving vertically . *)
  (1.2 +. 1.16 *. abs_float (cos beta) ) *. speedFreeRun


(* the time from when the ball is hit to when the player realizes its
   direction. Is of crucial importance for passing shot tuning 

   Update: this is now set to zero. it is made obsolete by the fact
   that we now have different running speed when under net. The new
   solution allows tuning for passing shots and, at the same time,
   removes the following problem: with reflex time != 0, sometimes
   there is not enough time for normal volee, but there is time for
   dive, even though the ball is in your chest.

*)
let reflexDeltaT = 0.0

let powerAttenuationForVolee = 0.55

let powerAttenuationForStretchForwardAndDive = 0.5

type playerCommon = {
  pc_maxShotHt:float; 
  pc_maxSmashHt:float; 
  pc_minSmashHt:float;
  pc_firstServiceXAngleInDeg:float;
  pc_secondServiceXAngleInDeg:float;
  pc_firstServiceSpeedKmh: float;
  pc_secondServiceSpeedKmh:float;
  pc_firstServiceSpin: float;
  pc_secondServiceSpin: float;

  pc_tendsToAnticipateGroundShots:bool; 
  pc_prefersVolleysToGroundShots: bool; 

  pc_topSpin: float;
  pc_backSpin: float;
  pc_maxSpeedInFreeRunUnderNet:float;
  pc_maxSpeedInNormalResearchUnderNet:float;
  pc_maxSpeedInFreeRunStayBack:float;
  pc_maxSpeedInNormalResearchStayBack:float;
  pc_maxShotPower:float;
  pc_exploitationOfOpponentsPower: float;
}

(* XXX: Why isn't the fatigueData in the playerCommon? I think the playerCommon data are all constants. 
   Still, why couldn't it also have dynamic data? Not only fatigueData, in fact, there are other
   variables common to both players: scoreIndex, playsInTopMostHalf*)
type fatigueData = 
  { fatigueDivisor:float;
    fatigueStep:float;
    fatiguePreviousPos:vec2d;
    fatigueAvailableSprintDistance:float
  }

type playerName = Pete | Mats | Ivan

type attackIntention = StayBack | AttackWithPowerShot | AttackApproach


type spinKind = Topspin | Backspin

type voleeOrTopspin = VOT_Volee | VOT_NotVolee of spinKind

type voleeOrTopspinAndIntention = VOTI_Volee | VOTI_NotVolee of spinKind * attackIntention

type volleyOrIntention = Volley | NotVolley of attackIntention

type wonLost = Won | Lost

type uniformMotionData = 
  { umd_startPos: vec2d;
    umd_startVel: vec2d;
    umd_timer: float}

type askData = HasAskedAndObtained of vec2d | HasAskedAndWasDenied | HasNotAsked

type divePossibility = DivePossible | DiveNotPossible | DiveNotNeeded

type realizingState = Realized | NotRealized

type reasonForDiveMiss = DiveTooLate | DiveOnShotTooFar | DiveWithNoNeed

type diveHasEverBeenPossible = DiveHasBeenPossible | DiveHasNeverBeenPossible | DivePossibilityUnknown

let stringOfAttackIntention a = 
  match a with
  | StayBack -> "StayBack"
  | AttackApproach -> "AttackApproach"
  | AttackWithPowerShot -> "AttackWithPowerShot"

let maxSprintCm = 800.0

type attackPolicyNotService = APNS_AttackApproach | APNS_AttackPowerShot | APNS_StayBack

let updateFatigue f newPos = 
  let delta = vec2dSub newPos f.fatiguePreviousPos in
  let div = f.fatigueDivisor +. f.fatigueStep *. (length2d delta) in
  { f with
    fatiguePreviousPos = newPos;
    fatigueDivisor = div }

let createPlayerCommonData ~plName =
  {
    pc_maxShotHt = (match plName with Mats -> 180.0 | Ivan -> 200.0 |
        Pete -> 200.0);
    pc_maxSmashHt = (match plName with Mats ->300.0 | Ivan ->350.0 |
        Pete -> 350.0);
    pc_minSmashHt = (match plName with Mats ->250.0 | Ivan ->290.0 |
        Pete -> 290.0);
    pc_prefersVolleysToGroundShots =  (match plName with Mats -> false
                                                       | Ivan -> false | Pete -> true);
    pc_firstServiceXAngleInDeg = 7.8;
    pc_secondServiceXAngleInDeg = 6.3;
    pc_firstServiceSpeedKmh = 192.0;
    pc_secondServiceSpeedKmh = 145.0;
    pc_firstServiceSpin = 0.2; (* @@ 0.0 crashes *)
    pc_secondServiceSpin = 500.0;
    pc_tendsToAnticipateGroundShots = (match plName with Mats -> false | Ivan
        -> false | Pete -> true);

    pc_maxSpeedInFreeRunStayBack =
      (match plName with Mats -> 592.0 | Ivan-> 587.0 | Pete ->582.0);
    pc_maxSpeedInNormalResearchStayBack =
      (match plName with Mats -> 592.0 |Ivan -> 587.0 | Pete ->582.0);

    (* with 420.0 I pass Pete with Pete too easily on cement.*)
    pc_maxSpeedInFreeRunUnderNet = 420.0; 
    pc_maxSpeedInNormalResearchUnderNet = 420.0;

    (* Regolare prima lo spin, perche' influenza in modo
       			     drastico pc_maxShotPower e pc_exploitationOfOpponentsPower.

       			     per capire se il topspin e' troppo alto,
       			     vedi quanto pagano le palle strette. Se sono
       			     troppo facili, il topspin e' troppo alto 

       			     poi regola a zero pc_exploitationOfOpponentsPower, cosi' che la forza e' tutta sua.
       			     E regola pc_maxShotPower verificando che non possa
       			     tirare mazzate a chiudere.

       			     Poi aumenta pc_exploitationOfOpponentsPower fino a che le mazzate a 
       			     chiudere sono possibili quando il colpo dell'altro e' abbast veloce.
       			  *)
    pc_topSpin = (match plName with Mats -> 720.0 | Ivan -> 700.0 
                                  | Pete -> 500.0); 
    pc_backSpin = (match plName with Mats -> -. 405.0 | Ivan ->
        -.  405.0 | Pete -> -. 250.0);
    pc_maxShotPower = (match plName with Mats -> 2040.0  | Ivan -> 2650.0 | Pete -> 2550.0);
    (* consider that ballVelZAtImpactTime is typically 1300 on cement, 1100 on clay *)
    pc_exploitationOfOpponentsPower = 0.3 (*(match plName with Mats -> 0.3| Ivan -> 0.2 | Pete ->0.3); *)
  }

(**--------------Computer Player---------------- FIXME: Why isn't this on the
   ComputerPlayer Module?**) 
type researchKindComputer = RKC_Smash of volleyOrIntention
                          | RKC_Normal of voleeOrTopspinAndIntention 
                          | RKC_StretchForward of volleyOrIntention

(* Research After Deciding The Shot *)
type radts = {
  radts_Trajectory:trajectory;
  radts_Forehand: bool;
  radts_TimeToRunFromOpeningToImpact: float;
  radts_RunSpeedFromOpeningToImpact: vec2d;
  radts_FootTarget: vec2d;
  radts_Impact: vec3d;
  radts_researchKind: researchKindComputer;
  radts_BallVelAtImpact:vec3d
}

(* Research Before Deciding The Shot *)
type rbdts = { 
  rbdts_forehand: bool;
  rbdts_timeToRunBeforeOpening: float;
  rbdts_timeToRunFromOpeningToImpact: float;
  rbdts_runSpeedBeforeOpening: vec2d;
  rbdts_runSpeedFromOpeningToImpact: vec2d;
  rbdts_footTarget: vec2d;
  rbdts_impact: vec3d;
  rbdts_researchKind: researchKindComputer;
  rbdts_ballVelAtImpact:vec3d
}

type computerPlayerState =   CPS_ResearchBeforeDecidingTheShot of rbdts
                         | CPS_ResearchAfterDecidingTheShot of radts
                         | CPS_GetBackToCenterDuringGame of  volleyOrIntention * 
                                                             vec2d (* targetPos*) * vec2d (* optimalPosition *)
                         | CPS_GetBackToCenterAtPointFinished of float (* time to stop *)
                         | CPS_WaitingForBallToComeTowardsMe
                         | CPS_WaitingForANewPointToBegin
                         | CPS_RealizingWhereTheBallIs 
                         | CPS_TheAnimationIsTerminating of volleyOrIntention
                         | CPS_ServingBeforeLaunch of bool 
                         | CPS_ServingAfterLaunchAndBeforeStartingGesture of bool * float (*timer *)
                         | CPS_ServingAfterStartingGesture of bool  * float (* timer *)
                         | CPS_ServingAfterHittingBall 


type computerPlayer = { cp_obj: obj3d; 
                        cp_pc: playerCommon;
                        cp_name : playerName;
                        cp_playsInTopmostCourtHalf: bool;
                        cp_fatigueData : fatigueData;
                        cp_scoreIndex:int;
                        cp_state: computerPlayerState;
                        cp_pointsWonAttacking: wonLost list;
                        cp_pointsWonStayingBack: wonLost list;
                        cp_umd: uniformMotionData;
                        cp_distanceOfBounceFromLine:float }

let curPosOfComputerPlayer c = 
  vec2dAdd c.cp_umd.umd_startPos (vec2dMulScalar c.cp_umd.umd_timer c.cp_umd.umd_startVel )

(* small, auxiliary functions for AI decisions. TODO: This is for the computer
   player only, so why is it here?*)
module AI = struct

  let isVeryDecenteredBackwards pos = abs_float pos.z2 > 1500.0 

  let isAttacking pos = abs_float pos.z2 < courtHt4 +. 101.0 

  let isABitDecentered h pos = 
    match h with
    | Right -> pos.x2 > 330.0
    | Left -> pos.x2 < -. 330.0

  let isVeryLittleDecentered h pos = 
    match h with
    | Right -> pos.x2 > 230.0
    | Left -> pos.x2 < -. 230.0

  let isQuiteDecentered h pos = 
    match h with
    | Right -> pos.x2 > courtWt2 -. 30.0
    | Left -> pos.x2 < -. courtWt2 +. 30.0


  let isABitDecenteredHorizontally pos = 
    exists [Left;Right] (fun h -> isABitDecentered h pos )

  let isVeryDecentered h pos = 
    match h with
    | Right -> pos.x2 > 441.0
    | Left -> pos.x2 < -. 441.0

  let isVeryDecenteredHorizontally pos =
    exists [Left;Right] (fun x -> isVeryDecentered x pos)

  let isInNoMansLand pos = 
    let z = abs_float pos.z2 in
    756.0 <= z && z <= courtHt2 +. 20.0

  let interpolateVote x xSinistra xDestra votoXSin votoXDes = 
    assert (xSinistra < xDestra);
    let a = 
      if votoXSin <= votoXDes then
        (abs_float (votoXDes -. votoXSin)) /. (abs_float (xDestra
                                                          -. xSinistra))
      else
        -. ( (abs_float (votoXDes -. votoXSin)) /. (abs_float (xDestra
                                                               -. xSinistra)))
    in
    votoXSin +. a *. ( x -. xSinistra)


  let voteClosenessToNet p = 
    let z = abs_float p.z2 in
    let bestz = 0.0 in
    let worstz = abs_float upperBound in
    interpolateVote z bestz  worstz 10.0 0.0

  let voteHorizontalCentering p = 
    let x = abs_float p.x2 in
    let bestx = 0.0 in
    let worstx = abs_float rightBound in
    interpolateVote x bestx worstx 10.0 0.0

  let voteClosenessToGroundLine p = 
    let z = abs_float p.z2 in
    let bestz = abs_float courtHt2 +. 100.0 in
    if z <= bestz then
      interpolateVote z 0.0 bestz 0.0 10.0
    else
      interpolateVote z bestz (abs_float upperBound) 10.0 0.0

  let voteNotTooMuchBehindGroundLine p = 
    let z = abs_float p.z2 in
    if z <= courtHt2 then
      10.0
    else
      interpolateVote z courtHt2 (abs_float upperBound)   10.0 0.0

  let voteImpactHtTheHigherTheBetter i maxSmashHt = 
    let y =  i.y3 in
    let besty = maxSmashHt in
    interpolateVote y 0.0  besty 0.0 10.0

  let voteRunSpeed sp maxSp = 
    interpolateVote sp 0.0 maxSp 10.0 0.0

  let voteImpactHtCloseTo altezzaMigl i maxSmashHt = 
    let y =  i.y3 in
    if y < altezzaMigl then
      interpolateVote y 0.0 altezzaMigl 0.0 10.0
    else if y > altezzaMigl then
      interpolateVote y altezzaMigl maxSmashHt  10.0 0.0
    else
      10.0
end

(**--------------Human Player----------------**)
let spinForVolee = -. 2.0 (* @@ discover why 0.0 crashes when I do volee *)

type researchKindHuman = RKH_Smash of bool (* volee *) 
                       | RKH_Normal of voleeOrTopspin 
                       | RKH_Dive of bool
                       | RKH_StretchForward of bool

type ballResearchHuman = 
  { brh_forehand:bool; (* @@ makes no sense for smash! remove this *)
    brh_runSpeedBeforeOpening: vec2d;
    brh_runSpeedAfterOpening: vec2d;
    brh_researchKind: researchKindHuman ;
    brh_t1:float;
    brh_t0:float;
    brh_tChange:float;
    brh_ballVelAtImpact:vec3d;
    brh_impact: vec3d;
    brh_footTarget: vec2d;
    brh_modulusOfRunSpeedAtImpactTime:float;
  }

(* Human Player State - Serving After Pressing Button *)
type hpssapb =
  {hpssapb_ToTheRight: bool; 
   hpssapb_AimAngle:float ;
   hpssapb_Timer: float;
   hpssapb_pos: vec2d;
   hpssapb_FirstService: bool

  }

(* Human Player State - Manual Search *)
type hpsms = {hpsms_pos:vec2d;
              hpsms_realizing:realizingState;
              hpsms_askedToSprintInPrevFrame:askData;
              hpsms_diveIsPossibleNow:divePossibility;
              hpsms_diveHasEverBeenPossible:diveHasEverBeenPossible}

(* Automatic Search After Opening *)
type asao = 
  { asao_BallVelAtImpact:vec3d;
    asao_HtOverNet: float;
    asao_Impact: vec3d;
    asao_FootTarget: vec2d;
    asao_ModulusOfRunSpeedAtImpactTime: float;
    asao_CurAim: vec2d;
    asao_UniformMotionData : uniformMotionData;
    asao_TimeToRunFromOpeningToImpact: float;
    asao_RunSpeedFromOpeningToImpact: vec2d;
    asao_Forehand: bool;
    asao_researchKind: researchKindHuman
  }

(* Automatic Search Before Opening *)
type asbo = 
  { 
    asbo_TimeToRunBeforeOpening: float;
    asbo_RunSpeedBeforeOpening: vec2d;

    asbo_BallVelAtImpact:vec3d;
    asbo_HtOverNet: float;
    asbo_Impact: vec3d;
    asbo_FootTarget: vec2d;
    asbo_ModulusOfRunSpeedAtImpactTime: float;
    asbo_CurAim: vec2d;
    asbo_UniformMotionData : uniformMotionData;
    asbo_TimeToRunFromOpeningToImpact: float;
    asbo_RunSpeedFromOpeningToImpact: vec2d;
    asbo_Forehand: bool;
    asbo_researchKind: researchKindHuman
  }

(* The states of the player are mostly related to the animations.
   Some of them are self explanatory, but most aren't, so here it goes:
   .-ServingBeforeLaunch: The player is serving, but hasn't yet tossed the ball
   .-ServingAfterLaunchAndBeforePressingButton: The player has tossed the bal, 
   but hasn't pressed the shoit button yet
   .-ServingAfterPressingButton: The player is making the moves to hit the ball during the serve
   .-ServingAfterHittingBall: The player hit the ball and is now finishing the serve motion
   .-ManualSearch: The player is moving according to the mouse input instructions
   .-AutoSearchBeforeOpening: The player moves automatically towards the place where he will hit the ball;
   this happens after the ManualSearch, and during this time the user is now controlling the aim and spin
   .-AutoSearchAfterOpening: The player executes the shot motion before hitting the ball. After that,
   it moves to the AutoSearchAfterImpactWaitingForAnimToEnd state
   .-AutoSearchAfterImpactWaitingForAnimToEnd: The player has hit the ball and is now finishing the shot motion. After that,
   it moves to the ManualSearch state
   .-DivingFake
   .-GettingUpAfterDive
   .-RealizingWhereTheBallIs: After the opponent hits the ball, the player identifies where it will fall; after that he moves 
   towards it, thus entering the AutoSearchBeforeOpening state.
*)
type humanPlayerState = HPS_ServingBeforeLaunch of bool * vec2d
                      | HPS_ServingAfterLaunchAndBeforePressingButton
                        of bool * float * vec2d
                      | HPS_ServingAfterPressingButton of hpssapb
                      | HPS_ServingAfterHittingBall of vec2d
                      | HPS_ManualSearch of hpsms
                      | HPS_RealizingWhereTheBallIs of uniformMotionData
                      | HPS_AutoSearchBeforeOpening of asbo
                      | HPS_AutoSearchAfterOpening of asao
                      | HPS_AutoSearchAfterImpactWaitingForAnimToEnd of uniformMotionData * bool (*dive *)
                      | HPS_GettingUpAfterDive of vec2d * float (* timer *) * bool (* too late *)
                      | HPS_DivingFake of uniformMotionData * reasonForDiveMiss


type humanPlayer = { hp_objLeading: obj3d; 
                     hp_objSlave: obj3d; 
                     hp_pc: playerCommon;

                     hp_maxParabOpacityGroundShots:float;
                     hp_maxParabOpacityVolleys:float;
                     hp_playsInTopmostCourtHalf: bool;
                     hp_fatigueData : fatigueData;
                     hp_scoreIndex:int;
                     hp_state: humanPlayerState;
                     hp_startHtOverNetForTopSpinGround: float;
                     hp_startHtOverNetForBackSpinGround: float;
                     hp_startHtOverNetForVolleys: float      
                   }

let spinOfResearchKind ~r ~p =
  match r with
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

let curPosOfHumanPlayer p = 
  match p.hp_state with
  | HPS_AutoSearchBeforeOpening x ->
    let u = x.asbo_UniformMotionData in
    vec2dAdd u.umd_startPos (vec2dMulScalar u.umd_timer  u.umd_startVel)
  | HPS_AutoSearchAfterOpening x ->
    let u = x.asao_UniformMotionData in
    vec2dAdd u.umd_startPos (vec2dMulScalar u.umd_timer  u.umd_startVel)
  | HPS_AutoSearchAfterImpactWaitingForAnimToEnd (u, _) ->
    vec2dAdd u.umd_startPos (vec2dMulScalar u.umd_timer  u.umd_startVel)
  | HPS_ManualSearch q-> q.hpsms_pos
  | HPS_ServingBeforeLaunch (_,pos)  ->
    pos
  | HPS_ServingAfterLaunchAndBeforePressingButton (_, _, p) ->
    p
  | HPS_ServingAfterPressingButton x ->
    x.hpssapb_pos
  | HPS_ServingAfterHittingBall  p ->
    p
  | HPS_RealizingWhereTheBallIs  u ->
    vec2dAdd u.umd_startPos (vec2dMulScalar u.umd_timer  u.umd_startVel)
  | HPS_GettingUpAfterDive (pos, _, _) -> pos
  | HPS_DivingFake (u, _) -> vec2dAdd u.umd_startPos (vec2dMulScalar u.umd_timer  u.umd_startVel)

type player = HP of humanPlayer | CP of computerPlayer

let playsInTopmostCourtHalf p = 
  match p with
  | HP h -> h.hp_playsInTopmostCourtHalf
  | CP c -> c.cp_playsInTopmostCourtHalf

let curPosOfPlayer p = 
  match p with 
  | HP h -> curPosOfHumanPlayer h
  | CP c -> curPosOfComputerPlayer c

let resetFatigue pl  = 
  match pl with
  | HP h ->
    HP {h with hp_fatigueData = { h.hp_fatigueData with
                                  fatigueDivisor = 1.0;
                                  fatiguePreviousPos = (curPosOfHumanPlayer h);
                                  fatigueAvailableSprintDistance = maxSprintCm }
       }
  | CP c ->
    CP {c with cp_fatigueData = { c.cp_fatigueData with
                                  fatigueDivisor = 1.0;
                                  fatiguePreviousPos = (curPosOfComputerPlayer c);
                                  fatigueAvailableSprintDistance = maxSprintCm }
       }
