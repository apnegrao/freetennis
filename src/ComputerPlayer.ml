open Math
open SharedData
open PlayerData
open BallMovement
open Animation
open List
open Sound
open Objects3D

(** ---- Consts ---- **)
let offsFromCenterToJustifyKillingDownTheLIne = 180.0 

(* XXX: Warning. Too low values can make the computer choose bad
   trajectories. The reason is that the computer chooses among a small
   number of possible heights over the net. If the impact is very
   close to the net, a small difference in height over the net can
   make a big difference in maximum shot height. So the computer
   produces lob-volleys *)
let minZIcanHitTheBall = 60.0

(** ---- Data Types ---- **)
(* se l'avversario ha lockato, i dati che mi interessano sono il punto
   in cui colpira' e quando.  Se invece non ha lockato, stimero' il
   punto. *)
type lockInfo = HasLocked of vec2d * float | HasNotLocked

type trajData = {
  td_distance:float; td_speedAtBounce:float; td_bounce:vec2d; 
  td_neededPower:float; td_netHt:float; td_maxHt:float;
  td_impact:vec3d
}

type qvd = {qvd_quality: int; qvd_vote:float; qvd_descr: string}

type circle = { c_center:vec2d; c_radius:float}

type impactVote = IV_Zero | IV_NonZero of int * float * volleyOrIntention

type researchJudgement = { rj_rbdts : rbdts; rj_quality: int; rj_vote:float}

(** ---- Functions ---- **)
let createComputerPlayer ~playsInTopmostCourtHalf ~plName ~scoreIndex 
    ~startPos  ~mat ~animdata ~skillLevel =
  let ob = create3dObj ~dirs:animdata  ~initialAnim:(gfxDir ^ "/Asaltello")
  and commonData = createPlayerCommonData ~plName in
  CP
  { cp_name = plName;
    cp_distanceOfBounceFromLine = skillLevel +. 1.0;
    cp_umd = {umd_timer= 0.0;
              umd_startPos = startPos;
              umd_startVel = vec2dCreate 0.0 0.0 };
    cp_obj = ob;
    cp_pointsWonAttacking = [];
    cp_pointsWonStayingBack = [];
    cp_pc = commonData;
    cp_playsInTopmostCourtHalf = playsInTopmostCourtHalf;
    cp_fatigueData =
    { fatigueDivisor = 1.0;
      fatigueStep =
        (match plName with Mats ->0.0001 | Ivan -> 0.00015 |Pete -> 0.0002);
      fatiguePreviousPos = startPos;
      fatigueAvailableSprintDistance= maxSprintCm};
    cp_scoreIndex = scoreIndex;
    cp_state = CPS_WaitingForBallToComeTowardsMe;
  }

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
    (* 	( print_endline ( "shouldAttack: wa = " ^ string_of_int wa ^ ", la  = "
      ^ string_of_int la ^ ", ws = " ^ string_of_int ws ^ ", ls = "
      ^ string_of_int ls ); *)
    wa - la > ws - ls 
    (* 	)  *)
  else
    p.cp_pc.pc_prefersVolleysToGroundShots 

let doStepTowardsCenter ~p ~volleyOrInt ~lockInfo ~dt ~opponentZ ~ball ~prefix = 
  let newAttackChoice = 
    match volleyOrInt with
    | Volley -> Volley
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
    | Volley -> aggressive
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
            let oppLine = { sl_a = 0.0; sl_b = 1.0; sl_c = -. opponentZ}in
            intersectionOfStraightLines oppLine ballLine
          | BS_Still _ -> assert(false)
        )
      | HasLocked (pos, _) -> pos
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
         | StayBack -> dirsign *. (-. courtHt2 -. 180.0)
         | AttackApproach | AttackWithPowerShot -> dirsign *. (-. 100.0) )
      | Volley -> dirsign *. (-. 100.0)
    in
    vec2dCreate (( -. hotLine.sl_b *. optY -. hotLine.sl_c) /. hotLine.sl_a) optY 
  and myPos =  (curPosOfComputerPlayer p) in 
  let youCanStop = 
    let o2 = 
      let animName = prefix ^ "saltello" in
      setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false
    in 
    ( CPS_GetBackToCenterDuringGame (newAttackChoice, optimalPosition, optimalPosition),
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
      let normalSpeed = speedFreeRunNoFatigue
          /. p.cp_fatigueData.fatigueDivisor
      in
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
              None
          in
          let deltaSWithSprint = speedWithSprint *. dt in
          canSprint deltaSWithSprint p.cp_fatigueData
        in
        let finalSp, finalFat = 
          match mayb with
          | None -> (  normalSpeed, p.cp_fatigueData)
          | Some fatig' -> ( speedWithSprint , fatig')
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
         | AttackWithPowerShot | AttackApproach -> trySprint
        )
      | Volley ->
        if abs_float myPos.x2 > courtWt2 *. 0.6 then
          trySprint
        else
          (normalSpeed, p.cp_fatigueData, false)
    in
    let getBackPosition =
      match lockInfo with
      | HasNotLocked  -> optimalPosition
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
            let b_eq = -. 2.0 *. c *. q -. 2.0 *. a *. xc *. q -. 2.0
                      *. zc *. a *. a
            in
            let c_eq = c*.c +. xc *. xc *. a*.a +. 2.0 *. a*.xc *.c +. zc
                      *. zc *. a *. a -. r *. r*. a*. a
            in
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
                | NotVolley _ -> closestToOptimum ~p1 ~p2 ~optimalPosition
                | Volley -> closestToNet ~p1 ~p2
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
              | NotVolley _ -> closestToOptimum ~p1 ~p2 ~optimalPosition
              | Volley -> closestToNet ~p1 ~p2
            else
            if p1.z2 > 200.0 then
              p1
            else if p2.z2 > 200.0 then
              p2
            else
              farthestFromNet p1 p2
          | None ->
            (* se la retta e la circonferenza non si intersecano, non arriveremo
              mai alla retta calda, indipendentemente da in che direzione
              corriamo. In tal caso il punto di rientro e' il piu' vicino alla
              retta calda (in direzione ortogonale).  vedi quadernino *)
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
            let zq = ( d *. c  +. d *. a *. xp +. e *. a *. zp)
                      /. (e *. a -. d *. b) in
            let xq = ( -. c -. b *. zq ) /. a in
            vec2dCreate xq zq
    in 
    let o2 = 
      if sprinting then
        let walkAni = computeWalkAnim ~footTarget:getBackPosition
            ~curPos:myPos ~dirsign:(-. dirsign)
        in
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
    (* let deltaX = distance2d getBackPosition myPos in * XXX: Unused variable *)
      CPS_GetBackToCenterDuringGame ( newAttackChoice, getBackPosition, optimalPosition)
    in
    let umd = {umd_timer = 0.0;
               umd_startVel= vec2dCreate velx velz;
               umd_startPos = myPos}
    in
    ( st, umd, fatigueData', o2)

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
         		   I.e. I lost a point staying back! *)
      p.cp_pointsWonAttacking, Lost::p.cp_pointsWonStayingBack
    else
      Lost::p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack
  else
    p.cp_pointsWonAttacking, Lost::p.cp_pointsWonStayingBack

let judgeImpactPointComp ~cp ~isVolley ~opponentCurPos ~impact
    ~footTarget ~mat  ~isService ~iWantToAttack =
  if impact.y3 <= 0.1 then
    IV_Zero
  else if cp.cp_playsInTopmostCourtHalf && impact.z3 > -. minZIcanHitTheBall then
    IV_Zero
  else if (not cp.cp_playsInTopmostCourtHalf) && impact.z3 < minZIcanHitTheBall then
    IV_Zero
  else
    let iAmForcedToAttack = abs_float impact.z3 < courtHt4 *. 1.2 in
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
      let v = ((AI.voteNotTooMuchBehindGroundLine footTarget ) *. 0.26
              +. (AI.voteHorizontalCentering footTarget) *. 0.15
              +. (AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt)
              *. 0.59)
      in
      IV_NonZero (1, v, NotVolley StayBack)
    else 
      (* the impact is a groundstroke and the opponent is not attacking *)
      let quality = if iWantToAttack  || iAmForcedToAttack then 2 else 1 in
      let ac = 
        let chooseAttackForImpactPoint ~cp  ~impact ~opponent ~mat  =
          let decideZtest = 
            let maybeAttack = 
              let chooseKindOfAttack = 
                let maybMe = pick [Right;Left]
                                  (fun d -> AI.isABitDecentered d footTarget) 
                and maybeHim = pick [Right;Left]
                                    (fun d -> AI.isABitDecentered d opponent) 
                in
                match maybMe with
                | None -> 
                  ( match maybeHim with
                    | None -> AttackApproach  
                    | Some _ -> AttackWithPowerShot)
                | Some d ->
                  ( match maybeHim with
                    | None -> AttackApproach 
                    | Some q ->
                      if q = d then AttackWithPowerShot else AttackApproach )
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
        chooseAttackForImpactPoint  ~impact ~cp ~opponent:opponentCurPos ~mat
      in
      match ac with 
      | StayBack ->
        (* @@ ignoring pc_tendsToAnticipateGroundShots .
 			     Strange. Sometimes, on grass, responding to service, the player 
 			     goes very much backwards. Even if voteClosenessToGroundLine is the 
 			     only parameter. *)
        (* let q = if iWantToAttack then 3 else 1 in *)
        let v = (AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt)
                *. 0.7 +. (AI.voteClosenessToGroundLine footTarget) *. 0.3
        in
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
        let v = AI.voteImpactHtTheHigherTheBetter impact cp.cp_pc.pc_maxSmashHt
                *. 0.6 +. (AI.voteClosenessToNet footTarget) *. 0.4
        in
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
          ~surf
      in
      let maybeFirstPointforResearch = 
        if theBallHasAlreadyCrossedTheNet  then
          Some (projection2d (curBallPos ball))
        else
          let fifty =
            if p.cp_playsInTopmostCourtHalf then -. minZIcanHitTheBall
            else minZIcanHitTheBall
          in
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
            (vec2dMulScalar fac 
              (vec2dSub secondPointForResearch firstPointforResearch)
            )
          ) 
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
            | None -> None
            | Some iaba ->
              let impact3d =
                vec3dCreate impact.x2 iaba.iaba_whatYItWillHave impact.z2
              in
              let deltaT = iaba.iaba_timeFromImpactToArrival -. s.bsm_curTimer in
              let isVolley =
                s.bsm_bouncesSoFar = 0 && not iaba.iaba_itWillBounceFirst
              in
              let t1 = iaba.iaba_timeFromImpactToArrival in
              let t0 = s.bsm_curTimer in
              let speedFreeRun = speedFreeRunNoFatigue
                                 /. p.cp_fatigueData.fatigueDivisor
              in
              let speedNormalResearch = speedNormSearchNoFatigue
                                        /. p.cp_fatigueData.fatigueDivisor
              in
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
              let footPosImpSmash = footPosAtImpactTime ~deltaXFootRacket:0.0
                ~deltaZ:80.0
              in
              let runDirSmash = vec2dSub footPosImpSmash playerCurPos in
              let runAngleSmash = 
                (* NON CAMBIARE! Potresti pensare che se gioca in alto
                   devi usare l'asse Z negato, come fai per
                   l'umano. Invece no!  La cerca male se lo fai. *)
                smallestAngleWithZAxis runDirSmash
              in
              let signXSmash = if runDirSmash.x2 > 0.0 then 1.0 else -. 1.0 in
              let deltaXSmash = distance2d footPosImpSmash playerCurPos in
              assert (deltaXSmash != 0.0);

              let voteAndIntentSmash = 
                judgeImpactPointComp ~cp:p ~isVolley ~opponentCurPos 
                  ~impact:impact3d ~footTarget:footPosImpSmash
                  ~mat:surf.s_material ~isService:s.bsm_lastShotWasAService
                  ~iWantToAttack
              in

              let computeDeltaOpening ~isForehand ~researchKind= 
                let prefix =
                  if p.cp_playsInTopmostCourtHalf then gfxDir ^ "/A"
                  else gfxDir ^ "/B"
                in
                let animName = 
                  match researchKind with
                  | RKC_Smash _ -> prefix ^ "smash"
                  | RKC_StretchForward _ -> prefix
                    ^ if isForehand then "drittoforwardstretch"
                      else "rovescioforwardstretch"
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
              let calcResearch ~researchKind ~tChange ~v2 ~footPosImp ~runAngle
                  ~signX ~deltaX ~volleyOrInten =
                let speedBefOpening = 
                  if v2 < 0.0 then
                    let v1 = deltaX /. (tChange -. t0) in
                    vec2dCreate (signX *. v1 *. sin runAngle) 
                      (v1 *. cos runAngle) (* @@ it is - v1 for humans. investigate. *)
                  else
                    vec2dCreate (signX *. speedFreeRun *. sin runAngle) 
                      (speedFreeRun *. cos runAngle)  (* @@ it is - speedFreeRun for humans*)
                in		
                let speedAftOpening = 
                  if v2 < 0.0 then
                    vec2dCreate 0.0 0.0 
                  else
                    vec2dCreate (signX *. v2 *. sin runAngle) 
                      ( v2 *. cos runAngle (* it is - v2 for humans *) )
                in
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
                  let footPosImpStretch = 
                    footPosAtImpactTime ~deltaXFootRacket:50.0 ~deltaZ:301.0
                  in
                  let runDirStretch = vec2dSub footPosImpStretch playerCurPos in
                  let runAngleStretch = 
                  (* NON CAMBIARE! Potresti pensare che se gioca in alto
   						       devi usare l'asse Z negato, come fai per
   						       l'umano. Invece no!  La cerca male se lo fai. *)
                    smallestAngleWithZAxis runDirStretch
                  in
                  let signXStretch =
                    if runDirStretch.x2 > 0.0 then 1.0 else -. 1.0
                  in
                  let deltaXStretch = distance2d footPosImpStretch playerCurPos in
                  assert (deltaXStretch != 0.0);

                  let voteAndIntentStretch = 
                    judgeImpactPointComp ~cp:p ~isVolley ~opponentCurPos
                      ~impact:impact3d ~footTarget:footPosImpStretch
                      ~mat:surf.s_material
                      ~isService:s.bsm_lastShotWasAService ~iWantToAttack
                  in
                  match voteAndIntentStretch with
                  | IV_Zero -> None
                  | IV_NonZero  (qualStr, voteStr, volleyOrIntStr) ->
                    let deltaOpening =  computeDeltaOpening 
                      ~researchKind:(RKC_StretchForward volleyOrIntStr)
                      ~isForehand
                    in
                    let tChangeStretchShot = t1 -. deltaOpening in
                    assert (tChangeStretchShot <= t1);
                    let v2StretchShot = (deltaXStretch +. speedFreeRun
                      *. (t0-. tChangeStretchShot)) /. (t1 -. tChangeStretchShot)
                    in
                    let canDoStretchShot = 0.0 < impact3d.y3
                      && impact3d.y3 < p.cp_pc.pc_maxShotHt
                      && tChangeStretchShot >= 0.0
                      && deltaT >= deltaOpening
                      && v2StretchShot <= speedNormalResearch
                    in
                    if not canDoStretchShot then
                      None
                    else
                      Some
                      { rj_rbdts = calcResearch
                          ~footPosImp:footPosImpStretch
                          ~researchKind:(RKC_StretchForward volleyOrIntStr)
                          ~v2:v2StretchShot ~tChange:tChangeStretchShot
                          ~signX:signXStretch ~deltaX:deltaXStretch
                          ~runAngle:runAngleStretch
                          ~volleyOrInten:volleyOrIntStr;
                        rj_quality = qualStr;
                        rj_vote = voteStr}
                in
                let footPosImpNormal =
                  footPosAtImpactTime ~deltaXFootRacket:110.0 ~deltaZ:80.0
                in
                let runDirNormal = vec2dSub footPosImpNormal playerCurPos in
                let runAngleNormal = 
                  (* NON CAMBIARE! Potresti pensare che se gioca in alto
       						   devi usare l'asse Z negato, come fai per
      						   l'umano. Invece no!  La cerca male se lo fai. *)
                  smallestAngleWithZAxis runDirNormal
                in
                let signXNormal = if runDirNormal.x2 > 0.0 then 1.0 else -. 1.0 in
                let deltaXNormal = distance2d footPosImpNormal playerCurPos in
                assert (deltaXNormal != 0.0);

                let voteAndIntentNormal = 
                  judgeImpactPointComp ~cp:p ~isVolley ~opponentCurPos
                    ~impact:impact3d ~footTarget:footPosImpNormal
                    ~mat:surf.s_material ~isService:s.bsm_lastShotWasAService
                    ~iWantToAttack
                in
                match voteAndIntentNormal with
                | IV_Zero -> tryStretchSh
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
                    (deltaXNormal +. speedFreeRun *. (t0-. tChangeNormalShot))
                    /. ( t1 -. tChangeNormalShot)
                  in
                  let canDoNormalShot = 
                    0.0 < impact3d.y3 && impact3d.y3 < p.cp_pc.pc_maxShotHt
                    && tChangeNormalShot >= 0.0 && deltaT >= deltaOpening
                    && v2NormalShot <= speedNormalResearch
                  in
                  if not canDoNormalShot then
                    tryStretchSh
                  else
                    Some
                    { rj_rbdts = calcResearch
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
                    ~researchKind:(RKC_Smash volleyOrIntSm) ~isForehand
                in
                let tChangeSmash = t1 -. deltaOpeningSmash in
                assert (tChangeSmash <= t1);
                let v2Smash = (deltaXSmash +. speedFreeRun 
                              *. (t0 -. tChangeSmash)) /. ( t1 -. tChangeSmash)
                in
                let canDoSmash = p.cp_pc.pc_minSmashHt < impact3d.y3
                  && impact3d.y3 < p.cp_pc.pc_maxSmashHt && tChangeSmash >= 0.0
                  && deltaT >= deltaOpeningSmash && v2Smash <= speedNormalResearch
                in
                if canDoSmash then
                  Some
                  { rj_rbdts = calcResearch ~researchKind:(RKC_Smash volleyOrIntSm)
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
      ~opponentCurPos ~ball ~ballPos ~ballDir ~bsm ~surf
  in
  let researches'  = select_some researches in
  let better x1 x2 =
    if x1.rj_quality < x2.rj_quality then x1
    else if x2.rj_quality  < x1.rj_quality then x2
    else if x1.rj_vote > x2.rj_vote then x1 else x2
  in

  let smashVolleys = 
    let isSmashVolley res =
      match res.rj_rbdts.rbdts_researchKind with
      | RKC_Smash voi -> 
        ( match voi with
          | Volley -> true
          | NotVolley _ -> false)
      | RKC_Normal _ | RKC_StretchForward _ -> false
    in
    List.filter isSmashVolley researches'
  in
  match smashVolleys with
  | [] ->
    let tryNormalShot = 
      let normalShots = 
        let isNormalShot res =
          match res.rj_rbdts.rbdts_researchKind with
          | RKC_Normal _ -> true
          | RKC_StretchForward _ | RKC_Smash _ -> false
        in
        List.filter isNormalShot researches'
      in
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
        | l -> Some (  (findBestElement l better).rj_rbdts))
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

let calcTrajData ~tr ~iAmOpening ~opp ~oppDir ~availablePowerForMyShot = 
  let opp = 
    (* distance the opponent will presumably cover while I am doing the gesture *)
    let foreseenMovement = 228.0 in
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
  | Some _ -> None
  | None ->
    let neededPower = length3d tr.startVel in
    if availablePowerForMyShot < neededPower then
      None
    else
      let distance = 
        (* @@ wrong. If the ball is a dropshot, the calculation is wrong. We
        should take the second bounce into account *)
        let opponentImpact = 
          let ballLine = straightLineFromPointAndDir (projection2d tr.impact)
                                                    (projection2d tr.startVel)
          in
          intersectionOfStraightLines oppDir ballLine
        in
        distance2d opp opponentImpact
      in
      let speedAtBounce = 
        let t = whenWillItBounce tr in
        let vx = tr.startVel.x3 -. tr.spin.x3 *. t in
        let vz = tr.startVel.z3 -. tr.spin.z3 *. t in
        length2d (vec2dCreate vx vz )
      in
      let bounce = whereWillItBounce tr in
      let deltaNet =
        let may = whenWillTheTrajArriveAtZ ~z:0.0 ~t:tr in
        match may with
        | None -> assert false
        | Some iata ->
          let ballYOverNet = tr.impact.y3 +. tr.startVel.y3 *. iata.iata_t
                +. 0.5 *. ( -. tr.spin.y3 -. abs_float g)
                *. iata.iata_t *. iata.iata_t
          in
          let netHtThere = netHtAtX(iata.iata_x) in
          abs_float (netHtThere -. ballYOverNet)
      in
      let maxHt = 
        let tmaxht =
          let whenWillReachMaximumHt ~y0 ~v0y ~sy = v0y /. ( sy +. abs_float g) in
          whenWillReachMaximumHt ~y0:tr.impact.y3 ~v0y:tr.startVel.y3 ~sy:tr.spin.y3
        in
        let tmaxht = if tmaxht < 0.0 then 0.0 else tmaxht in
        tr.impact.y3 +. tr.startVel.y3 *. tmaxht +. 0.5
          *. ( -. tr.spin.y3 -. abs_float g) *. tmaxht *. tmaxht
      in
      Some
      { td_maxHt = maxHt;
        td_netHt = deltaNet;
        td_impact = tr.impact;
        td_bounce = bounce;
        td_speedAtBounce = speedAtBounce;
        td_distance = distance;
        td_neededPower = neededPower}


let judgeTrajectory ~p ~td   ~surf ~iAmOpening ~researchKind
    ~canDoTopSpin ~opp ~myPosAtImpact =
  let truncateBetween0And10 v = 
    if v > 10.0 then 10.0 else if v < 0.0 then 0.0 else v
  in
  let voteDistance = truncateBetween0And10(
      AI.interpolateVote td.td_distance 200.0 courtWt 0.0 10.0)
  in
  let voteSpeed = truncateBetween0And10(
      AI.interpolateVote td.td_speedAtBounce 0.0 4500.0 0.0 10.0)
  in
  let voteSloth = 10.0 -. voteSpeed in
  let voteLen = truncateBetween0And10(
      AI.interpolateVote (abs_float td.td_bounce.z2) 0.0 courtHt2 0.0 10.0)
  in
  let voteNetHt = 
    truncateBetween0And10(AI.interpolateVote td.td_netHt 0.0 60.0 10.0 0.0 )
  in
  let voteMaxHt =  truncateBetween0And10(
      AI.interpolateVote td.td_maxHt 0.0 (td.td_impact.y3 +. 200.0) 10.0 0.0) 
  in
  match researchKind with
  | RKC_Smash _ ->
    { qvd_quality = 1; 
      qvd_vote = voteSpeed *. 0.8 +. voteDistance *. 0.2;
      qvd_descr = "RKC_Smash" }
  | RKC_StretchForward Volley | RKC_Normal VOTI_Volee ->
    let quality = if abs_float td.td_bounce.z2 <= courtHt4 then 2 else 1 in
    if AI.isInNoMansLand opp then 
    { qvd_quality = quality; 
      qvd_vote = voteMaxHt *. 0.5 +. voteLen *. 0.3 +.  voteDistance *. 0.2;
      qvd_descr = "I am doing a volley, opponentIsInNoMansLand" }
    else if abs_float myPosAtImpact.z2 > courtHt4 *. 0.68 then
    { qvd_quality = quality; 
      qvd_vote = voteMaxHt *. 0.5 +. voteLen *. 0.3 +.  voteDistance *. 0.2;
      qvd_descr = "I am doing a volley, but I am far from the net" }
    else
    { qvd_quality = quality;
      qvd_vote = voteMaxHt *. 0.6 +.  voteDistance *. 0.4;
      qvd_descr = "I am doing a volley, I am close to the net . maxHt = "
          ^ string_of_float td.td_maxHt ^ ",voteMaxHt = "
          ^ string_of_float voteMaxHt ^ ", impactht = "
          ^ string_of_float td.td_impact.y3 ^ ", speedAtBounce = "
          ^ string_of_float td.td_speedAtBounce ^ ", impact.z = "
          ^ string_of_float td.td_impact.z3
    }
  | RKC_StretchForward NotVolley intent | RKC_Normal VOTI_NotVolee (_, intent) ->
    if AI.isAttacking opp then
      if td.td_distance < 290.0 || td.td_speedAtBounce < 2500.0 then
      { qvd_quality =  2;
        qvd_vote = voteNetHt *. 0.07 +. voteSpeed *. 0.91 +. voteDistance *. 0.02;
        qvd_descr = "passingShot, low distance "}
      else
        (* voteNetHt is dangerous here: the player risks to ignore a 
           			 passingshot that travels outside, preferring diagonal *)
      { qvd_quality = 1;
        qvd_vote = voteSpeed *. 0.95 +. voteDistance *. 0.05 ;
        qvd_descr = "passingShot, distance good. speedatbounce = "
          ^ string_of_float td.td_speedAtBounce
      }
    else (* both players are back *)
    ( match intent with
      | StayBack ->
        let mayb = pick [Left;Right] 
                        (fun d -> AI.isQuiteDecentered d myPosAtImpact)
        in
        ( match mayb with
          | None -> (* I am centered *)
            if AI.isVeryDecenteredBackwards opp then
              if AI.isABitDecentered Left opp then
                let qual = if td.td_bounce.x2 <= 200.0 then 2 else 1 in
                { qvd_quality = qual;
                  qvd_vote = voteDistance *. 0.5 +. voteSpeed *. 0.5;
                  qvd_descr = "debugPlace 25"}
              else if AI.isABitDecentered Right opp then
                let q = if td.td_bounce.x2 >= -. 200.0 then 2 else 1 in
                { qvd_quality = q;
                  qvd_vote = voteDistance *. 0.5 +. voteSpeed *. 0.5;
                  qvd_descr = "debugPlace 27"}
              else
              { qvd_quality = 1;
                qvd_vote = voteDistance *. 0.4 +. voteSpeed *. 0.6;
                qvd_descr = "opponent is backwards but centered"}
            else if AI.isInNoMansLand opp then
              { qvd_quality = 1;
                qvd_vote = voteDistance *. 0.3 +. voteSpeed *. 0.7;
                qvd_descr = "opponentIsInNoMansLand: distance and speed"}
            else
             (* both players are centered *)
              let isClay = 
                match surf with Clay -> true | Cement | Grass -> false
              in
              if not isClay && td.td_distance > courtWt2
                && td.td_speedAtBounce > 2900.0 (*check whether it is > @@ *)
                && (not p.cp_pc.pc_prefersVolleysToGroundShots)
              then
              { qvd_quality = 1;
                qvd_vote = voteDistance *. 0.4 +. voteSpeed *. 0.6;
                qvd_descr = "both centered, debugPlace 30"}
              else
              ( match p.cp_name with
                | Pete ->
                  let q = if td.td_distance <= 210.0 then 2 else 1 in
                  { qvd_quality = q;
                    qvd_vote = voteDistance *. 0.1 +. voteSpeed *. 0.9 ;
                    qvd_descr = "pete interlocutory shot"}
                | Ivan ->
                  let q = if td.td_distance <= 210.0 then 2 else 1 in
                  { qvd_quality = q;
                    qvd_vote =( match surf with 
                         | Clay -> voteDistance *. 0.05 +. voteSpeed *. 0.95
                         | Grass| Cement ->  
                           voteDistance *. 0.3 +. voteSpeed *. 0.7);
                    qvd_descr = "ivan normal interlocutory shot"}
                | Mats ->
                  let q =
                    if abs_float td.td_bounce.z2 <= courtHt4 *. 1.5 then 2 else 1
                  in
                  (* no voteLen, because it clashed with distance *)
                  { qvd_quality = q;
                    qvd_vote = voteDistance *. 0.5 +. voteSpeed *. 0.5;
                    qvd_descr = "both are centered, mats interlocutory shot"}
              )
          | Some dir -> (* I am decentered in direction dir *)
            let lrSign = match dir with Left -> 1.0 | Right -> -. 1.0 in
            if AI.isABitDecentered dir opp || (AI.isVeryDecenteredBackwards opp
              && not (AI.isABitDecentered (oppositeDir dir) opp))
            then
              (* diagonale a chiudere *)
              let q = 
                if lrSign *. td.td_bounce.x2 -. 200.0 < 0.0 then 2 else 1
              in
              (* e' diag *)
              { qvd_quality = q;
                qvd_vote = voteDistance *. 0.7 +. voteSpeed *. 0.3;
                qvd_descr = "I am decentered left, opponent decentered
                             left or back: power diagonal "}
            else if AI.isQuiteDecentered  (oppositeDir dir) opp then
              (* downTheLine a chiudere *)
              let q =
                if lrSign *. td.td_bounce.x2 +. 200.0 > 0.0  then 2 else 1
              in
              { qvd_quality = q;
                qvd_vote = voteDistance *. 0.7 +. voteSpeed *. 0.3;
                qvd_descr = "i am decentered left, opponent quite
                             decentered right: downTheLine "}
            else
              (* I am decentered and the opponent centered *)
              let doLongLowDiag = 
                (* You may think not too diagonal, because I give a lot of
                angle. This is wrong, because I am decentered in "dir", so I
                give angle to aim where I already am *)
                let isDiagonal = lrSign *. td.td_bounce.x2 -. 202.0 > 0.0 in
                if not isDiagonal then
                { qvd_quality = 3;
                  qvd_vote = 1.0 ;
                  qvd_descr = ">>>>>WARNING: Not diagonal should
                              not have been chosen<<<<"}
                else
                  (* This is difficult to balance. it must surely be long, in
                order not to give angle. It must be low, for the same reason.
   				  	  but long and slow tends to produce fast balls. And this is bad.
   				  	  It should not be too fast, to give me time to get back to center.
                But not too slow either: otherwise the opponent plays a killing
                volee. So, how slow? Found by trials: the lowest speed such that
                the player did not hit a volee.*)
                  if td.td_speedAtBounce < 1800.0 then
                  { qvd_quality = 2;
                    qvd_vote = td.td_speedAtBounce;
                    qvd_descr = ">>>WARNING: too slow, should not
                              have been chosen <<<<"}
                  else if td.td_speedAtBounce > 3500.0 then
                  { qvd_quality = 2;
                    qvd_vote = td.td_speedAtBounce;
                    qvd_descr = ">>>WARNING: too fast, should not have
                              been chosen <<<<"}
                  else
                    let fores = 
                      if iAmOpening then
                        " - foreseen opponent x = " ^ string_of_float opp.x2
                      else
                        " - final opponent x = " ^ string_of_float opp.x2
                    in
                    { qvd_quality = 1;
                      qvd_vote = voteLen *. 0.8 +. voteMaxHt *. 0.2;
                      qvd_descr = "iAmDecentered, opponent is centered. Do long,
                                low diagonal. speedAtBounce = "
                                ^ string_of_float td.td_speedAtBounce ^ fores}
              in
              let killingShotOrSlowDiag = 
                let tryTheKillingDownTheLine  (* @@ move outside! *)=
                  let iAmCloseToTheSideLine = 
                    lrSign *. myPosAtImpact.x2 +. courtWt2 +. 50.0 > 0.0
                  in
                  canDoTopSpin &&
                  lrSign *. opp.x2 -. offsFromCenterToJustifyKillingDownTheLIne > 0.0 && 
                  iAmCloseToTheSideLine
                in
                if tryTheKillingDownTheLine then
                  let isKilling = lrSign *. td.td_bounce.x2 < 0.0
                     && (abs_float (abs_float td.td_bounce.x2 -. courtWt2)) < 150.0
                  in
                  if not isKilling then
                  { qvd_quality = 2;
                    qvd_vote = 1.0;
                    qvd_descr = ">>>WARNING: not isKilling, should not have
                                been chosen <<<"}
                  else
                    let str =
                      if iAmOpening then
                         " foreseen opponent x = " ^ string_of_float opp.x2
                      else
                         " opponent  x = " ^ string_of_float opp.x2
                    in
                    { qvd_quality = 1;
                      qvd_vote = voteLen *. 0.15 +. voteDistance *. 0.05
                                  +. voteSpeed *. 0.8;
                      qvd_descr = "iAmDecenteredLeft,  not opponentIsDecenteredLeft,
                                  killingshot." ^ str}
                else
                  doLongLowDiag
              in
              ( match p.cp_name with
                | Mats -> killingShotOrSlowDiag
                | Ivan -> killingShotOrSlowDiag
                | Pete -> killingShotOrSlowDiag
              )
        )
      | AttackApproach ->
        (* slow is better, but not slower than a certain value *)
        let quality =
          if td.td_speedAtBounce <= 1400.0 (* 2200.0 originally *) then 2 else 1
        in
        { qvd_quality = quality;
          qvd_vote = voteDistance *. 0.1 +. voteLen *. 0.25 +. voteSloth *. 0.4
                      +. voteNetHt *. 0.25;
          qvd_descr = "AttackApproach. speedAtBounce = "
                      ^ string_of_float td.td_speedAtBounce}
      | AttackWithPowerShot ->
        { qvd_quality = 1;
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
        let htOfIndex i = lowestHt +. i *. 8.0 
        and indices = List.map (fun x -> float_of_int x ) (listFromTo 0 10 ) in
        List.map htOfIndex indices
      in
      let possibleHeightsHi = 
        let htOfIndex i = lowestHt +. 80.0  +. i *. 25.0 
        and indices = List.map (fun x -> float_of_int x ) (listFromTo 0 8 ) in
        List.map htOfIndex indices
      in
      allPairs possibleSpins (append possibleHeightsLow possibleHeightsHi)
    in

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
      else l
    in
    allPairs (maybeFlipY bouncePoints)  pairsSpinAndHt
  in (* end triples *)

  let trajs =
    select_some
    ( List.map (fun (bounc, (spin, ht)) -> 
        buildTrajFromTwoPointsAndHeight ~impact ~htOverNet:ht ~spin
          ~bounceDesired:bounc ~targetRect) 
      triples)
  in

  let availablePowerForMyShot = 
    let po = 
      match researchKind with
      | RKC_Normal VOTI_Volee -> 
        p.cp_pc.pc_maxShotPower *. powerAttenuationForVolee
      | RKC_Normal VOTI_NotVolee _  | RKC_Smash _ -> p.cp_pc.pc_maxShotPower 
      | RKC_StretchForward _ ->
        p.cp_pc.pc_maxShotPower *. powerAttenuationForStretchForwardAndDive
    in
    po  +. (abs_float ballVelAtImpact.z3) *. p.cp_pc.pc_exploitationOfOpponentsPower 
  in
  let maybeTrajDatas = 
    List.map
      (fun tr -> calcTrajData ~tr ~iAmOpening ~opp:opponentPos
          ~oppDir:opponentDir ~availablePowerForMyShot  ) 
      trajs
  in

  let trajsWithVote = 
    let trajAndVoteOfPair (tr, td) = 
      ( match td with
        | None -> None
        | Some td' ->
          let qvd = 
            judgeTrajectory ~p ~td:td' ~opp:opponentPos ~iAmOpening
              ~canDoTopSpin:canIDoTopSpin ~surf ~myPosAtImpact ~researchKind
          in
          Some ( qvd , tr) ) 
    and pairs = List.combine trajs maybeTrajDatas in
    select_some (List.map trajAndVoteOfPair pairs) in

  if length trajsWithVote = 0 then 
    None
  else
    let (qvdBest, trBest) = 
      let better (qvd1, x1) (qvd2, x2) = 
        if qvd1.qvd_quality < qvd2.qvd_quality then (qvd1, x1)
        else if qvd1.qvd_quality > qvd2.qvd_quality then (qvd2, x2)
        else if qvd1.qvd_vote > qvd2.qvd_vote then qvd1, x1
        else qvd2, x2
      in
      (findBestElement trajsWithVote better)
    in
    Some ( qvdBest.qvd_descr  , trBest)


let updateComputerPlayer  ~p ~dt ~b ~opponentCurPos ~surf ~opponentLock ~mouse
    ~sounds ~opt ~aidebug ~nextServiceIsFirst = 
  let prefix = 
    if p.cp_playsInTopmostCourtHalf then gfxDir ^ "/A" else gfxDir ^ "/B"
  in
  let dirsign = if p.cp_playsInTopmostCourtHalf then -. 1.0 else 1.0 in
  let p = 
    let newObj = updateAnim p.cp_obj dt in
    { p with
      cp_obj = newObj;
      cp_umd = {p.cp_umd with umd_timer = p.cp_umd.umd_timer +. dt}
    }
  in
  let startWalkingTowards ~getBackPoint ~vel = 
    let my2dpos = curPosOfComputerPlayer p in
    let getBackDir = vec2dSub getBackPoint my2dpos in
    if getBackDir.x2 = 0.0 && getBackDir.z2 = 0.0 then
    { umd_timer = 0.0;
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
          { impact = vec3dCreate (curPosOfComputerPlayer p).x2 171.0
                                  (dirsign*.(courtHt2 +. 20.0));
            startVel = vec3dCreate 0.0 750.0 0.0;
            spin = vec3dNull;
            targetRect = None}
          in
          (print_endline "ball launched by computer";
           createRunningBall ~traj
             ~scoreIndexOfLastPlayerWhoHit:b.b_siolpwhtb
             ~polyBall:b.b_polygon ~service:true
             ~polyRedCross:b.b_redCrossPolygon ~polyShadow:b.b_shadowPolygon)
        else
          b
      in
      (st2, p.cp_umd, p.cp_obj, b2, p.cp_fatigueData, 
        p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
    | CPS_ServingAfterLaunchAndBeforeStartingGesture ( right , timer )->
      let st2, o2 = 
        let timer2 = timer +. dt in
        (* @@ ugly hack, this should be calculated dynamically given the anim *)
        let delta =  if p.cp_playsInTopmostCourtHalf then 0.2 else 0.87 in
        if timer2 > delta then
          ((CPS_ServingAfterStartingGesture (right, 0.0) ), 
           {p.cp_obj with o3d_animState = Animated 0.0;
                          o3d_curFrameIdx = p.cp_obj.o3d_curFrameIdx +1})
        else
          CPS_ServingAfterLaunchAndBeforeStartingGesture (right, timer2), p.cp_obj
      in
      (st2, p.cp_umd, o2, b, p.cp_fatigueData, p.cp_pointsWonAttacking,
        p.cp_pointsWonStayingBack)
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
                    let dest = vec2dCreate (-. dirsign *. offsX) 
                        (-. dirsign *. (courtHt4 +. 50.0))
                    in
                    vec2dSub dest  src
                  in
                  if p.cp_playsInTopmostCourtHalf then
                    -. (smallestAngleWithZAxis dir)
                  else
                    -. (smallestAngleWithNegativeZAxis dir)
                else
                  let dir = 
                    let src = curPosOfComputerPlayer p in
                    let dest = vec2dCreate ( dirsign *. offsX) 
                        (-. dirsign *. (courtHt4 +. 50.0))
                    in
                    vec2dSub dest  src
                  in
                  if p.cp_playsInTopmostCourtHalf then
                    smallestAngleWithZAxis dir
                  else
                    smallestAngleWithNegativeZAxis dir
              in

              let angX = degToRad p.cp_pc.pc_firstServiceXAngleInDeg  in
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
                    upperLeftServiceBox
                  else
                    upperRightServiceBox
                else if right then
                  lowerLeftServiceBox
                else
                  lowerRightServiceBox
              in
              let heights =
                List.map (fun x -> float_of_int x) (listFromTo 270 310)
              in
              let goodHeights = 
                let shotIsGood h = 
                  let tr =
                  { impact = { (curBallPos b) with y3 = h };
                    startVel = vec3dCreate velx (-. vely) velz;
                    spin = vec3dCreate ( spi *. dirsign *. (sin ang)) 
                            spi
                            (-. spi *. (cos ang) *. dirsign);
                    targetRect = Some rett }
                  in
                  let notHitNet = 
                    let may = whenWillTheTrajectoryHitTheNet tr in
                    match may with None -> true | _ -> false
                  in
                  theTrajectoryFallsInTheTargetRect tr && notHitNet
                in
                Array.of_list (List.filter (fun h -> shotIsGood h ) heights)
              in
              { impact = 
                { (curBallPos b) with
                    y3 = goodHeights.( (Array.length goodHeights )/2) };
                startVel = vec3dCreate velx (-. vely) velz;
                spin = vec3dCreate
                    ( spi *. dirsign *. (sin ang)) 
                    spi
                    (-. spi *. (cos ang) *. dirsign);
                targetRect = Some rett
              } 
            in
            playSoundId ~sounds ~id:SoundNormalShot;
            print_endline ("Service impact height: "
                            ^ string_of_float traj.impact.y3 );
            createRunningBall ~traj ~scoreIndexOfLastPlayerWhoHit:p.cp_scoreIndex
              ~polyBall:b.b_polygon ~polyRedCross:b.b_redCrossPolygon
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
                vec2dCreate x y
              in
              { umd_startPos = newPos;
                umd_startVel = vec2dCreate 0.0 0.0;
                umd_timer = 0.0 }
            in
            { p with cp_umd = uu }
          in
          doStepTowardsCenter ~p ~volleyOrInt:voint ~lockInfo:opponentLock ~prefix
            ~dt ~opponentZ:opponentCurPos.z2 ~ball:b
        else
          CPS_ServingAfterHittingBall, p.cp_umd, p.cp_fatigueData, p.cp_obj
      in 
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
        | BS_Still _ -> CPS_WaitingForBallToComeTowardsMe, p.cp_umd, p.cp_obj
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
                deltaX /. speedFreeRunNoFatigue
              in
              CPS_GetBackToCenterAtPointFinished stopTime
            and o2 = 
              let animName = prefix ^ "attesa" in
              setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false 
            and u = 
              startWalkingTowards ~getBackPoint ~vel:speedFreeRunNoFatigue
            in
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
          (* XXX: Unused function
          let ballGoesOutOrOntoNet bsm = 
            let ballIsAboutToHitNet = 
              let maybeT = whenWillTheTrajectoryHitTheNet bsm.bsm_trajectory in
              match maybeT with
              | None -> false
              | Some _ -> (bsm.bsm_bouncesSoFar = 0) 
            and ballWillGoOut = 
              bsm.bsm_bouncesSoFar = 0 &&
              (match bsm.bsm_trajectory.targetRect with None -> false | _ -> true)
              && not (theTrajectoryFallsInTheTargetRect bsm.bsm_trajectory) in
            (not bsm.bsm_isItGoodSoFar) ||  ballIsAboutToHitNet || ballWillGoOut
          *)
          let my2dpos = curPosOfComputerPlayer p in
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
                        (projection2d (curBallVel bsm))
                    in
                    intersectionOfStraightLines horizLine ballDir
                  and p2 = (* @@ crashes if already done second bounce! *)
                    whereWillTheBallMakeSecondBounce ~b ~bsm ~surf
                  in
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
                        let walkAni = computeWalkAnim ~footTarget ~curPos:my2dpos
                                                      ~dirsign
                        in
                        prefix ^ walkAni
                      in
                      setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false
                    in
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
              umd_startPos = my2dpos }
            in
            ( CPS_GetBackToCenterAtPointFinished stopTime, umd, o2, pwa, pws)
          in
          match b.b_state with
          | BS_Still _ -> assert false
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
                (match bsm.bsm_trajectory.targetRect with None -> false | _ -> true)
                && not (theTrajectoryFallsInTheTargetRect bsm.bsm_trajectory)
              in
              (not bsm.bsm_isItGoodSoFar) ||  ballIsAboutToHitNet || ballWillGoOut
            in
            if ballGoesOutOrOntoNet bsm then
              let pwa, pws = 
                if bsm.bsm_lastShotWasAService || not bsm.bsm_isItGoodSoFar then
                  (p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
                else
                (
                (*print_endline ("ballGoesOutOrOntoNet bsm. computer adds a point"); *)
                  updateMemoryOfPointsWonAndLost ~p ~won:true ~opponentCurPos)
              in
              backAtPointFinished ~pwa ~pws ~b ~bsm
            else
              let res = chooseBestResearch ~playerCurPos:my2dpos ~opponentCurPos
                  ~ballPos:(projection2d (curBallPos b) ) 
                  ~ballDir:(projection2d (curBallVel bsm) ) ~ball:b ~surf ~bsm ~p
              in
              match res with
              | None ->
                let pwa, pws = updateMemoryOfPointsWonAndLost ~p
                    ~won:false ~opponentCurPos
                in
                backAtPointFinished ~pwa ~pws ~b ~bsm
              | Some s ->
                let walkAni = 
                  computeWalkAnim ~footTarget:s.rbdts_footTarget
                    ~curPos:my2dpos ~dirsign
                in
                let o2 = 
                  let animName = prefix ^ walkAni in
                  setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:false
                in
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
        (*let destRect = if p.cp_playsInTopmostCourtHalf then
          lowerHalfOfCourt else upperHalfOfCourt in * XXX: Unused variable *)
        let st2 = 
          let tr = 
            let targetRect = (Some (if p.cp_playsInTopmostCourtHalf then
                                      lowerHalfOfCourt else upperHalfOfCourt))
            in
            chooseTrajectory ~p ~impact:rbdts.rbdts_impact
              ~opponentPos:opponentCurPos ~aidebug
              ~ballVelAtImpact:rbdts.rbdts_ballVelAtImpact
              ~canIDoTopSpin:true ~canIDoBackSpin:true
              ~myPosAtImpact:rbdts.rbdts_footTarget
              ~surf:surf.s_material ~targetRect ~iAmOpening:true
              ~researchKind:rbdts.rbdts_researchKind
          in
          match tr with
          | None -> 
         (* @@ happened on computer pete smashing close to net ( z = -. 60.0 )*)
            ( print_endline ("failure finding a traj. impact = "
                            ^ string_of_float rbdts.rbdts_impact.x3 ^ ", "
                            ^ string_of_float rbdts.rbdts_impact.y3 ^ ", "
                            ^ string_of_float rbdts.rbdts_impact.z3);
              assert(false) )
          | Some (descr, tt) ->
            print_endline ("1: " ^descr);
            let researchWithUpdatedSpin = 
              match rbdts.rbdts_researchKind with
              | RKC_StretchForward v -> RKC_StretchForward v
              | RKC_Smash v -> RKC_Smash v
              | RKC_Normal VOTI_Volee -> RKC_Normal VOTI_Volee
              | RKC_Normal VOTI_NotVolee (_, at) ->
                let sp = if tt.spin.y3 >= 0.0 then Topspin else Backspin in
                RKC_Normal (VOTI_NotVolee (sp, at))
            in
            { radts_Trajectory = tt;
              radts_Forehand = rbdts.rbdts_forehand;
              radts_TimeToRunFromOpeningToImpact = 
                  rbdts.rbdts_timeToRunFromOpeningToImpact;
              radts_RunSpeedFromOpeningToImpact =
                  rbdts.rbdts_runSpeedFromOpeningToImpact;
              radts_FootTarget= rbdts.rbdts_footTarget;
              radts_Impact = rbdts.rbdts_impact;
              radts_researchKind = researchWithUpdatedSpin;
              radts_BallVelAtImpact = rbdts.rbdts_ballVelAtImpact}
        in
        let my2dpos = curPosOfComputerPlayer p in
        let u2 =
        { umd_timer = 0.0;
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
            | RKC_Smash _ -> prefix ^ "smash"
            | RKC_StretchForward _ -> prefix
              ^ if rbdts.rbdts_forehand then "drittoforwardstretch"
                else "rovescioforwardstretch"
          in
          setAnim ~o:p.cp_obj ~animName ~restartIfSameAnimation:true
        in
        ( CPS_ResearchAfterDecidingTheShot st2, u2, o2, b, p.cp_fatigueData,
          p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
    | CPS_ResearchAfterDecidingTheShot r ->
      if p.cp_umd.umd_timer <= r.radts_TimeToRunFromOpeningToImpact then
        (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData,
         p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
      else
        (* let destRect = if p.cp_playsInTopmostCourtHalf then
            lowerHalfOfCourt else upperHalfOfCourt in * XXX: Unused variable *)
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
          CPS_TheAnimationIsTerminating volleyOrIntention
        in
        let my2dpos = curPosOfComputerPlayer p in
        let u2 =
        { umd_timer = 0.0;
          umd_startVel = r.radts_RunSpeedFromOpeningToImpact; (* the same as now*)
          umd_startPos = my2dpos}
        in
        let b2 =
          let mayb = 
            let canIDoTopSpin = r.radts_Trajectory.spin.y3 >= 0.0 in
            chooseTrajectory ~p ~impact:r.radts_Impact
              ~opponentPos:opponentCurPos ~myPosAtImpact:r.radts_FootTarget
              ~ballVelAtImpact:r.radts_BallVelAtImpact ~iAmOpening:false
              ~canIDoTopSpin ~canIDoBackSpin:(not canIDoTopSpin) ~aidebug
              ~surf:surf.s_material ~targetRect:r.radts_Trajectory.targetRect
              ~researchKind:r.radts_researchKind
          in
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
            ~dt ~opponentZ:opponentCurPos.z2 ~ball:b ~prefix
        in
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
           umd_startVel = vec2dCreate 0.0 0.0}
        in
        let ballIsComingTowardsMe  = 
          match b.b_state with
          | BS_Still _ -> false
          | BS_Moving bsm ->
            if p.cp_playsInTopmostCourtHalf then
              bsm.bsm_trajectory.startVel.z3 < 0.0
            else
              bsm.bsm_trajectory.startVel.z3 > 0.0
        in
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
        let stop = setAnim  ~o:p.cp_obj ~animName:(prefix ^ "saltello")
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
      (st, umd, o2, b, p.cp_fatigueData,  p.cp_pointsWonAttacking, 
        p.cp_pointsWonStayingBack)
    | CPS_WaitingForANewPointToBegin -> 
      (p.cp_state, p.cp_umd, p.cp_obj, b, p.cp_fatigueData,
        p.cp_pointsWonAttacking, p.cp_pointsWonStayingBack)
  in
  let newPlayer = {p with cp_state = newState;
                          cp_umd = newUmd;
                          cp_obj = newObj;
                          cp_fatigueData = newFatigueData;
                          cp_pointsWonAttacking = pwa;
                          cp_pointsWonStayingBack = pws
                  }
  in
  let newPlayer =
  { newPlayer with
    cp_fatigueData = (updateFatigue newPlayer.cp_fatigueData (curPosOfComputerPlayer newPlayer))
  }
  in
  ( newBall, CP newPlayer, mouse, [] )

