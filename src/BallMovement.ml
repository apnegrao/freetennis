open Math
open SharedData
open Objects3D
open Sound

(** ----- Data Types ----- **)
type trajectory = {
  impact:vec3d; 
  startVel:vec3d;
  spin:vec3d;
  targetRect:rectangleTheShotIsMeantToFallIn option
}

type hitNetResult = {hnr_y: float; hnr_t:float; hnr_x:float}

type infoAboutTrajArrival = {iata_x:float; iata_t:float}

type infoAboutBallArrival = {
  iaba_timeFromImpactToArrival:float;
  iaba_itWillBounceFirst:bool;
  iaba_whatYItWillHave:float;
  iaba_ballVelWhenItArrives:vec3d
}

(* Ball State Moving *)
type bsm = {
  bsm_trajectory: trajectory;
  bsm_isItGoodSoFar : bool;
  bsm_lastShotWasAService:bool;
  bsm_curTimer:float;
  bsm_whenWillItBounce:float;
  bsm_whenWillHitTheNet:hitNetResult option;
  bsm_bouncesSoFar:int
}

type ballState = BS_Still of vec3d | BS_Moving of bsm

type ball = {
  b_state:ballState;
  b_polygon: polygon;
  b_redCrossPolygon: polygon;
  b_shadowPolygon: polygon;
  b_siolpwhtb: (* scoreIndexOfLastPlayerWhoHitTheBall *) int
}

(* --- Trajectory --- *)
(* equazioni quaderno 

   Can return negative time!

   FIXME: Bug trovato. Non devo tornare il tempo massimo, ma il minimo
   positivo! Perche' non e' una vera parabola (c'e' lo spin).
*)
let whenWillTheTrajArriveAtZ ~z ~t =
  let a = -. 0.5 *. t.spin.z3 in
  let b = t.startVel.z3 in
  let c = t.impact.z3  -. z in
  let may = solve2ndDegreeEquat a b c in
  match may with
  | None -> None
  | Some ( t1, t2) -> 
    let mi = min t1 t2 in
    let ma = max t1 t2 in
    let tArr = if mi > 0.0 then mi else ma in
    Some
    { iata_t = tArr;
      iata_x = t.impact.x3 +. t.startVel.x3 *. tArr -. 0.5
                *. t.spin.x3 *. tArr *. tArr}

(* controllata corretta.

   usa le equazioni della relazione Inform Grafica: 

   yn = y0 + vy tn + 0.5 ( -s - |G| ) t2 ; 

   0 = q0 + vq tn  - 0.5 s tn2

*)	    
let buildTrajFromTwoPointsAndHeight ~impact ~htOverNet ~spin ~bounceDesired  ~targetRect = 
  assert ( htOverNet > 0.0);
  assert ( bounceDesired.z2 *. impact.z3 < 0.0);
  assert ( spin >= 0.0 || abs_float spin < abs_float g);
  (* flippo se gioca nella parte alta, cosi' che assumo che sto
     giocando nella parte bassa *)
  let iFlipped = impact.z3 < bounceDesired.z2 in
  let flipxz3 a = vec3dCreate  (-. a.x3) a.y3  (  -. a.z3) in
  let impact = if iFlipped then flipxz3 impact else impact in
  let bounceDesired = 
    if iFlipped then flipxz2 bounceDesired else bounceDesired
  in
  let impact2d = projection2d impact in
  let shotDir = straightLineBetween impact2d bounceDesired in
  let horizLine = {sl_a = 0.0 ; sl_b = 1.0; sl_c = 0.0} in
  let pointOverNet = intersectionOfStraightLines horizLine shotDir in
  (* assumendo q0 positivo e qr negativo, il mio algo funziona solo
     se sto nella parte bassa. Sono percio' obbligato a flippare.*)
  let q0 = distance2d impact2d pointOverNet  in
  let qr = -. distance2d pointOverNet bounceDesired in
  let y0 = impact.y3 in
  let yn = htOverNet in
  (*let qr2 = qr *. qr in * XXX: Unused variable *)
  (*let q02 = q0 *. q0 in * XXX: Unused variable *)
  let g = abs_float g in
  let s2 = spin *. 0.5 in
  let s1 = (spin +. g )*. 0.5 in
  let q1 = q0 -. qr in
  let y1 = y0 -. yn in
  let denom = ( -. s2 *. y0 +. s1  *. q1 ) 
              *. ( s1 *. q1 -. s1 *. q0 +. s2 *. y1 -. s2 *. y0 )
  in
  assert (denom != 0.0);
  let discr = -. ( ( -. s2 *. y1  +. s1 *. q0 )
                  *. ( -. y0 *. q0 +. y1 *. q1 ) ) /. denom
  in
  if discr  < 0.0 then
    None
  else
    let tn = sqrt discr in
    assert  (tn != 0.0) ; 
    let vq = ( s2 *. tn *. tn -. q0 ) /. tn in
    let vy = ( s1 *. tn *. tn -. y1 ) /. tn in
(* il contrario significherebbe che la palla va prima indietro e poi avanti lungo q *)
    if vq >= 0.0  then 
      None
    else
      let shotDir = vec2dSub bounceDesired impact2d in
      let beta' = 
        if shotDir.z2 > 0.0 then smallestAngleWithZAxis shotDir
        else smallestAngleWithNegativeZAxis shotDir
      in
      let beta = if shotDir.z2 *. shotDir.x2 > 0.0 then -. beta' else beta' in
      let vx = -. vq *. sin beta in
      let vz = vq *. cos beta in
      assert (vz != 0.0);
      let spinY = spin in
      let spinX = -. spin *. sin beta in
      let spinZ =  spin *. cos beta in
      assert ( spinZ *. spinY >= 0.0);
      (* it can be 0 if the ball is flat! e.g. volee *)
      (* spiegazione del precedente assert: sto facendo i calcoli ipotizzando di
      essere nella parte bassa del campo (z positivo). Ora, se la palla rallenta
      lungo z (spinZ >0), allora si deve anche alzare lungo y (spinY>0). Se
      invece accelera lungo z (spinZ<0), allora si deve anche abbassare lungo y
      (spinY < 0). Quindi in tutti i casi i segni devono essere concordi. *)
      let vx = if iFlipped then -. vx else vx in
      let vz = if iFlipped then -. vz else vz in
      let spinX = if iFlipped then -. spinX else spinX in
      let spinZ = if iFlipped then -. spinZ else spinZ in
      let impact = if iFlipped then flipxz3 impact else impact in
      Some {impact = impact ; startVel = vec3dCreate vx vy vz;
            spin = vec3dCreate spinX spinY spinZ ;
            targetRect = targetRect}

(* usa le equazioni della relazione pdf a pag  19, o quaderno "calcoli 2005" *)
let whenWillItBounce t = 
  let a = 0.5 *. (  -. t.spin.y3 -. abs_float g  ) in
  let b = t.startVel.y3 in
  let c = t.impact.y3 in
  let may  = solve2ndDegreeEquat a b c  in
  match may with
  | None -> assert false
  | Some (t1, t2) -> ( assert (max t1 t2 > 0.0); max t1 t2 )

(* corretta con equazioni quaderno *)
let whereWillItBounce t = 
  let tim = whenWillItBounce t in
  vec2dCreate
    (t.impact.x3 +. t.startVel.x3 *. tim -. tim *. tim *. 0.5 *. t.spin.x3 ) 
    (t.impact.z3 +. t.startVel.z3 *. tim -. tim *. tim *. 0.5 *. t.spin.z3 )


let theTrajectoryFallsInTheTargetRect tr =
  match tr.targetRect with
  | None -> assert(false)
  | Some rect ->
    let b = whereWillItBounce tr in
    let zmin = min rect.rtsimtfi_top rect.rtsimtfi_bottom in
    let zmax = max rect.rtsimtfi_top rect.rtsimtfi_bottom in
    let goodLeft = rect.rtsimtfi_left <= b.x2 in
    let goodRight = b.x2 <= rect.rtsimtfi_right in
    let goodZMin = b.z2 >= zmin in
    let goodZMax = b.z2 <= zmax in
    goodLeft && goodRight && goodZMax && goodZMin

(* equazioni quaderno *)
let whenWillTheTrajectoryHitTheNet t = 
  let may = whenWillTheTrajArriveAtZ ~z:0.0 ~t in
  match may with
  | None -> None
  | Some iata ->
    if iata.iata_t < 0.0 then
      None
    else
      let yn = t.impact.y3 +. t.startVel.y3 *. iata.iata_t +. 0.5
               *. (-. t.spin.y3 -. abs_float g) *. iata.iata_t *. iata.iata_t
      in
      if (-. 5.0) <= yn && yn <= (netHtAtX iata.iata_x) then
        Some {hnr_y = yn; hnr_x = iata.iata_x; hnr_t = iata.iata_t}
      else
        None

(** ----- Ball ----- **)
(* equazioni quaderno *)
let curBallVel bsm = 
  vec3dCreate 
    (bsm.bsm_trajectory.startVel.x3 -. bsm.bsm_curTimer *.
      bsm.bsm_trajectory.spin.x3)
    (bsm.bsm_trajectory.startVel.y3 +. bsm.bsm_curTimer *.
      (-. bsm.bsm_trajectory.spin.y3 -. abs_float g))
    (bsm.bsm_trajectory.startVel.z3 -. bsm.bsm_curTimer *.
      bsm.bsm_trajectory.spin.z3)

let whereWillTheBallMakeSecondBounce ~b ~bsm ~surf = 
  assert(bsm.bsm_bouncesSoFar <= 1);
  let whereBou = whereWillItBounce bsm.bsm_trajectory in
  if bsm.bsm_bouncesSoFar = 1 then
    whereBou
  else
    let i = bsm.bsm_trajectory.impact in
    let v = bsm.bsm_trajectory.startVel in
    let sp = bsm.bsm_trajectory.spin in
    let t = bsm.bsm_whenWillItBounce in
    let newTraj = 
    { impact = 
        vec3dCreate (i.x3 +. v.x3 *. t -. 0.5 *. sp.x3 *. t *. t )
          0.0
          (i.z3 +. v.z3 *. t -. 0.5 *. sp.z3 *. t *. t );
      startVel = 
        vec3dCreate ( (v.x3 -. t *. sp.x3)/. surf.s_velXZAttenuationFactor) 
          (-.( (v.y3 +. t*. (-. sp.y3 -. abs_float g) )) /.
            surf.s_velYAttenuationFactor)
          ( (v.z3 -. t *. sp.z3)/. surf.s_velXZAttenuationFactor) ;
      spin = 
        vec3dCreate (sp.x3 /. surf.s_spinAttenuationFactor) 
          (sp.y3 /. surf.s_spinAttenuationFactor) 
          (sp.z3 /. surf.s_spinAttenuationFactor) ;
      targetRect = bsm.bsm_trajectory.targetRect
    }
    in
    whereWillItBounce newTraj

(* equazioni quaderno *)
let curBallPos b = 
  match b.b_state with
  | BS_Still p -> p
  | BS_Moving m ->
    let i = m.bsm_trajectory.impact in
    let v = m.bsm_trajectory.startVel in
    let s = m.bsm_trajectory.spin in
    let t = m.bsm_curTimer in
    vec3dCreate (i.x3 +. v.x3 *. t -. 0.5 *. s.x3 *. t *. t)
      (i.y3 +. v.y3 *. t +. 0.5 *. (-. s.y3 -. abs_float g) *. t *. t)
      (i.z3 +. v.z3 *. t -. 0.5 *. s.z3 *. t *. t)

(* the difference with whenWillTheTrajArriveAtZ is that this takes the
   bounce into account. Also, it never returns a negative time.

   equazioni quaderno *)
let whenWillTheBallArriveAtZ  ~z ~s ~surf =
  let spin = s.bsm_trajectory.spin in
  let startVel = s.bsm_trajectory.startVel in
  let i = s.bsm_trajectory.impact in
  let may = whenWillTheTrajArriveAtZ ~z ~t:s.bsm_trajectory in
  match may with
  | None -> None (* @@ happened on very short dropshot *)
  | Some iata ->
      (*if iata.iata_t < 0.0 then
		      ( assert(false);
          None)
      else* XXX: Old code *)
      assert(iata.iata_t >= 0.0);
      let tArrive = iata.iata_t in
      let ballHtAtTArrive = i.y3 +. startVel.y3 *. tArrive +. 0.5 *.
                            (-. spin.y3 -. abs_float g) *. tArrive *. tArrive
      in
      if ballHtAtTArrive >= 0.0 then
        (*let speedAtImpactTime = vec3d  startVel.z3 -. tArrive *. spin.z3 in *)
        Some
        { iaba_timeFromImpactToArrival = tArrive;
          iaba_itWillBounceFirst = false;
          iaba_whatYItWillHave = ballHtAtTArrive;
          iaba_ballVelWhenItArrives = 
              vec3dSub startVel (vec3dMulScalar tArrive spin)
        }
      else
      if s.bsm_bouncesSoFar > 0 then
        None
      else
        let t = s.bsm_whenWillItBounce in
        let newTraj =
        { 
          impact =
          {
            x3 = i.x3 +. startVel.x3 *. t -. 0.5 *. spin.x3 *. t *. t;
            y3 = 0.0;
            z3 = i.z3 +. startVel.z3 *. t -. 0.5 *. spin.z3 *. t *. t
          };
          startVel = {
            x3 = (startVel.x3  -. t *. spin.x3) /. surf.s_velXZAttenuationFactor;
            y3 = -.( (startVel.y3 +. t*. (-. spin.y3 -. abs_float g) )) /.
                 surf.s_velYAttenuationFactor;
            z3 = (startVel.z3  -. t *. spin.z3) /. surf.s_velXZAttenuationFactor;
          };
          spin = {
            x3 = spin.x3 /. surf.s_spinAttenuationFactor;
            y3 = spin.y3 /. surf.s_spinAttenuationFactor;
            z3 = spin.z3 /. surf.s_spinAttenuationFactor
          };
          targetRect = None
        }
        in
        assert(newTraj.startVel.y3 > 0.0);
        let may = whenWillTheTrajArriveAtZ ~z ~t:newTraj in
        match may with 
        | None -> None (* happened for shot that bounces backwards! *)
        | Some iata ->
          if iata.iata_t < 0.0 then
            assert(false)
          else
            let tArrive2 = iata.iata_t in
            assert(tArrive2 > 0.0);
            let yTarg = 0.0 +. newTraj.startVel.y3 *. tArrive2 +. 0.5 *.
                    (-. newTraj.spin.y3 -. abs_float g) *. tArrive2 *. tArrive2
            in
            if yTarg <= 0.0 then
              None
            else
              Some
              { iaba_timeFromImpactToArrival =
                  s.bsm_whenWillItBounce +. tArrive2;
                iaba_itWillBounceFirst = true;
                iaba_whatYItWillHave = yTarg;
                iaba_ballVelWhenItArrives = 
                  vec3dSub newTraj.startVel
                            (vec3dMulScalar tArrive2  newTraj.spin )
              }

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
    b_siolpwhtb = scoreIndexOfLastPlayerWhoHit
  }

(*
Let's see what this method actually does.
b:	The ball (type ball)
dt:	Seconds since last frame (see calcDt)
score:	The score (type score)
surf:	Surface
sounds:	The sounds to be played during the game (type sounds)
nextServiceIsFirst:	A boolean indicating if the next service is a first service
opt:	I'm not sure what this is and I think it isn't used. By the name I'd guess
  this is the 'options' data type
players:	The players array
@returns:	A tuple (score, ball, nextServiceIsFirst, players)
*)
let updateBall ~b ~dt ~score ~surf ~sounds ~nextServiceIsFirst ~opt 
              ~players ~letComputerKnowHeWon = 
  match b.b_state with
  | BS_Still _ -> score, b, nextServiceIsFirst, players
  | BS_Moving m ->
    let newTimer = dt +. m.bsm_curTimer in
    let b = {b with b_state = BS_Moving {m with bsm_curTimer = newTimer}} in
    let incScore p s  = 
      assert (p = 0 || p = 1);
      let opponent x = 1-x in
      let incPair p arr = 
        if p = 0 then [| arr.(0) +1 ; arr.(1) |] else [| arr.(0) ; arr.(1)+1 |]
      in
      match s.sc_state with
      | TieBreak points -> 
        let newPoints = incPair p points in
        if newPoints.(p) >=7 && newPoints.(p) > newPoints.(opponent p) +1 then
          let newSets = (incPair p    [|6; 6 |]   )::s.sc_finishedSets in
          { sc_state = NoTieBreak {
              points= [|0;0|]; games=[|0;0|]
            };
            sc_finishedSets = newSets
          }
        else
          { s with sc_state =  TieBreak newPoints}
      | NoTieBreak n ->
        let rec normalize p = 
          if p.(0) > 3 && p.(1) > 3 then normalize [| p.(0) -1 ;  p.(1) -1 |]
          else p
        in
        let newPoints = normalize (incPair p n.points) in
        if newPoints.(p) > 3 && newPoints.(p) > newPoints.(opponent p)+1 then
          let newGames = incPair p n.games in
          if newGames.(p) >= 6 && newGames.(p) > newGames.(opponent p) + 1 then
            { sc_state = NoTieBreak {
                points= [|0;0|]; games=[|0;0|]
              };
              sc_finishedSets = newGames::s.sc_finishedSets
            }
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
              | None -> score, false, true, players
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
                  (*print_endline ("bounceOnGround: inform computer he has won");*)
                  Array.map (fun p -> letComputerKnowHeWon ~p
                                ~siolpwhtb:b.b_siolpwhtb ~players) players
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
            { impact = vec3dCreate whereBounce.x2 0.0 whereBounce.z2;
              startVel = 
                vec3dCreate (curV.x3 /. surf.s_velXZAttenuationFactor)
                  (-.(curV.y3 /. surf.s_velYAttenuationFactor) )
                  (curV.z3 /. surf.s_velXZAttenuationFactor);
              spin =
                vec3dCreate (s.bsm_trajectory.spin.x3 /. surf.s_spinAttenuationFactor)
                  (s.bsm_trajectory.spin.y3 /. surf.s_spinAttenuationFactor)
                  (s.bsm_trajectory.spin.z3 /. surf.s_spinAttenuationFactor);
              targetRect = None 
            }
          in

          let newState =   
            if newTraj.startVel.y3 < 120.0 then
              BS_Still newTraj.impact
            else
              let bo = whenWillItBounce newTraj in
              BS_Moving
              { bsm_trajectory = newTraj;
                bsm_isItGoodSoFar = isGoodSoFar;
                bsm_curTimer = 0.0;
                bsm_whenWillItBounce = bo;
                bsm_lastShotWasAService = m.bsm_lastShotWasAService;
                bsm_whenWillHitTheNet = whenWillTheTrajectoryHitTheNet newTraj;
                bsm_bouncesSoFar = newBounceCount
              }
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
              (*print_endline ("bounceAgainstNetOrWall: inform computer he has won");*)
              Array.map (fun p -> letComputerKnowHeWon ~p
                            ~siolpwhtb:b.b_siolpwhtb ~players) players
            in
            s', true, pl'
          else
            score, nextServiceIsFirst, players
        in
        let curV = curBallVel s in
        let newVel =
          vec3dCreate (curV.x3 /. 8.0) (curV.y3 /. 4.0) (-. curV.z3 /. 8.0)
        in
        let curP = curBallPos b in
        let newImpact = 
          if newVel.z3 > 0.0 then vec3dCreate curP.x3 curP.y3 (z +. 2.0)
          else vec3dCreate curP.x3 curP.y3 (z -. 2.0)
        in
        let newTraj = 
        { impact = newImpact;
          startVel = newVel;
          spin = vec3dNull ;
          targetRect = None
        }
        in
        let s' =
        { bsm_bouncesSoFar = s.bsm_bouncesSoFar +1 ;
          bsm_isItGoodSoFar = false;
          bsm_trajectory = newTraj;
          bsm_lastShotWasAService = s.bsm_lastShotWasAService;
          bsm_curTimer = 0.0;
          bsm_whenWillItBounce = whenWillItBounce newTraj;
          bsm_whenWillHitTheNet = whenWillTheTrajectoryHitTheNet newTraj
        }
        in
        let b' = { b with  b_state= BS_Moving s' } in
        (b', score', nextServiceIsFirst, newPlayers)
      in
      let b, score, nextServiceIsFirst, newPlayers0 = 
        match m.bsm_whenWillHitTheNet with
        | None -> b, score, nextServiceIsFirst, newPlayers0
        | Some hnr ->
          if newTimer > hnr.hnr_t then
            (playSoundId ~sounds ~id:SoundHitNet;
             bounceAgainstNetOrWall ~b ~z:0.0 ~score ~s:m ~nextServiceIsFirst
                                    ~players:newPlayers0)
          else
            b, score, nextServiceIsFirst, newPlayers0
      in
      match b.b_state with 
      | BS_Still _ -> score, b, nextServiceIsFirst, newPlayers0
      | BS_Moving m ->
        let curP = curBallPos b in
        let b, score , nextServiceIsFirst, newPlayers0 = 
          if curP.z3 < upperBound then
            (playSoundId ~sounds ~id:SoundHitBorder;
             bounceAgainstNetOrWall ~b ~z:upperBound ~score ~s:m
                                    ~nextServiceIsFirst ~players:newPlayers0)
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
               bounceAgainstNetOrWall ~b ~z:lowerBound ~score ~s:m
                                      ~nextServiceIsFirst ~players:newPlayers0)
            else
              b, score, nextServiceIsFirst, newPlayers0
          in
          (score, b, nextServiceIsFirst, newPlayers0)
