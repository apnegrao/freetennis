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
    ~pausedWithKey  ~noOneIsServing  ~windowHt ~windowWt ~handleOfTexture ~s =
  let plBelow =
    if playsInTopmostCourtHalf  players.(0) then players.(1) else players.(0) in
  let factor = float_of_int windowHt /. 480.0 in
  let d = 40.0 *.  factor
  and h = 25.0 *. factor
  and d2 = (22.0 *. 22.0 /. 26.0) *. factor
  and h2 = 22.0 *. factor in
  (** Render score *)
  (*I'm pretty convinced that these renderNumber,renderString09 and 
    		renderNumber09 can be merged...*)
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

    match s.sc_state with
    | TieBreak points -> 
      (let w = whoServes s in
       let tieStr =  string_of_int points.(w) ^ " " ^ string_of_int points.(1-w) in
       let destX =  float_of_int windowWt -. d2 *. float_of_int (String.length tieStr) in
       renderString09 tieStr destX 0.0 ;
       renderString09 "6 6" 0.0 0.0
       );
    | NoTieBreak n ->

      (let w = whoServes s in
       renderNumber n.points.(w)     (float_of_int windowWt -. d *. 2.4)     0.0;
       renderNumber n.points.(1-w)   (float_of_int windowWt -. d *. 1.0)     0.0;
       let scoreStr = 
         (string_of_int (n.games.(0)) ^ " " ^ string_of_int (n.games.(1))) in
       renderString09 scoreStr 0.0 0.0 
      );

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
    let yOffs = 50.0 in
    renderTexture 0.0 yOffs 249.0   (30.0 +. yOffs)
    (* renderTexture 0.0 0.0 249.0   46.0 *)
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
  (Array.iter maybeRenderSprint players;


   GlMat.mode `projection;
   GlMat.load_identity ();
   GluMat.perspective ~fovy:fovY ~aspect:(float_of_int windowWt /. float_of_int windowHt) ~z:(zNear,  20000.0);
   GlMat.mode `modelview;)

(** Renders 3d stuff, I guess**)
let render ~players ~ball ~aidebug ~serverData ~camData 
    ~handleOfTexture ~realisticParabolaOpacity () =
  (* FIXME: This function is duplicated in render2dStuff *)
  let plBelow =
    if playsInTopmostCourtHalf players.(0) then players.(1) else players.(0) in
  let plAbove =
    if playsInTopmostCourtHalf  players.(0) then players.(0) else players.(1) in

  let xrepeat = 20.0 
  and yrepeat = 4.0 in
  let v1 = [ vertexCreate (-. distanceFromPolesToExternalBorder -. courtWt2)
               netHtBorder 0.0 0.0 0.0;
             vertexCreate 0.0 netHtCenter 0.0 xrepeat 0.0;
             vertexCreate 0.0 0.0 0.0 xrepeat yrepeat;
             vertexCreate (-.distanceFromPolesToExternalBorder -. courtWt2) 0.0 0.0 0.0 yrepeat ]
  in
  let offx = 40.0 in
  let v1s = [ vertexCreate ((-. distanceFromPolesToExternalBorder -. courtWt2) +. offx)
                0.1   (2.0 *. netHtBorder) 0.0 0.0;
              vertexCreate offx 0.1 (2.0 *. netHtCenter) xrepeat 0.0;
              vertexCreate 0.0 0.1 0.0 xrepeat yrepeat;
              vertexCreate (-.distanceFromPolesToExternalBorder -. courtWt2) 0.1 0.0 0.0 yrepeat ] 
  in
  let polyNet1 = { polyVerts = v1 ;
                   polyTextureHandle = (StringMap.find  (gfxDir ^ "/rete.bmp.png") handleOfTexture);
                   polyColor = {r = 1.0; g = 1.0; b=1.0;a= 0.6};
                   polyVisible = true} 
  in
  let polyNet1Shad = {polyNet1 with
                      polyColor = {r = 0.0; g = 0.0; b=0.0;a= shadowIntensity };
                      polyVerts = v1s}
  in
  let v2 = [ vertexCreate 0.0 netHtCenter 0.0 0.0 0.0;
             vertexCreate (distanceFromPolesToExternalBorder +. courtWt2)
               netHtBorder 0.0 xrepeat 0.0;
             vertexCreate (distanceFromPolesToExternalBorder +. courtWt2)
               0.0 0.0 xrepeat 5.0;
             vertexCreate 0.0 0.0 0.0 0.0 5.0] 
  and v2s = [ vertexCreate offx 0.1 (2.0 *. netHtCenter)   0.0 0.0;
              vertexCreate (offx +. (distanceFromPolesToExternalBorder +. courtWt2)) 0.1 (2.0 *. netHtBorder) xrepeat 0.0;
              vertexCreate (distanceFromPolesToExternalBorder +. courtWt2) 0.1 0.0 xrepeat yrepeat;
              vertexCreate 0.0 0.1 0.0 0.0 yrepeat]
  in
  let polyNet2 = { polyNet1 with polyVerts =v2 } in
  let polyNet2Shad = {polyNet2 with
                      polyColor = {r = 0.0; g = 0.0; b=0.0;a= shadowIntensity };
                      polyVerts = v2s}
  in

  GlDraw.line_width 2.0;
  Gl.disable `texture_2d;
  Gl.disable `depth_test;
  GlDraw.begins `line_strip;
  GlDraw.color ~alpha:0.64 (0.95, 0.95, 0.95);
  GlDraw.vertex3 ( -. courtWt2, 0.0, -. courtHt2 ) ;
  GlDraw.vertex3 ( courtWt2, 0.0, -. courtHt2 ) ;
  GlDraw.vertex3 ( courtWt2, 0.0, courtHt2 ) ;
  GlDraw.vertex3 ( -. courtWt2, 0.0, courtHt2 ) ;
  GlDraw.vertex3 ( -. courtWt2, 0.0, -. courtHt2 ) ;
  GlDraw.vertex3 ( -. (courtWt2 +. corridorWt), 0.0, -. courtHt2 ) ;
  GlDraw.vertex3 ( -. (courtWt2 +. corridorWt), 0.0, courtHt2 ) ;
  GlDraw.vertex3 ( -. courtWt2 , 0.0, courtHt2 ) ;
  GlDraw.ends ();


  GlDraw.begins `line_strip;
  GlDraw.vertex3 ( courtWt2, 0.0, -. courtHt2 ) ;
  GlDraw.vertex3 ( courtWt2 +. corridorWt, 0.0, -. courtHt2 ) ;
  GlDraw.vertex3 ( courtWt2 +. corridorWt, 0.0, courtHt2 ) ;
  GlDraw.vertex3 ( courtWt2 , 0.0, courtHt2 ) ;

  GlDraw.ends ();


  GlDraw.begins `lines;
  GlDraw.vertex3 ( -. courtWt2, 0.0, courtHt4 ) ;
  GlDraw.vertex3 ( courtWt2, 0.0, courtHt4 ) ;
  GlDraw.vertex3 ( -. courtWt2, 0.0, -. courtHt4 ) ;
  GlDraw.vertex3 (  courtWt2, 0.0, -. courtHt4 ) ;


  GlDraw.vertex3 (  0.0, 0.0,  courtHt4 ) ;
  GlDraw.vertex3 (  0.0, 0.0,  -. courtHt4 ) ;


  GlDraw.vertex3 (  0.0, 0.0,  courtHt2 ) ;
  GlDraw.vertex3 (  0.0, 0.0,  courtHt2 -. 25.0 ) ;

  GlDraw.vertex3 (  0.0, 0.0,  -. courtHt2 ) ;
  GlDraw.vertex3 (  0.0, 0.0,  -. courtHt2 +. 25.0 ) ;

  GlDraw.ends ();


  begin
    match ball.b_state with
    | BS_Still _ -> ()
    | BS_Moving bsm ->
      match bsm.bsm_whenWillHitTheNet with
      | None ->
        let t = bsm.bsm_trajectory in

        let bounceT = bsm.bsm_whenWillItBounce in
        let bounceX = t.impact.x3 +. t.startVel.x3 *. bounceT -. 0.5 *. t.spin.x3 *. bounceT *. bounceT in
        let bounceZ = t.impact.z3 +. t.startVel.z3 *. bounceT -. 0.5 *. t.spin.z3 *. bounceT *. bounceT in
        renderPolygon ball.b_redCrossPolygon (Some (vec3dCreate bounceX 1.0 bounceZ))
      | _ -> ()
  end;

  let p = curBallPos ball in
  renderPolygon ball.b_shadowPolygon (Some (vec3dCreate p.x3 1.0 p.z3 ));

  if aidebug then
    ( match players.(1) with
      | CP c ->
        (match c.cp_state with
         | CPS_GetBackToCenterDuringGame (_, targetPos, optPos) ->
           (renderPolygon ball.b_shadowPolygon
              (Some (vec3dCreate targetPos.x2 0.5 targetPos.z2));
            renderPolygon ball.b_shadowPolygon
              (Some (vec3dCreate optPos.x2 0.5 optPos.z2)))
         | _ -> ())
      | HP _ -> ())
  else
    ();


  let objOf p = (* @@ va bene solo epr il plAbove *)
    if playsInTopmostCourtHalf p then
      match p with
      | HP h -> 
        (match serverData with
         | Client _ ->
           h.hp_objLeading 
         | Server _ ->
           h.hp_objSlave  
         | NeitherServerNorClient ->
           h.hp_objLeading)
      | CP c -> 
        c.cp_obj
    else
      match p with
      | HP h -> 
        (match serverData with
         | Client _ ->
           h.hp_objSlave 
         | Server _ ->
           h.hp_objLeading  
         | _ ->
           h.hp_objLeading)
      | CP c -> 
        c.cp_obj

  in
  let thePlayerMustBeTransparentNow p = 
    match p with
    | HP h ->
      (match h.hp_state with
       | HPS_ServingBeforeLaunch _ -> false
       | HPS_ServingAfterLaunchAndBeforePressingButton _ -> false
       | HPS_ServingAfterPressingButton _ -> false
       | HPS_GettingUpAfterDive _ -> false
       | HPS_DivingFake _ -> false
       | _ -> true)
    | CP _ -> false
  in
  let thePlayerMustBeGreenNow p = 
    match p with
    | HP h ->
      (match h.hp_state with
       | HPS_ManualSearch m ->
         m.hpsms_diveIsPossibleNow = DivePossible
       |  _ -> false)
    | CP _ -> false
  in

  let thePlayerMustBeRedNow p = 
    match p with
    | HP h ->
      (match h.hp_state with
       | HPS_ManualSearch m ->
         m.hpsms_diveIsPossibleNow = DiveNotPossible 
       (* && not (match m.hpsms_askedToSprintInPrevFrame with  HasAskedAndObtained _ -> true | _ -> false) *)
       |  _ -> false)
    | CP _ -> false
  in

  let theHeadCoversTheNet p =
    let verticalVec = vec3dCreate 0.0 1.0 0.0 in
    let eye = vec3dCreate camData.eyeX camData.eyeY camData.eyeZ in
    let angHead =
      let headVec =
        let head =
          let po = curPosOfPlayer p in
          vec3dCreate po.x2 180.0 po.z2 in
        vec3dSub head eye in
      smallestAngleBetween3d verticalVec headVec in
    let angCenter = 
      let centerVec = 
        let center = vec3dCreate 0.0 netHtCenter 0.0 in
        vec3dSub center eye in
      smallestAngleBetween3d verticalVec centerVec in
    angHead < angCenter
  in

  let playerToRenderFirst, playerToRenderLast = 
    match serverData with
    | Client _ -> plBelow, plAbove
    | NeitherServerNorClient | Server _ -> plAbove, plBelow
  in


  let color = 
    let alpha = 
      match serverData with
      | Client _ ->
        if theHeadCoversTheNet playerToRenderFirst && 
           thePlayerMustBeTransparentNow playerToRenderFirst 
        then
          0.5
        else 1.0

      | Server _ | NeitherServerNorClient -> 1.0
    in
    if thePlayerMustBeGreenNow playerToRenderFirst then
      {r = 1.0; g = 1.0; b = 0.4; a = alpha}
    else if thePlayerMustBeRedNow playerToRenderFirst then
      {r = 1.0; g = 0.6; b = 0.5; a = alpha}
    else
      {r = 1.0; g = 1.0; b = 1.0; a = alpha}
  in

  let po = curPosOfPlayer playerToRenderFirst in
  let x = po.x2 in
  let z = po.z2 in


  renderObj3d ~o:(objOf playerToRenderFirst) ~handleOfTexture ~pos:(Some (vec3dCreate x 0.0 z))
    ~flipX:(match serverData with | Client _ -> true | NeitherServerNorClient | Server _  -> false) ~color;


  renderPolygon ball.b_polygon (Some (curBallPos ball));


  (* draw net *)

  GlDraw.line_width 3.0 ;
  Gl.disable `texture_2d;
  GlDraw.begins `line_strip;
  GlDraw.color ~alpha:1.0 (0.82, 0.82, 0.82);
  GlDraw.vertex3 ( -. distanceFromPolesToExternalBorder -. courtWt2 , netHtBorder, 0.5 ) ;
  GlDraw.vertex3 ( 0.0 , netHtCenter, 0.5 ) ;
  GlDraw.vertex3 ( distanceFromPolesToExternalBorder +. courtWt2 , netHtBorder, 0.5 ) ;
  GlDraw.ends ();

  GlDraw.begins `line_strip;
  GlDraw.vertex3 ( 0.0 , netHtCenter, 0.5 ) ;
  GlDraw.vertex3 ( 0.0 , 0.0, 0.5 ) ;
  GlDraw.ends ();




  renderPolygon polyNet1 None;
  renderPolygon polyNet2 None;
  renderPolygon polyNet1Shad None;
  renderPolygon polyNet2Shad None;


  (* net poles *)	

  Gl.disable `texture_2d;
  Gl.enable `line_smooth;
  Gl.disable `depth_test;

  GlDraw.line_width 7.0 ;
  GlDraw.color ~alpha:1.0 (0.3, 0.4, 0.3);
  GlDraw.begins `line_strip;
  let poleX = -. distanceFromPolesToExternalBorder -. courtWt2 in
  GlDraw.vertex3 (  poleX, netHtBorder +. 2.0, 0.0 ) ;
  GlDraw.vertex3 ( poleX, 0.0, 0.0 ); 
  GlDraw.ends ();

  GlDraw.begins `line_strip;
  GlDraw.vertex3 (  -. poleX, netHtBorder +. 2.0, 0.0 ) ;
  GlDraw.vertex3 ( -. poleX, 0.0, 0.0 ) ;
  GlDraw.ends ();


  (* draw parabola *)


  let renderPar i =

    let pl = players.(i) in
    match pl with
    | CP _ -> ()
    | HP hp ->
      let renderParabola  ~t ~opacity =

        Gl.disable `texture_2d;
        Gl.enable `line_smooth;
        Gl.enable `point_smooth;
        GlDraw.line_width 1.5 ;


        (* draw the vertical line showing the height of the impact *)
        GlDraw.begins `line_strip;
        GlDraw.color ~alpha:(max 0.13 (min 1.0 (2.3 *. opacity))) (1.0, 1.0, 1.0);
        GlDraw.vertex3 ( t.impact.x3 , 0.0, t.impact.z3 ) ;
        GlDraw.vertex3 ( t.impact.x3 , t.impact.y3, t.impact.z3 ) ;
        GlDraw.ends ();


        (* draw the first part of the parabola, up to the net *)
        GlDraw.begins `line_strip;
        GlDraw.color ~alpha:opacity (1.0, 1.0, 0.0);
        let rec vertsBeforeNet startT  count =
          if count = 0 then
            ( print_endline "Failed printing first part of parabola. Skipping.";
              [])
          else
            let vertOfTime ti =
              let ti2 = ti *. ti in
              vec3dCreate
                (t.impact.x3 +. t.startVel.x3 *. ti  -. 0.5 *. ti2 *. t.spin.x3)
                (t.impact.y3 +. t.startVel.y3 *. ti  +. 0.5 *. ti2 *. (-. t.spin.y3 -. abs_float g))
                (t.impact.z3 +. t.startVel.z3 *. ti  -. 0.5 *. ti2 *. t.spin.z3) in
            let v = vertOfTime startT in
            if v.z3 *. t.impact.z3 < 0.0 then
              []
            else
              v::(vertsBeforeNet (startT +. 0.01)) (count -1) in
        List.iter (fun v -> GlDraw.vertex3 (v.x3, v.y3, v.z3)) (vertsBeforeNet 0.0 200);
        GlDraw.ends ();


        (* draw the second part of the parabola, up to the bounce point *)
        let maybeHit = whenWillTheTrajectoryHitTheNet t in
        let maybeIat = whenWillTheTrajArriveAtZ ~z:0.0 ~t in
        (match maybeIat with
         | None -> assert false
         | Some iat ->
           (match maybeHit with
            | Some _ -> ()
            | None ->
              if iat.iata_t < 0.0 then
                assert (false)
              else
                (assert(iat.iata_t > 0.0);
                 GlDraw.begins `line_strip;
                 GlDraw.color ~alpha:(opacity) (1.0, 1.0, 0.0);
                 let rec vertsBeforeBounce startT  count =
                   if count = 0 then
                     ( print_endline "Failed printing second part of parabola. Skipping.";
                       [])
                   else
                     let vertOfTime ti =
                       let ti2 = ti *. ti in
                       vec3dCreate
                         (t.impact.x3 +. t.startVel.x3 *. ti  -. 0.5 *. ti2 *. t.spin.x3)
                         (t.impact.y3 +. t.startVel.y3 *. ti  +. 0.5 *. ti2 *.
                                                                 (-. t.spin.y3 -. abs_float g))
                         (t.impact.z3 +. t.startVel.z3 *. ti  -. 0.5 *. ti2 *. t.spin.z3) in
                     let v = vertOfTime startT in
                     if v.y3 <= 0.0 then
                       []
                     else
                       v::(vertsBeforeBounce (startT +. 0.01)) (count -1 )in
                 List.iter (fun v -> GlDraw.vertex3 (v.x3, v.y3, v.z3)) (vertsBeforeBounce iat.iata_t 500);
                 GlDraw.ends ()));

           (* draw the bold point over the net *)
           let xn =
             t.impact.x3 +. t.startVel.x3 *. iat.iata_t -. 0.5 *. iat.iata_t *. iat.iata_t *. t.spin.x3 in

           let yn =
             t.impact.y3 +. t.startVel.y3 *. iat.iata_t
             +. 0.5 *. iat.iata_t *. iat.iata_t *. (-. t.spin.y3 -. abs_float g) in
           (match maybeHit with
            | None ->

              begin
                Gl.disable `depth_test;
                GlDraw.color ~alpha:opacity (1.0, 1.0, 0.0);
                GlDraw.point_size 3.5 ;
                GlDraw.begins `points;
                GlDraw.vertex3 ( xn, yn, 0.0 ) ;
                GlDraw.ends ();
                Gl.enable `depth_test
              end
            | Some _ ->
              ()
           );

           (* draw the vertical line over the net *)
           (match maybeHit with
            | None -> GlDraw.color ~alpha:(opacity *. 0.2) (1.0, 1.0, 1.0)
            | Some _ -> GlDraw.color ~alpha:(opacity *. 0.4) (1.0, 0.9, 0.2) );
           GlDraw.begins `line_strip;
           GlDraw.vertex3 ( xn, yn, 0.0 ) ;
           GlDraw.vertex3 ( xn, 0.0, 0.0 ) ;
           GlDraw.ends ();

        ) ;


        (* draw the bold point on the bounce point *)

        match maybeHit with
        | Some _ -> ()
        | None ->
          begin
            Gl.disable `depth_test;
            GlDraw.point_size 4.0;
            GlDraw.begins `points;
            GlDraw.color ~alpha:opacity (1.0, 1.0, 0.0);
            let bounc = whereWillItBounce t in
            GlDraw.vertex3 ( bounc.x2, 0.0, bounc.z2 ) ;
            GlDraw.ends ();
            Gl.enable `depth_test
          end;

          (* draw the bold points on the impact *)

          match maybeHit with
          | Some _ -> ()
          | None ->
            begin
              Gl.disable `depth_test;
              GlDraw.point_size 2.8;
              GlDraw.begins `points;
              GlDraw.color ~alpha:0.7 (1.0, 1.0, 0.0);
              GlDraw.vertex3 ( t.impact.x3, t.impact.y3, t.impact.z3 ) ;
              GlDraw.vertex3 ( t.impact.x3, 0.0, t.impact.z3 ) ;
              GlDraw.ends ();
              Gl.enable `depth_test
            end

      in (* end renderParabola *)



      let calcParabOpacity ~p ~curRunSpeed ~volley ~ballVelAtImpact =
        if realisticParabolaOpacity then
          let minVelAtWhichTheParabolaIsAffected = 
            if volley then  50.0 else 20.0 in
          let maxVelAtWhichTheParabolaIsVisible = 
            if volley then 120.0 else 85.0 in
          let velZFactor =
            let velZ_kmh = kmH_of_cmPerSec (length3d ballVelAtImpact) in
            if velZ_kmh < minVelAtWhichTheParabolaIsAffected then
              1.0
            else if velZ_kmh > maxVelAtWhichTheParabolaIsVisible then
              0.0
            else
              let m = 1.0 /. (abs_float ( minVelAtWhichTheParabolaIsAffected 
                                          -. maxVelAtWhichTheParabolaIsVisible)) in
              1.0 -. m *. (velZ_kmh -. minVelAtWhichTheParabolaIsAffected)
          in
          if volley then
            p.hp_maxParabOpacityVolleys   *. velZFactor

          else

            p.hp_maxParabOpacityGroundShots  *. velZFactor
        else
          1.0
      in (* end calcParabOpacity *)

      match hp.hp_state with
      | HPS_AutoSearchAfterOpening h ->
        let traj =

          buildTrajFromTwoPointsAndHeight ~impact:h.asao_Impact 
            ~htOverNet:h.asao_HtOverNet
            ~spin:(spinOfResearchKind ~r:h.asao_researchKind ~p:hp)
            ~bounceDesired:h.asao_CurAim ~targetRect:None
        in
        begin
          match traj with
          | Some tr ->
            if hp.hp_playsInTopmostCourtHalf then
              assert(tr.impact.z3 < 0.0)
            else
              assert(tr.impact.z3 > 0.0);
            let volley = 
              match h.asao_researchKind with
              | RKH_Normal vt ->
                ( match vt with
                  | VOT_Volee -> true
                  | VOT_NotVolee _ -> false)
              | RKH_Smash v -> v
              | RKH_Dive v -> v
              | RKH_StretchForward v -> v
            in
            renderParabola
              ~t:tr
              ~opacity:(calcParabOpacity ~p:hp  ~volley
                          ~curRunSpeed:h.asao_ModulusOfRunSpeedAtImpactTime
                          ~ballVelAtImpact:h.asao_BallVelAtImpact)
          | None ->
            assert(false) 
        end
      | HPS_AutoSearchBeforeOpening h ->
        let traj =
          buildTrajFromTwoPointsAndHeight ~impact:h.asbo_Impact 
            ~htOverNet:h.asbo_HtOverNet
            ~spin:(spinOfResearchKind ~p:hp ~r:h.asbo_researchKind)
            ~bounceDesired:h.asbo_CurAim ~targetRect:None
        in
        begin
          match traj with
          | Some tr ->
            if hp.hp_playsInTopmostCourtHalf then
              assert(tr.impact.z3 < 0.0)
            else
              assert(tr.impact.z3 > 0.0);
            let volley = 
              match h.asbo_researchKind with
              | RKH_Normal vt ->
                ( match vt with
                  | VOT_Volee -> true
                  | VOT_NotVolee _ -> false)
              | RKH_Smash v -> v
              | RKH_Dive v -> v
              | RKH_StretchForward v -> v
            in
            renderParabola
              ~t:tr
              ~opacity:(calcParabOpacity ~p:hp ~volley
                          ~curRunSpeed:h.asbo_ModulusOfRunSpeedAtImpactTime
                          ~ballVelAtImpact:h.asbo_BallVelAtImpact)
          | None ->
            assert(false)
        end

      | _ -> ()

  in (* end renderPar *)


  (match serverData with
   | NeitherServerNorClient ->
     for i = 0 to 1 do

       renderPar i
     done
   | Server _ ->
     renderPar 0
   | Client _ ->
     renderPar 1);

  (* draw player below*)

  let po = curPosOfPlayer playerToRenderLast in
  let x = po.x2 in
  let z = po.z2 in
  let color = 
    let alpha = 
      if theHeadCoversTheNet playerToRenderLast
      && thePlayerMustBeTransparentNow playerToRenderLast
      then
        0.5
      else 
        1.0

    in
    if thePlayerMustBeGreenNow playerToRenderLast then
      {r = 1.0; g = 1.0; b = 0.4; a = alpha}
    else if thePlayerMustBeRedNow playerToRenderLast then
      {r = 1.0; g = 0.6; b = 0.5; a = alpha}
    else
      {r = 1.0; g = 1.0; b = 1.0; a = alpha}
  in

  (renderObj3d ~o:(objOf playerToRenderLast) ~handleOfTexture ~pos:(Some (vec3dCreate x 0.0 z))
     ~flipX:(match serverData with | Client _ -> true | _  -> false) ~color;)

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

let loadTextures surface =
  let maxNumTextures = 1500 in
  let textureHandles = GlTex.gen_textures maxNumTextures in
  assert ( Array.length textureHandles = maxNumTextures);


  let handleOfTexture = StringMap.empty in
  let nextFreeTextureIndex = 0 in

  let surfaceFileName =
    match surface.s_material with
    | Clay ->gfxDir ^ "/terra.bmp.png"
    | Cement->gfxDir ^ "/cemento.bmp.png"
    | Grass -> gfxDir ^ "/erba.bmp.png"
  in

  let (handleOfTexture, nextFreeTextureIndex) =
    loadTextureFromFile ~fileName:surfaceFileName
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
  surfaceFileName, handleOfTexture
