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
open HumanPlayer
open ComputerPlayer

exception TheAngleAlongXIsTooCloseToPi2
exception MistakeWithUncertainPhysicalMeaning
exception NotImplemented

(* "bool IntMap.t" is a map  int -> bool *)
module IntMap = Map.Make (struct
    type t = int
    let compare = compare
  end )

type td2 = { 
  t2_numFramesSinceLastFpsUpdate:int;
  t2_timeOfLatestFpsCalculation:int;
  t2_frameCountList:int list
}

type td1 = {
  t1_numFramesSinceLastFpsUpdate:int;
  t1_timeOfLatestFpsCalculation:int
}

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

(* FIXME: It think this is unused *)
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

(* Checks if the service is to the right (TRUE) or to the left (FALSE) based
   on the points of the players *)
let serviceIsToTheRight s = 
  let p1, p2 = 
    match s.sc_state with
    | TieBreak a -> a.(0), a.(1)
    | NoTieBreak a -> a.points.(0), a.points.(1)
  in
  (( p1 + p2) mod 2) = 0

(* I haven't quite figured out what this is used for, but in non-networked games,
   the human player has scoreIndex = 0 and the computer player has scoreIndex = 1 *)
let scoreIndex p = 
  match p with
  | HP h -> h.hp_scoreIndex
  | CP c -> c.cp_scoreIndex


(* TODO: Move to ComputerPlayerModule *)
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

(* FIXME: These next two types are not being used *)
type decreaseLenResult = DLR_ErrorInsufficientPowerToSurpassNet
                       | DLR_Ok of float | DLR_ErrorCountReachedZero


type impactVote2 = VoteNonZero of float * volleyOrIntention 
                 | VoteZero


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
    let mouseSensitivity = 120.0 in (* TODO: move to Input or Options *)
    (* let xCamBehav = PushScroll in * XXX: Unused variable *)
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

    (* FIXME: While screen isn't used, it has side effects that are executed
    when the function is evaluated; so removing it breaks the code. I'll try to
    re-write the code so that the side effects are executed without needing to
    create this var. *)
    let listOfFlags =
      [`OPENGL;  (* `DOUBLEBUF is useless for opengl, see docs *) `RESIZABLE ]
    in
    (* FIXME: Added this ignore to shut the warning up, but I don't like this *)
    ignore(Sdlvideo.set_video_mode ~w:windowWt ~h:windowHt ~bpp:0 listOfFlags);
    Sdlwm.set_caption ~title:loadingString ~icon:freeTennisString;
    Sdlwm.set_icon (Sdlvideo.load_BMP ( gfxDir ^ "/ball-caption.bmp"));

    (* XXX: Not sure what this code does *)
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

    let windowWt, windowHt = resizeCallback windowWt windowHt in
    (*Load textures*)
    let surfaceFileName, handleOfTexture = Renderization.loadTextures surface in


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

    let players =
      let animdata = loadAnimdata

      in
      let p0 =
        createHumanPlayer ~playsInTopmostCourtHalf:false ~plName:opt.opt_p0 ~scoreIndex:0
          ~startPos:(vec2dCreate 0.0 (  courtHt2 +. 200.0)) 
          ~serverData:serverData ~animdata:animdata
      in
      let p1=
        match serverData with
        | NeitherServerNorClient ->
          createComputerPlayer ~playsInTopmostCourtHalf:true ~plName:opt.opt_p1 ~scoreIndex:1
            ~mat:surface.s_material    ~startPos:(vec2dCreate 0.0 ( -. courtHt2 +. 200.0))
            ~animdata:animdata ~skillLevel:opt.opt_skillLevel
        | Server _ | Client _ ->

          createHumanPlayer ~playsInTopmostCourtHalf:true ~plName:opt.opt_p1 ~scoreIndex:1
            ~startPos:(vec2dCreate 0.0 (  -. courtHt2 +. 200.0)) 
            ~serverData:serverData ~animdata:animdata
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
    let rec mainLoop ~players ~ball ~score ~timer ~nextServiceIsFirst ~vd =
      let vd = manageAllPendingSdlEvents ~vd ~windowHt ~windowWt in

      (* FIXME: Why isn't this part of manageAllPendingSdlEvents ? *)
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
              let letComputerKnowHeWon ~p 
                    ~siolpwhtb (* scoreIndexOfLastPlayerWhoHitTheBall *) 
                    ~players = 
                match p with 
                | CP c ->
                  if c.cp_scoreIndex = siolpwhtb then
                    let pwa, pws = 
                      let opponentCurPos = 
                        let mOpp = pick (Array.to_list players) (fun x -> scoreIndex x != c.cp_scoreIndex) in
                        match mOpp with
                        | None -> assert false
                        | Some opp -> curPosOfPlayer opp 
                      in
                      updateMemoryOfPointsWonAndLost ~p:c ~won:true ~opponentCurPos
                    in
                    CP {c with cp_pointsWonAttacking = pwa;
                        cp_pointsWonStayingBack = pws}
                  else
                    p
                | HP _ -> p
              in
              updateBall ~b:ball ~dt ~score ~surf:surface ~sounds 
                      ~nextServiceIsFirst ~opt ~players ~letComputerKnowHeWon
          in
          renderCourt ~surfaceType:surface.s_material 
            ~surfaceFileName:surfaceFileName ~handleOfTexture:handleOfTexture;

          render ~players:players ~ball:ball ~aidebug:opt.opt_aidebug ~serverData:serverData
            ~camData:camData ~handleOfTexture:handleOfTexture
            ~realisticParabolaOpacity:opt.opt_realisticParabolaOpacity ();

          renderText ~players:players ~ball:ball ~serverData:serverData 
            ~showRemotePause:(pausedOnTheOtherSide && not opt.opt_doNotShowPause)
            ~showLocalPause:(vd.vd_pausedWithKey && not opt.opt_doNotShowPause)
            ~windowHt:windowHt ~windowWt:windowWt ~handleOfTexture:handleOfTexture
            ~score:score;
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

    (**This is after the main loop returns. Shuts down network and
       		   SDL and exit**)
    (* FIXME: The network part should be moved to Network.ml *)
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
