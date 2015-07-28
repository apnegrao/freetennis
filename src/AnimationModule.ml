open Math

open SharedData

open Objects3D

(** --- Consts --- **)

let white = {r = 1.0; g=1.0; b=1.0; a=1.0}

(** --- Data Types --- **)

(** --- Functions --- **)
let durationOfCurAnimUpToImpactFrame ~o =
    let a = StringMap.find o.o3d_curAnimName o.o3d_animations in
    match a with 
	| ShotAnimation s ->
	      s.shotAnim_TimeFromOpeningToImpact
	| RunAnimation _ ->
	      raise ThereIsNoImpactFrameInThisAnim
	| ServiceAnimation s ->
	      s.serviceAnim_TimeFromLaunchToImpact


let updateAnim o dt = 
    match o.o3d_animState with
	| Animated timer -> 
	      begin 
		  let a = StringMap.find o.o3d_curAnimName o.o3d_animations in
		  let frames = 
		      match a with
			  | RunAnimation x -> x
			  | ServiceAnimation x -> x.serviceAnim_ArrayOfFrames
			  | ShotAnimation x -> x.shotAnim_ArrayOfFrames in
		  let f = frames.(o.o3d_curFrameIdx) in
		  let timer = timer +. dt in
		  if timer <= f.animFrameDuration then
		      { o with o3d_animState = Animated timer }
		  else
		      
		      let objWithNewStateAndTimer  = 
			  let newTimer = timer -. f.animFrameDuration in
			  if o.o3d_curFrameIdx + 1 >= Array.length frames then
			      match a with
				  | RunAnimation _ -> 
					  { o with
						o3d_curFrameIdx = 0;
						o3d_animState = Animated newTimer}
				  | ShotAnimation _ 
				  | ServiceAnimation _ -> 
					{ o with
					      o3d_curFrameIdx = Array.length frames - 1;
					      o3d_animState = NotAnimated}

			  else
			      { o with
				    o3d_curFrameIdx = o.o3d_curFrameIdx + 1;
				    o3d_animState = Animated newTimer}
		      in
		      
		      match a with 
			  |  ServiceAnimation s ->
				  if o.o3d_curFrameIdx + 1 = s.serviceAnim_FrameOfBallLaunch then
				      { o with
					    o3d_animState = PausedDuringService;
					    o3d_curFrameIdx = o.o3d_curFrameIdx + 1}
				  else
				      objWithNewStateAndTimer
					  
			  | RunAnimation _ | ShotAnimation _ -> objWithNewStateAndTimer
	      end
	| NotAnimated | PausedDuringService -> o

let setAnim ~animName  ~o ~restartIfSameAnimation = 

    let an , fr, st = 
	if 0 != (compare animName o.o3d_curAnimName) then
	    animName, 0, Animated 0.0
	else
	    if restartIfSameAnimation then
		animName, 0, Animated 0.0
	    else
		o.o3d_curAnimName, o.o3d_curFrameIdx, o.o3d_animState
    in
    { o with o3d_curFrameIdx = fr;
	  o3d_curAnimName = an;
	  o3d_animState = st}

let computeWalkAnim ~footTarget ~curPos ~dirsign   = 
    let dir = vec2dSub footTarget curPos in
    assert( dir.x2 != 0.0 || dir.z2 != 0.0);

    let appropriateZAxis  = 
	vec2dCreate (0.0)  (dirsign *. (-. 1.0)) 
    in
    let appropriateXAxis  = 
	vec2dCreate (dirsign *. (-. 1.0)) 0.0
    in

    let angZ = 
	smallestAngleBetween appropriateZAxis dir in
    assert(angZ >= 0.0);
    if angZ < degToRad 40.0 then

	"su"
    else if angZ < degToRad (130.0) then
	let angX = smallestAngleBetween appropriateXAxis dir in
	
	if angX < degToRad 90.0 then
	    "sinistra"
	else
	    "destra"
    else
	"giu"

let loadAnimData = 
	let fma = 0.09 in
	let fma2 = 0.07 in
	let fm = 0.08 in
	(* Unused variable? -- let fm2 = fm *. 2.0 in -- *)
	let fm3 = fm *. 3.0 in
	let fm4 = fm *. 4.0 in
	(* Unused variable? -- let fm8 = fm *. 8.0 in -- *)
	let fm23 = fm *. 2.0 /. 3.0 in
	let fm12 = fm  /. 2.0 in
	let fm13 = fm  /. 3.0 in
	let fm14 = fm  /. 4.0 in
	      
	let noScale = (1.0, 1.0) in
	      [ gfxDir ^ "/Battesa", NoImpactFrame, [| fma2; fma2; fma2; fma2 |], noScale;
		gfxDir ^ "/Bdestra", NoImpactFrame, [| fma; fma; fma; fma; fma; fma |], noScale;
			gfxDir ^ "/Bsinistra", NoImpactFrame, [| fma; fma; fma; fma; fma |], noScale;
		gfxDir ^ "/Bgiu", NoImpactFrame, [| fma; fma; fma; fma; fma |], noScale;
		gfxDir ^ "/Bsu", NoImpactFrame, [| fma; fma; fma; fma; fma |], noScale;
			gfxDir ^ "/Bdritto", NotService 10, [| fm23; fm23; fm; fm23; fm23;
						       fm23; fm23; fm12; fm12; fm12;
						       fm14; fm4 |], noScale;
			gfxDir ^ "/Brovescio", NotService 12, [| fm23; fm23; fm23; fm23; fm23; fm23; 
							 fm13; fm13; fm13; fm13; fm13; fm13; fm13; fm13; 
							 fm4 |], noScale;
		gfxDir ^ "/Brovescioback", NotService 5, [|  fm; fm; fm; fm; fm; fm;
							     fm; fm |], noScale;
		gfxDir ^ "/Bdrittoback", NotService 4, [|  fm; fm; fm; fm; fm; fm |], noScale;
		gfxDir ^ "/Bdrittov", NotService 2, [|  fm12; fm; fm12; fm12; fm12; fm12 |], noScale;
		gfxDir ^ "/Bdrittoallungov", NotService 1, [|  fm14; fm3 ;fm|], noScale;
		gfxDir ^ "/Brovescioallungov", NotService 1, [| fm14; fm3; fm |], noScale;

		gfxDir ^ "/Bdrittoforwardstretch", NotService 1, [|  fm4 ; fm|], noScale;
		gfxDir ^ "/Brovescioforwardstretch", NotService 1, [| fm4 ;fm |], noScale;
		gfxDir ^ "/Adrittoforwardstretch", NotService 1, [|  fm4;fm|], noScale;
		gfxDir ^ "/Arovescioforwardstretch", NotService 1, [| fm4;fm |], noScale;
			gfxDir ^ "/Brovesciov", NotService 5, [|  fm14; fm; fm14; fm14;
							  fm14; fm; fm |], noScale;
			gfxDir ^ "/Bsmash", NotService 4, [|  fm; fm; fm; fm; fm |], noScale;
		gfxDir ^ "/Bservizio", Service (5, 10), [| fm *. 16.0; fm; fm;fm;fm;fm;fm;fm12;
							   fm12;fm12;fm12;fm12;fm;fm |], (0.37, 0.5);
		gfxDir ^ "/Aattesa", NoImpactFrame, [| fma2; fma2; fma2; fma2|], noScale;
		gfxDir ^ "/Asaltello", NoImpactFrame, [| fma2|], noScale;
		gfxDir ^ "/Bsaltello", NoImpactFrame, [| fma2|], noScale;
		gfxDir ^ "/Adestra", NoImpactFrame, [| fma; fma; fma; fma;fma |], noScale;
			gfxDir ^ "/Asinistra", NoImpactFrame, [| fma; fma; fma; fma; fma |], noScale;
		gfxDir ^ "/Agiu", NoImpactFrame, [| fma; fma; fma; fma; fma |], noScale;
		gfxDir ^ "/Asu", NoImpactFrame, [| fma; fma; fma; fma; fma |], noScale;
			gfxDir ^ "/Adritto", NotService 8, [|fm23; fm23; fm23;
						     fm23; fm23; fm23; fm23; fm23; fm23; fm; fm3|], noScale;
		gfxDir ^ "/Arovescio", NotService 11, [|fm23; fm23; fm23;
							fm23; fm23; fm23;
							fm12; fm12; fm14;
							fm14; fm14; fm14;
							fm23; fm3|], noScale;
		gfxDir ^ "/Arovescioback", NotService 4, [|fm; fm; fm; fm;
							   fm; fm; fm|], noScale;
		gfxDir ^ "/Adrittoback", NotService 4, [|fm; fm; fm; fm;
							 fm; fm|], noScale;
		gfxDir ^ "/Adrittov", NotService 4, [|fm12; fm12; fm; fm12; fm12;
						      fm|], noScale;
		gfxDir ^ "/Arovesciov", NotService 5, [|fm12; fm12; fm; fm12; fm12;
							fm12; fm|], noScale;
		gfxDir ^ "/Aservizio", Service (7, 19), [| fm *. 16.0;
							   fm; fm; fm; fm;fm; fm;
							   fm12 ; fm12; fm12;fm12;fm12;fm12;fm12;fm12;fm12;fm12;fm12;fm12;fm12;fm12;
							   fm12;fm12;fm12;fm12
								|], (0.4, 0.5);
		gfxDir ^ "/Adrittoallungov", NotService 1, [|   fm14; fm3 ; fm |], noScale;
		gfxDir ^ "/Arovescioallungov", NotService 1, [|  fm14; fm3; fm |], noScale;
		gfxDir ^ "/Asmash", NotService 3, [|  fm; fm; fm; fm; fm |], noScale;
	      ]

