open Math

let g = -. 980.0 (* points towards the ground * TODO: This needs a better name*)

(* Packagers can customize these strings if they want to install the
   executable and the graphics/sound directories in different
   places. If you choose to customize them, use absolute paths without
   the trailing slash. *)
let gfxDir = "graphics" (* where the pngs and subdirs are located. 
			   example of customization: /usr/share/freetennis/gfx *)

(* ----- TODO: I think many of these should be in the AnimationModule ------*)

(* "bool StringMap.t" is a map  string -> bool *)
module StringMap = Map.Make (String)

(** ----- Animation Data ----- **)
type animFrame = { animFrameDuration:float (*seconds *);
		   animFrameTexture: string; animFrameHotSpot:vec2d;
		   animFrameDimensionsOfRect:vec2d (* in pixels *)}

type serviceAnim = {serviceAnim_FrameOfBallLaunch:int;
		    serviceAnim_FrameOfImpact:int; 
		    serviceAnim_TimeFromLaunchToImpact:float;
		    serviceAnim_ArrayOfFrames: animFrame array}

type shotAnim = {shotAnim_ArrayOfFrames: animFrame array;
		 shotAnim_FrameOfImpact: int;
		 shotAnim_TimeFromOpeningToImpact: float}

type animation = ServiceAnimation of serviceAnim
		 | RunAnimation of  animFrame array
		 | ShotAnimation of shotAnim

type animState = Animated of float | NotAnimated | PausedDuringService

(** ----- Court Data ----- **)
let tol = 5.0 (* cm of tolerance due to the fact that the ball is not a point object, so when it bounces outside it can touch the line *)

let netHtCenter = 91.4

let netHtBorder = 107.0

(* Court width*)
let courtWt = 823.0
let courtWt2 = courtWt /. 2.0
let courtWt4 = courtWt /. 4.0

(* Court width with doubles corridor*) 
let courtWtDoubles = 1097.3
let corridorWt = (courtWtDoubles -. courtWt ) *. 0.5
let corridorWt2 = corridorWt *. 0.5

(* Court height*)
let courtHt = 2377.4
let courtHt2 = courtHt *. 0.5 (* 1188.7 *)
let courtHt4 = courtHt *. 0.25 (* 594.35 *)

let leftBound = -. courtWt2 -. corridorWt2 -. 580.0
let upperBound = -. courtHt2 -. 1100.0
let rightBound = -. leftBound
let lowerBound = -. upperBound

let distanceFromPolesToExternalBorder = 91.4

let leftPoleX = -. courtWt2  -. distanceFromPolesToExternalBorder

let rightPoleX = courtWt2  +. distanceFromPolesToExternalBorder

let netHtAtX x = 
    let x = abs_float x in
    if x < rightPoleX then
	let t = (netHtBorder -. netHtCenter) /. rightPoleX in
	netHtCenter +. t *. x
    else
	0.0

type material = Cement | Grass | Clay

type surface = {s_material:material;
		s_spinAttenuationFactor:float;
		s_velXZAttenuationFactor:float;
		s_velYAttenuationFactor:float}

type rectangleTheShotIsMeantToFallIn = { 
    rtsimtfi_top : float;
    rtsimtfi_bottom : float;
    rtsimtfi_left : float;
    rtsimtfi_right : float }

(* TODO: Change names from Italian to English :-) *)
(* upperLeftServiceBox *)
let servizioInAltoSulPari  = {rtsimtfi_top = -. courtHt4 -. tol;
			      rtsimtfi_bottom = 0.0 +. tol;
			      rtsimtfi_left = -. courtWt2 -. tol;
			      rtsimtfi_right = 0.0
			     }

(* upperRightServiceBox *)
let servizioInAltoSulDispari  = {rtsimtfi_top = -. courtHt4 -. tol;
				 rtsimtfi_bottom = 0.0 ;
				 rtsimtfi_left = 0.0;
				 rtsimtfi_right = courtWt2 +. tol
				}

(* lowerLeftServiceBox *)
let servizioInBassoSulPari  = {rtsimtfi_top = 0.0;
			       rtsimtfi_bottom = courtHt4 +. tol;
			       rtsimtfi_left = 0.0;
			       rtsimtfi_right = courtWt2 +. tol
			      } 

(* lowerRightServiceBox *)
let servizioInBassoSulDispari  = {rtsimtfi_top = 0.0;
				  rtsimtfi_bottom = courtHt4 +. tol;
				  rtsimtfi_left = -. courtWt2 -. tol;
				  rtsimtfi_right = 0.0
				 }

let upperHalfOfCourt = {rtsimtfi_top = -. courtHt2 -. tol;
			rtsimtfi_bottom = 0.0;
			rtsimtfi_left = -. courtWt2 -. tol;
			rtsimtfi_right = courtWt2 +. tol
		       }

let lowerHalfOfCourt = {rtsimtfi_top = 0.0;
			rtsimtfi_bottom = courtHt2 +. tol;
			rtsimtfi_left = -. courtWt2 -. tol;
			rtsimtfi_right = courtWt2 +. tol
		       }

(** ------ List ------ **)
open List

exception EmptyList

(* "accumulate l f state" invokes f on each element of l, but
   threading a state. This means that

   f x state

   is called for each x in l, but each time with the state produced
   by the previous call.

   Example:

   accumulate [1;2;4] f initial_state

   means

   let st2 =
       let st1 = f 1 initial_state in
       f 2 st1 in
   f 4 st2

*)
let rec accumulate ~list ~f ~state =
    match list with
	    [] -> state
	| h::t -> 
	      let state' = f h state 
	      in
	      accumulate ~list:t ~f:f ~state:state'

let rec allPairs x y = 
    match x with
	| [] -> []
	| x1::xt -> 
	      let x1y = List.map (fun k -> (x1, k)) y in
	      List.append x1y (allPairs xt y)



let exists l p =
    let fil = List.filter p l in
    length fil != 0


let pick l p = 
    let fil = List.filter p l in
    match fil with
	| [] -> None
	| h::_ -> Some h

let rec select_some l = 
    match l with 
	| [] -> []
	| Some x :: xs -> x :: select_some xs
	| None :: xs -> select_some xs 


let numOf l f = 
    length (List.filter f l)

let printList l f = 
    List.iter (fun x -> print_endline (f x)) l

let rec findBestElement l  better = 
    match l with
	| [] -> raise EmptyList
	| [x] -> x
	| h::t -> better h (findBestElement t better)

(* e.g. listFromTo 0 5 = [0; 1; 2; 3; 4]. 5 is NOT present. *)
let rec listFromTo a b =
    if a >= b then [] else a::listFromTo (a+1) b

(** ------- Direction ------ **)
type leftOrRight = Right | Left

let oppositeDir d = match d with Right -> Left | Left -> Right

module DirectionMap = Map.Make (struct
				    type t = leftOrRight

				    let compare x y =
					match x with 
					    | Left ->
						  (match y with
						       | Left -> 0
						       | Right -> -1 )
					    | Right ->
						  (match y with
						       | Left -> 1
						       | Right -> 0 )
				end )

