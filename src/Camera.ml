open Math
open SharedData

(** --- Consts --- **)
let fovY = 16.9 (* increase the fov, and the upper player will be smaller with respect to the lower *)
let zNear = 100.0

(** --- Data Types --- **)
type cameraXBehavior = BehindThePlayer | Fixed | PushScroll

type cameraPositionAndDirection = { 
  eyeX :float ; eyeY :float; eyeZ: float;
  lookatX:float; lookatY:float; lookatZ:float
}

(** --- Functions --- **)
(* TODO: This is not exactly Camera related, but Screen related. Maybe this file should
be renamed to Screen.ml and all code related with screen stuff (including the Camera)
would be placed in here *)
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

(* TODO: The rest of the file is a single function; maybe some refactoring is needed *)
let calculateCamera ~fovy ~fovx ~znear ~posBottomPlayer ~posTopmostPlayer ~deltaCameraBackwards
    ~posBall ~xCamBehav  ~mustShowBottomCourtLine = 

  assert ( posTopmostPlayer.z2 < posBottomPlayer.z2);
  assert ( fovy > 0.0);
  assert (fovy < pi_2);

  let delta =
    let deltaForward = deltaCameraBackwards *. 0.6 in
    let t = (deltaCameraBackwards -. deltaForward) /. (courtHt2) in
    let zOpponentOrMe = min (min (abs_float posTopmostPlayer.z2) (courtHt2))
        (abs_float posBottomPlayer.z2  ) in
    degToRad (deltaForward +. t *. zOpponentOrMe) in

  let zPlayerOrBall = 
    let deltaZ = 200.0 in
    if mustShowBottomCourtLine then
      max (posBottomPlayer.z2 +. deltaZ)
        (max (posBall.z2 +. deltaZ) (courtHt2 +. 30.0) )
    else
      max (posBottomPlayer.z2 +. deltaZ) (posBall.z2 +. deltaZ)
  in


  let cameraX ~xg ~zc ~zl ~zg  ~fovXHalf ~d = 
    assert ( 0.0 <= fovXHalf  && fovXHalf <= pi_2);
    let zn = zc -. znear in
    assert (zg < zn);
    assert (zn < zc);
    let t = tan fovXHalf in


    let rec calcRec ~xg ~zn ~zc = 

      let calc xgg = 
        let x1g = xgg *. (zc -. zn) /. (zc -. zg) in
        let x18 = -. t *. (zc -. zn ) in
        if x1g -. x18 > d then
          0.0
        else
          let x1 = xgg -. (zc -. zg) *. d /. (zc -. zn) in
          let h = zc -. zl in
          let k = zc -. zg in
          let m = h -. k in 
          let b = -. (t *. x1 +. m) in
          let c = h *. x1 +. t *. h *. k in
          let discr = b *. b -. 4.0 *. t *. c in
          assert (discr >=0.0);
          let xc1 = ( -. b  +. sqrt discr ) /. ( 2.0 *. t) in
          let xc2 = ( -. b  -. sqrt discr ) /. ( 2.0 *. t) in
          let modxc1 = abs_float xc1 in
          let modxc2 = abs_float xc2 in
          if modxc1 < modxc2 then	
            xc1 
          else 
            xc2
      in
      let xc = calc xg in
      let iCanSeeTheCourtCorner = 
        let ang =


          let  eye = vec2dCreate xc zc in
          let fromEyeToLookat = 
            let lookat = vec2dCreate 0.0 zl in
            vec2dSub lookat eye 
          and fromEyeToCorner = 
            let courtCorner = vec2dCreate (-. courtWt2 -. 20.0) (-. courtHt2) in

            vec2dSub courtCorner eye in

          smallestAngleBetween fromEyeToCorner fromEyeToLookat in
        ang < fovXHalf 
      in
      if iCanSeeTheCourtCorner then
        xc, zn ,zc 
      else
        let z4' =
          let d' = d +. 10.0 
          and z4 = zc in
          (d' /. d) *. (z4 -. zl) +. zl in
        calcRec ~xg ~zn:(z4' -. znear) ~zc:z4'

    in

    if xg <= 0.0 then
      calcRec ~xg ~zn ~zc
    else
      let xc, zn, zc = calcRec ~xg:(-. xg)  ~zn ~zc in
      (abs_float xc, zn, zc)
  in

  let cameraZ ~alfa ~z3 ~z1 ~y1 ~delta = 

    let beta = 
      pi -. alfa *. 0.5  -. delta in
    let eps = pi -. beta
    and c = 
      let gamma = 
        pi -. alfa -. beta
      in
      let a = 
        let z0 = 
          let f = 
            y1 /. tan gamma in
          z1 -. f in
        z3 -. z0 in
      (sin gamma) *. a /. (sin alfa) in
    let z4 = 
      let g = 
        c *. cos eps in
      z3 +. g 
    and zL = 

      let h = 
        c *. sin (alfa *. 0.5) /. sin delta in
      z3 -. h 
    and y4 =
      c *. sin eps 
    in
    let d = y4 /. sin delta in
    (z4, zL, y4, d)
  in


  let (eyeZ, lookatZ, eyeY, d) = 
    let zOffsAboveHeadOfPAbove = 
      480.0
    in
    cameraZ ~alfa:(fovy) ~z3:zPlayerOrBall 
      ~z1:(-. courtHt2)
      ~y1:zOffsAboveHeadOfPAbove ~delta in

  let (eyeX, eyeZ, lookatX) = 
    match xCamBehav with

    | Fixed ->  (0.0 , eyeZ, 0.0)

    | BehindThePlayer ->
      let playerLine = straightLineBetween (vec2dCreate 0.0
                                              (-. courtHt2) )
          posBottomPlayer in
      let horizLine = straightLineBetween (vec2dCreate 0.0 eyeZ)
          ( vec2dCreate 20.0 eyeZ) in
      let camPos = intersectionOfStraightLines playerLine horizLine    in
      (camPos.x2, eyeZ, 0.0 )

    | PushScroll ->
      let xc, _, zc = 
        cameraX ~xg:posBottomPlayer.x2 ~zc:eyeZ ~zl:lookatZ
          ~zg:posBottomPlayer.z2  ~fovXHalf:(fovx *. 0.5) ~d:13.5 in
      (xc, zc, 0.0)
  in
  { eyeX = eyeX; eyeY = eyeY; eyeZ = eyeZ; lookatX = lookatX; lookatY
                                                              = 0.0; lookatZ = lookatZ}  
