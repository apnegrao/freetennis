exception NullVector
exception ParallelVectors

let cmPerSecondOfKmh x = x *. ( 1000.0 *. 100.0 /. (60.0 *. 60.0)) 

let kmH_of_cmPerSec x = x *. (60.0 *. 60.0) /. ( 1000.0 *. 100.0)

let pi = 4.0 *. atan 1.0

let pi_2 = pi *. 0.5

let degToRad x = x *. pi /. 180.0

let radToDeg x = x *. 180.0 /. pi

let solve2ndDegreeEquat a b c = 
  let discr = b *. b -. 4.0 *. a *. c in
  if discr < 0.0 then
    None
  else
    let den = 2.0 *. a in
    if den = 0.0 then
      None
    else
      let sq = sqrt(discr) in
      Some (( -. b -. sq) /. den , ( -. b +. sq ) /. den)

(*----------2D Vectorial Algebra----------*)

type vec2d = {x2:float; z2:float}

let isNull2d v = v.x2 = 0.0 && v.z2 = 0.0

let stringOfVec2d v= 
  "x = " ^ string_of_float v.x2  ^ ", z = " ^ string_of_float v.z2

let vec2dCreate x z = {x2= x; z2 = z}

let vec2dNull = vec2dCreate 0.0 0.0 

let flipxz2 a = vec2dCreate  (-. a.x2)   (  -. a.z2)

let length2d v = sqrt(v.x2 *. v.x2 +. v.z2 *. v.z2)

let normalize2d v = 
  let l = length2d v in
  vec2dCreate (v.x2 /. l) (v.z2 /. l)

let dotProduct2d v1 v2 = 
  v1.x2 *. v2.x2 +. v1.z2 *. v2.z2

let vec2dAdd v1 v2 = 
  { x2 = v1.x2 +. v2.x2; z2 = v1.z2 +. v2.z2 }

let vec2dMulScalar s v  = 
  { x2 = s *. v.x2 ; z2 = s *. v.z2 }

let vec2dSub v1 v2 = 
  { x2 = v1.x2 -. v2.x2; z2 = v1.z2 -. v2.z2 }

let distance2d p1 p2 =
  length2d ( vec2dSub p1 p2)

(*--------3D Vectorial Algebra---------*)

type vec3d = {x3:float; y3:float; z3:float}

let projection2d v = {x2 = v.x3; z2 = v.z3}

let stringOfVec3d v= 
  "x = " ^ string_of_float v.x3 ^ ", y = " ^ string_of_float v.y3 ^ ", z = " ^ string_of_float v.z3

let vec3dCreate x y z = {x3= x; y3=y; z3 = z}

let vec3dNull = vec3dCreate 0.0 0.0 0.0

let length3d v = sqrt(v.x3 *. v.x3  +. v.y3 *. v.y3 +. v.z3 *. v.z3)

let dotProduct3d v1 v2 = 
  v1.x3 *. v2.x3 +. v1.y3 *. v2.y3 +. v1.z3 *. v2.z3

let vec3dSub v1 v2 = 
  { x3 = v1.x3 -. v2.x3; y3 = v1.y3 -. v2.y3; z3 = v1.z3 -. v2.z3 }

let vec3dMulScalar s v  = 
  { x3 = s *. v.x3 ; y3 = s *. v.y3;  z3 = s *. v.z3 }

(*------Angles------*)
let smallestAngleBetween v1 v2 = 
  let den = length2d v1 *. length2d v2 in
  if den = 0.0 then
    raise NullVector
  else
    acos ( ( dotProduct2d v1 v2 ) /. den)

let smallestAngleBetween3d v1 v2 = 
  let den = length3d v1 *. length3d v2 in
  if den = 0.0 then
    raise NullVector
  else
    acos ( ( dotProduct3d v1 v2 ) /. den)

let smallestAngleWithZAxis v1 = 
  let zAxis = { x2 = 0.0 ; z2 = 1.0} in
  smallestAngleBetween zAxis v1

let smallestAngleWithNegativeZAxis v1 = 
  let zAxis = { x2 = 0.0 ; z2 = -. 1.0} in
  smallestAngleBetween zAxis v1

(*----Matrix Module----*)
module Matrix2x2 = struct
  type t = { a: float; b: float; c: float; d:float}

  let create a' b' c' d' = 
    {a= a'; b= b' ; c= c'; d= d'}

  let det  v = v.a *. v.d -. v.b *. v.c
end

(*------Lines-----*)

type straightLine = {sl_a:float ; sl_b:float; sl_c:float}

let straightLineBetween p1 p2 = 
  let dx = p2.x2 -. p1.x2 in
  if dx = 0.0 then
    {sl_a = -. 1.0 ; sl_b = 0.0 ; sl_c = p1.x2}
  else
    let t =     
      let dz = p2.z2 -. p1.z2 in
      dz /. dx in
    { sl_a = t ; sl_b = -. 1.0 ; sl_c = p1.z2  -. t *. p1.x2 }

let straightLineFromPointAndDir p dir = 

  let x0 = p.x2 in
  let a, b, c =
    if dir.x2 = 0.0 then
      -. 1.0, 0.0, x0 
    else
      let z0 = p.z2 in
      let t = dir.z2 /. dir.x2 in
      (t, -. 1.0, z0 -. t *. x0)
  in
  {sl_a = a; sl_b = b; sl_c = c}

let intersectionOfStraightLines l1 l2 = 
  let d = 
    let a = Matrix2x2.create l1.sl_a l1.sl_b l2.sl_a l2.sl_b in
    Matrix2x2.det a in
  if d = 0.0 then
    raise ParallelVectors
  else
    let b1 = Matrix2x2.create ( -. l1.sl_c)  l1.sl_b (-. l2.sl_c) l2.sl_b in
    let b2 = Matrix2x2.create l1.sl_a (-. l1.sl_c) l2.sl_a (-. l2.sl_c) in
    { x2 = (Matrix2x2.det b1) /. d ; z2 = (Matrix2x2.det b2) /. d }
