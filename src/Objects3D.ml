open Math

open SharedData

exception ThereIsNoImpactFrameInThisAnim
exception HotSpotNotFound

(** --- Consts --- **)
let sizeOfAPixelInCm = (* ony for some animation frames *) 1.72

let shadowIntensity = 0.2

(** --- Data Types --- **)
type infoAboutImpactFrame = NotService of int | Service of int * int | NoImpactFrame

type vertex = {vertX:float; vertY:float ; vertZ:float ; vertU:float ; vertV:float}

type rgba = {r:float; g:float; b:float; a:float}

type polygon = { polyVerts: vertex list;
                 polyTextureHandle:GlTex.texture_id ;
                 polyColor: rgba; polyVisible:bool}

type obj3d = { o3d_curFrameIdx:int ; 
               o3d_curAnimName:string;
               o3d_animations:animation StringMap.t (* string->animation *);
               o3d_animState: animState; 
               o3d_visible: bool}

(** --- Functions --- **)
let vertexCreate x y z u v = {vertX = x; vertY = y; vertZ=z; vertU=u; vertV=v}

let renderPolygon p maybePos = 
  if not p.polyVisible then
    ()
  else
    begin
      GlDraw.color ~alpha:p.polyColor.a (p.polyColor.r, p.polyColor.g,
                                         p.polyColor.b);
      Gl.enable `depth_test;

      begin
        match maybePos with
        | None -> ()
        | Some pos ->
          begin
            GlMat.push ();
            GlMat.translate ~x:pos.x3 ~y:pos.y3 ~z:pos.z3 ();
          end;
      end;
      GlTex.bind_texture ~target:`texture_2d p.polyTextureHandle;
      Gl.enable `texture_2d;
      GlDraw.begins `triangle_fan;

      let foo v =
        GlTex.coord2 (v.vertU, v.vertV) ;
        GlDraw.vertex3 ( v.vertX, v.vertY ,v.vertZ ) in

      List.iter foo p.polyVerts;
      GlDraw.ends ();
      match maybePos with
      |  None ->()
      | Some _ -> GlMat.pop ()
    end

let renderObj3d ~o ~handleOfTexture ~pos ~flipX ~color=

  let curFram = 
    let frames = 
      let a = 
        try
          StringMap.find o.o3d_curAnimName o.o3d_animations 
        with Not_found ->
          ( print_endline ("animation Not_found:" ^ o.o3d_curAnimName); raise Not_found)
      in
      match a with
      | RunAnimation x -> x
      | ServiceAnimation x -> x.serviceAnim_ArrayOfFrames
      | ShotAnimation x -> x.shotAnim_ArrayOfFrames in
    frames.(o.o3d_curFrameIdx) in
  let texHandle =
    try
      StringMap.find curFram.animFrameTexture handleOfTexture  
    with Not_found ->
      ( print_endline ("texture  Not_found:" ^ curFram.animFrameTexture); raise Not_found)
  in


  let vs, vsShad, vShad2 = 
    let w = curFram.animFrameDimensionsOfRect.x2 *. sizeOfAPixelInCm in
    let h = curFram.animFrameDimensionsOfRect.z2 *. sizeOfAPixelInCm in
    let hx = curFram.animFrameHotSpot.x2 *. sizeOfAPixelInCm in
    let hz = curFram.animFrameHotSpot.z2 *. sizeOfAPixelInCm in
    let hz2 = hz *. 2.0 in
    let h2 = h *. 2.0 in

    let flipSign = if flipX then -. 1.0 else 1.0 in
    ( [ vertexCreate (flipSign *. (-. hx))  hz 0.0 0.0 0.0 ;
        vertexCreate (flipSign *. (w -. hx))   hz 0.0 1.0 0.0;
        vertexCreate  (flipSign *.( w -. hx)) ( -. h +. hz) 0.0 1.0 1.0; 
        vertexCreate (flipSign *. (-. hx))  (-. h +. hz) 0.0 0.0 1.0 ] ,

      [ vertexCreate ((flipSign *. (-. hx)) +. 100.0)      0.5   hz2              0.0 0.0 ;
        vertexCreate ((flipSign *. (w -. hx)) +. 100.0)    0.5   hz2              1.0 0.0;
        vertexCreate  (flipSign *.( w -. hx))   0.5   ( -. h2 +. hz2)   1.0 1.0; 
        vertexCreate (flipSign *. (-. hx))      0.5   (-. h2 +. hz2)    0.0 1.0

      ],
      [ vertexCreate ((flipSign *. (-. hx)) -. 100.0)      0.5   hz2              0.0 0.0 ;
        vertexCreate ((flipSign *. (w -. hx)) -. 100.0)    0.5   hz2              1.0 0.0;
        vertexCreate  (flipSign *.( w -. hx))   0.5   ( -. h2 +. hz2)   1.0 1.0; 
        vertexCreate (flipSign *. (-. hx))      0.5   (-. h2 +. hz2)    0.0 1.0

      ])
  in
  let pol = { polyTextureHandle = texHandle ;
              polyVerts = vs;
              polyColor = { r = color.r; g = color.g; b = color.b; a = color.a};
              polyVisible = true
            } in

  let polShad = {pol with
                 polyColor = { r = 0.0; g = 0.0 ; b = 0.0; a = shadowIntensity};
                 polyVerts   = vsShad;
                } in
  (*     let polShad2 = {pol with *)
  (* 		       polyColor = { r = 0.0; g = 0.0 ; b = 0.0; a = 0.3}; *)
  (* 		       polyVerts   = vShad2; *)
  (* 		  } in *)

  ( renderPolygon pol pos;
    renderPolygon polShad pos;
    (*       renderPolygon polShad2 pos *)

  )

let create3dObj ~dirs ~initialAnim =
  let aMap =
    let animations =
      let animOfDir  (d, impactFrame, times, (scalex, scaley))  =
        print_endline ("Creating animation: " ^ d);
        let arr =
          let filesWithIndices =
            let files =
              let notCVS x =
                0 != (compare x "CVS") in
              List.sort compare (List.filter notCVS (Array.to_list  (Sys.readdir d))) in
            if  List.length files != Array.length times then
              (print_endline (d);
               assert(false))
            else
              ();
            let rec filesWithIndicesAux l next =
              match l with
              | [] -> []
              | h::t -> (h,next)::(filesWithIndicesAux t (next + 1)) in
            filesWithIndicesAux files 0 in
          let animFrameOfFile (scalex, scaley) (f, i) =
            let s = GdkPixbuf.from_file (d^"/"^f) in
            let findTheHotSpot s =
              let hotspots =

                let pairs =
                  let l1 = listFromTo 0 (GdkPixbuf.get_width s) in
                  let l2 = listFromTo 0 (GdkPixbuf.get_height s) in
                  allPairs l1 l2 in
                let isHotspot (x, y) =
                  let r, g, b, a =
                    let getpixel x y s =
                      let pixels = GdkPixbuf.get_pixels s in
                      assert( (GdkPixbuf.get_bits_per_sample s ) = 8);
                      let nChannels = GdkPixbuf.get_n_channels s in
                      assert(nChannels = 4 || nChannels = 3);
                      let offs =
                        let pitch = GdkPixbuf.get_rowstride s in
                        y * pitch + x * nChannels in
                      if nChannels = 3 then
                        (Gpointer.get_byte pixels ~pos:offs,
                         Gpointer.get_byte pixels ~pos:(offs + 1),
                         Gpointer.get_byte pixels ~pos:(offs + 2),
                         255 )
                      else
                        (Gpointer.get_byte pixels ~pos:offs,
                         Gpointer.get_byte pixels ~pos:(offs + 1),
                         Gpointer.get_byte pixels ~pos:(offs + 2),
                         Gpointer.get_byte pixels ~pos:(offs + 3) )in
                    getpixel x y s in
                  r = 255 && g = 0 & b = 255 && a = 255 (* needed! Transparent pixels still have colors, and that
                                                           											       color could be magenta *) in
                List.filter  isHotspot pairs in
              match hotspots with
              | [] -> raise HotSpotNotFound
              | [x, y] -> vec2dCreate (scalex *.(float_of_int x)) ((float_of_int y)*. scaley)
              | (x,y)::_ ->
                ( print_endline ( "More than one hotspot found in file " ^ f ^ ". Picking first");
                  printList hotspots (fun (x, y) ->  "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")");
                  vec2dCreate (scalex *. (float_of_int x))  (scaley *. (float_of_int y)))
            in
            { animFrameDuration = times.(i);
              animFrameTexture = d ^"/"^f;
              animFrameDimensionsOfRect = vec2dCreate
                  (float_of_int(GdkPixbuf.get_width s) *. scalex)
                  (float_of_int (GdkPixbuf.get_height s) *. scaley);
              animFrameHotSpot = (try
                                    findTheHotSpot s
                                  with HotSpotNotFound ->
                                    vec2dCreate ((float_of_int(GdkPixbuf.get_width s)) *. scalex /. 2.0)
                                      (float_of_int (GdkPixbuf.get_height s) *. scaley)
                                 )
            }
          in
          Array.map (animFrameOfFile (scalex, scaley)) (Array.of_list filesWithIndices)
        in

        match impactFrame with
        | NoImpactFrame -> (d, RunAnimation arr)

        | Service (launch, imp) ->
          let dur =
            let durations =
              let durationOfPair (f, i) =
                if launch <= i && i <imp then
                  f.animFrameDuration
                else
                  0.0 in
              let pairs =
                let arrl = Array.to_list arr in
                List.combine arrl (listFromTo 0 (List.length arrl)) in
              List.map durationOfPair pairs in
            List.fold_left (+.) 0.0 durations in
          (d, ServiceAnimation  {serviceAnim_FrameOfBallLaunch = launch;
                                 serviceAnim_FrameOfImpact = imp;
                                 serviceAnim_TimeFromLaunchToImpact = dur;
                                 serviceAnim_ArrayOfFrames = arr})
        | NotService imp ->

          let dur =
            let durations =
              let durationOfPair (f, i) =
                if i <imp then
                  f.animFrameDuration
                else
                  0.0 in
              let pairs =
                let arrl = Array.to_list arr in
                List.combine arrl (listFromTo 0 (List.length arrl)) in
              List.map durationOfPair pairs in
            List.fold_left (+.) 0.0 durations in
          (d, ShotAnimation {shotAnim_ArrayOfFrames = arr;
                             shotAnim_FrameOfImpact = imp;
                             shotAnim_TimeFromOpeningToImpact = dur})
      in
      List.map animOfDir  dirs
    in
    let addItemToMap (aName, a) m =
      StringMap.add aName a m in
    accumulate ~list:animations ~f:addItemToMap ~state:StringMap.empty in

  { o3d_curFrameIdx = 0;
    o3d_curAnimName = initialAnim;
    o3d_animations = aMap;
    o3d_animState = NotAnimated;
    o3d_visible = true
  }
