(** --- Consts --- **)
let sfxDir = "sfx" (* where the wav files are located.
                      		      example of customization: /usr/share/freetennis/sounds *)

(** --- Data Types --- **)
type soundId = SoundAhh | SoundHff | SoundNormalShot | SoundLightShot | 
    SoundSprintCantBegin | SoundSprintJustBegun | SoundSprintJustFinished | 
    SoundFault | SoundBounce | SoundHitNet | SoundHitBorder

type sounds = {
  sou_normalShot: Sdlmixer.chunk;
  sou_lightShot: Sdlmixer.chunk; 
  sou_fault: Sdlmixer.chunk;
  sou_hitBorder: Sdlmixer.chunk;
  sou_hitNet: Sdlmixer.chunk;
  sou_ahh: Sdlmixer.chunk;
  sou_hff: Sdlmixer.chunk;
  sou_sprint: Sdlmixer.chunk;
  sou_sprintCantBeginOutOfStamina: Sdlmixer.chunk;
  (* sou_sprintFinishedOutOfStamina: Sdlmixer.chunk; *)
  sou_bounce: Sdlmixer.chunk
}

(** --- Functions --- **)
let loadSounds = (
  Sdlmixer.open_audio ();
  let stam = Sdlmixer.loadWAV (sfxDir ^ "/fh2.wav") in
  Sdlmixer.setvolume_chunk stam 4.0 ;
  Some {
    sou_normalShot = Sdlmixer.loadWAV (sfxDir ^ "/colpo.wav");
    sou_bounce = Sdlmixer.loadWAV (sfxDir ^ "/palla leggera.wav");
    sou_hitNet = Sdlmixer.loadWAV (sfxDir ^ "/rete.wav");
    sou_hitBorder = Sdlmixer.loadWAV (sfxDir ^ "/muro2.wav");
    sou_fault = Sdlmixer.loadWAV (sfxDir ^ "/out.wav");
    sou_lightShot = Sdlmixer.loadWAV (sfxDir ^ "/fh.wav");
    sou_hff = Sdlmixer.loadWAV (sfxDir ^ "/hff.wav");
    sou_sprint = Sdlmixer.loadWAV (sfxDir ^ "/contrazione2.wav");
    sou_sprintCantBeginOutOfStamina = stam;
    sou_ahh = Sdlmixer.loadWAV (sfxDir ^ "/Ahh.wav") 
  }
)

let playSoundId ~id ~sounds = 
  match sounds with
  | None -> ()
  | Some sou ->
    let playSound s  = 
      try
        Sdlmixer.play_sound s
      with Sdlmixer.SDLmixer_exception _ -> 
        ()
    in
    match id with
    | SoundNormalShot -> playSound sou.sou_normalShot 
    | SoundHff -> playSound sou.sou_hff 
    | SoundSprintJustBegun -> playSound sou.sou_sprint 
    | SoundSprintCantBegin ->
      playSound sou.sou_sprintCantBeginOutOfStamina 
    | SoundSprintJustFinished -> playSound sou.sou_sprintCantBeginOutOfStamina 
    | SoundAhh -> playSound sou.sou_ahh 
    | SoundLightShot -> playSound sou.sou_lightShot 
    | SoundFault -> playSound sou.sou_fault
    | SoundBounce -> playSound sou.sou_bounce
    | SoundHitNet -> playSound sou.sou_hitNet
    | SoundHitBorder -> playSound sou.sou_hitBorder
