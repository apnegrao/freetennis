# add -noassert for speedup
all: freetennis.ml 
	ocamlopt   -I +camlimages  -I +lablGL -I +lablgtk2    -I +sdl -o freetennis  bigarray.cmxa sdl.cmxa lablgtk.cmxa lablgl.cmxa  ci_core.cmxa  sdlmixer.cmxa sdlttf.cmxa unix.cmxa freetennis.ml
