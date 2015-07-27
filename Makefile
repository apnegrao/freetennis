# add -noassert for speedup
SYSLIBS = -I +camlimages -I +lablGL -I +lablgtk2 -I +sdl
MLLIBS = bigarray.cmxa sdl.cmxa lablgtk.cmxa lablgl.cmxa  sdlmixer.cmxa sdlttf.cmxa unix.cmxa
SRCDIR=src
SRC=$(wildcard $(SRCDIR)/*.ml)
SOURCES = Math.ml SharedData.ml Objects3D.ml Input.ml Sound.ml BallMovement.ml PlayerData.ml AnimationModule.ml HumanPlayerModule.ml ComputerPlayerModule.ml Camera.ml Renderization.ml freetennis.ml
TARGET = freetennis
all:
	cd $(SRCDIR);make;mv $(TARGET) .. 

original: freetennis 

freetennis:
	ocamlopt $(SYSLIBS) -o $(TARGET) $(MLLIBS) freetennis_original.ml

clean:
	rm *.cmi *.cmx *.o
