# add -noassert for speedup
SYSLIBS = -I +camlimages -I +lablGL -I +lablgtk2 -I +sdl
MLLIBS = bigarray.cmxa sdl.cmxa lablgtk.cmxa lablgl.cmxa  sdlmixer.cmxa sdlttf.cmxa unix.cmxa
SOURCES = Math.ml SharedData.ml Objects3D.ml Input.ml Sound.ml BallMovement.ml PlayerData.ml AnimationModule.ml Network.ml HumanPlayerModule.ml ComputerPlayerModule.ml Camera.ml Renderization.ml Options.ml freetennis.ml
TARGET = freetennis
all: freetennis clean

freetennis:
	ocamlopt $(SYSLIBS) -o $(TARGET) $(MLLIBS) $(SOURCES)

clean:
	rm *.cmi *.cmx *.o
