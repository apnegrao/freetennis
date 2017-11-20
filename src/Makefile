# add -noassert for speedup
SYSLIBS = -I +camlimages -I +lablGL -I +lablgtk2 -I +sdl
OCAMLLIBS = bigarray.cmxa sdl.cmxa lablgtk.cmxa lablgl.cmxa sdlmixer.cmxa sdlttf.cmxa unix.cmxa
SOURCES = Sound.ml Network.ml Math.ml SharedData.ml Camera.ml Input.ml Objects3D.ml Animation.ml BallMovement.ml PlayerData.ml Options.ml Renderization.ml HumanPlayer.ml ComputerPlayer.ml freetennis.ml
TARGET = freetennis
all: freetennis clean

freetennis:
	ocamlopt $(SYSLIBS) -o $(TARGET) $(OCAMLLIBS) $(SOURCES)

clean:
	rm *.cmi *cmx *.o
