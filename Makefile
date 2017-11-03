# add -noassert for speedup
SYSLIBS = -I +camlimages -I +lablGL -I +lablgtk2 -I +sdl
MLLIBS = bigarray.cmxa sdl.cmxa lablgtk.cmxa lablgl.cmxa sdlmixer.cmxa sdlttf.cmxa unix.cmxa
SRCDIR=src
SRC=$(wildcard $(SRCDIR)/*.ml)
TARGET = freetennis
all:
	cd $(SRCDIR);make;mv $(TARGET) ..

clean:
	rm *.cmi *.cmx *.o
