# add -noassert for speedup

CMXA = bigarray.cmxa sdl.cmxa lablgtk.cmxa lablgl.cmxa graphics.cmxa sdlmixer.cmxa sdlttf.cmxa unix.cmxa
CMA  = $(CMXA:.cmxa=.cma)

OFLAGS = -I +camlimages -I +lablGL -I +lablgtk2 -I +sdl -w Aelzy
BFLAGS = $(OFLAGS) -custom

OCAMLC=ocamlc
OCAMLOPT=ocamlopt

NAME=freetennis

$(NAME).opt:
	$(OCAMLOPT) $(OFLAGS) -o $(NAME) $(CMXA) $(NAME).ml

$(NAME).byte:
	$(OCAMLC) $(BFLAGS) -o $(NAME) $(CMA) $(NAME).ml

clean:
	rm -f *.cm[oixa] *.cmxa *.o *.a *~ $(NAME).opt $(NAME).byte
