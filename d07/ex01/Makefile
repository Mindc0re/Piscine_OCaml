NAME = ex01

SOURCES = people.ml doctor.ml main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

all: depend $(NAME)

$(NAME): opt byt
	ln -sf $(NAME).opt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~
	rm -f *.o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

depend: .depend
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all
	
include .depend
