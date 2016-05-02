OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind

SRCDIR = src

OPTIONS = -g -annot -I $(SRCDIR)

PACKAGES = # sdl

COMPILE = $(OCAMLFIND) $(OCAMLC) # -package $(PACKAGES)
COMPILEOPT = $(OCAMLFIND) $(OCAMLOPT) # -package $(PACKAGES)

LIBS = str.cma graphics.cma
# bigarray.cma sdl.cma
LIBOPTS = $(LIBS:.cma=.cmxa)

EXEC = bsp

OBJS = 	$(addprefix $(SRCDIR)/, trigo.cmo options.cmo point.cmo \
	segment.cmo bsp.cmo physic.cmo \
	player.cmo parse_lab.cmo render.cmo main.cmo)

OPTOBJS = $(OBJS:.cmo=.cmx)
FILESMLI = $(OBJS:.cmo=.mli)
FILESML = $(OBJS:.cmo=.ml)

all: depend $(EXEC)

opt: depend $(EXEC).opt

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

$(EXEC): $(OBJS)
	$(COMPILE) $(OPTIONS) -o $(EXEC) $(LIBS) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(COMPILEOPT) $(OPTIONS) -o $(EXEC) $(LIBOPTS) $(OPTOBJS)

clean:
	rm -f $(addprefix $(SRCDIR)/, *.cm[iox] *~ .*~ *.o *.annot .depend)
	rm -f $(EXEC) 
	rm -f $(EXEC).opt

doclatex:
	ocamldoc -I src/ -latex -o sujet/documentationter.tex \
	$(filter-out %.mll %.mly, $(FILESML)) $(FILESMLI)

dochtml:
	mkdir -p doc/ocamldoc/html
	ocamldoc -I src/ -html -d doc/ocamldoc/html \
	$(filter-out %.mll %.mly, $(FILESML)) $(FILESMLI)


.SUFFIXES: .ml .mli .cmo .cmi .mll

.ml.cmo:
	$(COMPILE) $(OPTIONS) -c $<

.ml.cmx:
	$(COMPILEOPT) $(OPTIONS) -c $<

.cmo.cmi:
	$(COMPILE) $(OPTIONS) -c $<

.mli.cmi:
	$(COMPILE) $(OPTIONS) -c $<

.mll.ml:
	ocamllex $<

$(SRCDIR)/.depend:
	$(OCAMLDEP) $(FILESMLI) $(FILESML) > $(SRCDIR)/.depend

depend:
	$(OCAMLDEP) $(FILESMLI) $(FILESML) > $(SRCDIR)/.depend

-include $(SRCDIR)/.depend
