OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLFIND = ocamlfind

SRCDIR = src

OPTIONS = -g -annot -I $(SRCDIR)

PACKAGES = #tsdl

COMPILE = $(OCAMLFIND) $(OCAMLC) #-package $(PACKAGES)
COMPILEOPT = $(OCAMLFIND) $(OCAMLOPT) #-package $(PACKAGES)

LINK =# -linkpkg
LIBS = str.cma graphics.cma unix.cma
# bigarray.cma sdl.cma
LIBOPTS = $(LIBS:.cma=.cmxa)

EXEC = bsp

OBJS = 	$(addprefix $(SRCDIR)/, point.cmo trigo.cmo options.cmo \
	segment.cmo bsp.cmo physic.cmo \
	ennemi.cmo  player.cmo generateur.cmo parse_lab.cmo render.cmo peintre.cmo test.cmo main.cmo)

OPTOBJS = $(OBJS:.cmo=.cmx)
FILESMLI = $(OBJS:.cmo=.mli)
FILESML = $(OBJS:.cmo=.ml)

all: depend $(EXEC)

opt: depend $(EXEC).opt

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

$(EXEC): $(OBJS)
	$(COMPILE) $(OPTIONS) -o $(EXEC) $(LIBS) $(LINK) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(COMPILEOPT) $(OPTIONS) -o $(EXEC) $(LIBOPTS) $(LINK) $(OPTOBJS)

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
