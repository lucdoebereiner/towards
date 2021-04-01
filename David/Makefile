
FFLAGS = -fast -xHost
LIB = -llo -lsndfile -ljack -lpthread -lrt -lasound -lrattle -lrattleGui 
LIBDIR = ${CURDIR}/../../rattle_fortran/lib
INCDIR = ${CURDIR}/../../rattle_fortran/include
GTKFINCDIR = ${CURDIR}/../../rattle_fortran/include/gtkfinclude
GTKLIB = `pkg-config --cflags --libs gtk+-3.0`

.PHONY: sound_text_4 sound_text_5 kuramoto_2_beta test_gui

sound_text_4:
	gfortran -fPIC -march=native -ffast-math -funroll-loops -O3 -ffree-line-length-0 sound_text_4_clean.f90 -o sound_text 

sound_text_5:
	gfortran -fPIC -march=native -ffast-math -funroll-loops -O3 -ffree-line-length-0 -I${INCDIR} -I${GTKFINCDIR} sound_text_5.f90 -o sound_text  -L${LIBDIR} ${GTKLIB} ${LIB} -Wl,-rpath,${LIBDIR} 
