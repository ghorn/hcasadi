# Hopt-o-mex casadi interface makefile
# Greg Horn 2011

PROJ = casadiInterface

Q = @
CXX = g++

CPP_SRC = \
	sxInterface.cpp \
	sxMatrixInterface.cpp \
	sxFunctionInterface.cpp

CPP_OBJ = $(CPP_SRC:%.cpp=%.o) 
CPP_INCLUDES = -I$(CASADI)
CPP_LDFLAGS =
CPP_LIB = libcasadi_interface.a
CPP_FLAGS = -O2 -fPIC #-Wall -Wextra -Wshadow #-Werror #-g


HS_SRC = \
	SX.hs \
	SXMatrix.hs \
	CasadiInterfaceUtils.hs
#	SXFunction.hs \

HS_OBJ = $(HS_SRC:.hs=.o) 
HS_HI = $(HS_SRC:.hs=.hi) 
HS_INCLUDES =
HS_LDFLAGS = \
	-L./ \
	-lcasadi_interface \
	-L$(CASADI)/build/lib \
	-lcasadi

HS_LIB = libcasadi_interface.a
HS_FLAGS = -pgml g++ -O2 --make -fforce-recomp #-Wall #-Werror #-g


.PHONY: clean

$(PROJ): $(HS_SRC) $(CPP_LIB)
	@echo GHC $@
	$(Q)ghc -o $(PROJ) $(HS_FLAGS) $(HS_LDFLAGS) $(HS_SRC)

%.o : %.cpp
	@echo CXX $@
	$(Q)$(CXX) $(CPP_FLAGS) $(CPP_INCLUDES) -c $< -o $@

$(CPP_LIB): $(CPP_OBJ)
	@echo AR $@
	$(Q)ar rcs $(CPP_LIB) $(CPP_OBJ) $(CPP_LDFLAGS)

cppclean:
	rm -f $(CPP_LIB)
	rm -f $(CPP_OBJ)

hsclean:
	rm -f $(HS_OBJ)
	rm -f $(HS_HI)
	rm -f $(PROJ)

clean: cppclean hsclean
