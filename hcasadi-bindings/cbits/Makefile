# Hopt-o-mex casadi interface makefile
# Greg Horn 2011

Q = @
CXX = g++
#CXX = gcc

LIB = libcasadi_interface.a

SRC = \
	sxMatMath.cpp \
	sxmUnary.cpp \
	sxmBinary.cpp
#	codegen.cpp \
#	adInterface.cpp \
#	dMatrixInterface.cpp \
#	sxMatMisc.cpp \
#	sxMatrixInterface.cpp \
#	sxFunctionInterface.cpp \
#	ipoptSolverInterface.cpp

#ifdef SNOPT
#	SRC += \
#		snoptSolverInterface.cpp \
#		SnoptSolver.cpp
#endif

OBJ = $(SRC:%.cpp=%.o)
INCLUDES = -I$(CASADI) -I$(SNOPT) -I$(SNOPT)/include
LDFLAGS =
FLAGS = -O2 #-Wall -Wextra -Wshadow #-Werror #-g
FLAGS += -DCOUT_MEMORY_MANAGEMENT

.PHONY: clean

$(LIB): $(OBJ)
	@echo AR $@
	$(Q)ar rcs $(LIB) $(OBJ) $(LDFLAGS)

%.o : %.cpp
	@echo CXX $@
	$(Q)$(CXX) $(FLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f $(LIB)
	rm -f $(OBJ)
