# Hopt-o-mex casadi interface makefile
# Greg Horn 2011

Q = @
CXX = g++

LIB = libcasadi_interface.a

SRC = \
	sxInterface.cpp \
	sxMatrixInterface.cpp \
	sxFunctionInterface.cpp \
	ipoptSolverInterface.cpp

OBJ = $(SRC:%.cpp=%.o) 
INCLUDES = -I$(CASADI)
LDFLAGS =
FLAGS = -O2 -fPIC #-Wall -Wextra -Wshadow #-Werror #-g

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
