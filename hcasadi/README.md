hcasadi is a set of bindings for the C++ project CasADi

# INSTALLATION

THIS WILL NOT BUILD OUT OF THE BOX. My cabal build foo is weak where ffi is concerned and some things are hard-coded to my local installation paths. Also you have to figure out how to build the CasADi project. Ping me if you would like help.

Currently you must edit hopt-o-mex.cabal and replace "/home/greg/hcasadi" with wherever you put it. Fixing this is high on the todo list.


## HASKELL BINDINGS FOR CASADI:
To build the CasADi bindings you need to install CasADi yourself, make a CASADI environment variable, then

    cd hcasadi/hcasadi_cppsrc
    make

You will need to go into casadi/CMakeLists.txt and set "WITH_JIT" to ON in order to use the SXFunction code-generation capabilities


## IPOPT
Install ipopt on your own - their website has good instructions. Then you may have to append /usr/local/lib/coin and /usr/local/lib/coin/ThirdParty to your LD_LIBRARY_PATH env var.

When you cabal configure it will warn you about missing ipopt/coinmumps/coinmetis libraries - you can ignore this.

You might just want to comment out the whole ipoptTest executable in hcasadi.cabal if you are having problems.


## SNOPT
add the SNOPT environment variable
