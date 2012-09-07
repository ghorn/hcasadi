#include "math.h"
#include <cstdio>
#include <sstream>
#include <string.h>

#include <casadi/sx/sx.hpp>
#include <casadi/sx/sx_tools.hpp>
#include <casadi/fx/sx_function.hpp>
#include <casadi/matrix/matrix_tools.hpp>
#include <casadi/fx/fx.hpp>

#include "sxFunction.hpp"

//#define COUT_MEMORY_MANAGEMENT

using namespace std;
using namespace CasADi;

/******************** memory management *******************/
void sxFunctionDelete(SXFunction * const fun){
#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) sxFunctionDelete {address: " << fun << "}\n";
#endif
    delete fun;
}

SXFunction * sxFunctionCreate(const SXMatrix * sxIn[], int numInputs, const SXMatrix * sxOut[], int numOutputs){
    vector<SXMatrix> inputs(numInputs);
    vector<SXMatrix> outputs(numOutputs);

    for (int k=0; k<numInputs; k++)
        inputs.at(k) = *(sxIn[k]);

    for (int k=0; k<numOutputs; k++)
        outputs.at(k) = *(sxOut[k]);

    SXFunction * fun = new SXFunction(inputs, outputs);

#ifdef COUT_MEMORY_MANAGEMENT
    cout << "(cpp) sxFunctionCreate {address: " << fun << "}\n";
#endif

    return fun;
}

void sxFunctionInit(FX & fun){
    fun.init();
}

/*************** getters *****************/
int sxFunctionGetNumInputs(const FX & fun){
    return fun.getNumInputs();
}

int sxFunctionGetNumOutputs(const FX & fun){
    return fun.getNumOutputs();
}

void sxFunctionGetInputsSX(const SXFunction & fun, int idx, SXMatrix & mat){
    mat = fun.inputSX(idx);
}
void sxFunctionGetOutputsSX(const SXFunction & fun, int idx, SXMatrix & mat){
    mat = fun.outputSX(idx);
}

int sxFunctionInputSize( int idx, const SXFunction & fun ){
    return fun.input(idx).size();
}
int sxFunctionInputSize1( int idx, const SXFunction & fun ){
    return fun.input(idx).size1();
}
int sxFunctionInputSize2( int idx, const SXFunction & fun ){
    return fun.input(idx).size2();
}

int sxFunctionOutputSize( int idx, const SXFunction & fun ){
    return fun.output(idx).size();
}
int sxFunctionOutputSize1( int idx, const SXFunction & fun ){
    return fun.output(idx).size1();
}
int sxFunctionOutputSize2( int idx, const SXFunction & fun ){
    return fun.output(idx).size2();
}


/************ evaluate *************/
static void sxFunctionAssertGoodDimensions(int numInputs, const SXMatrix * inputs[],
					   int numOutputs,
					   SXFunction & fun){
    int badDims = 0;

    if (numInputs != fun.getNumInputs()){
        cerr << "(cpp) error: fun.numInputs() = " << fun.getNumInputs() << " but got " << numInputs << endl;
        badDims = 1;
    }

    if (numOutputs != fun.getNumOutputs()){
        cerr << "(cpp) error: fun.numOutputs() = " << fun.getNumOutputs() << " but got " << numOutputs << endl;
        badDims = 1;
    }

    for (int k=0; k<numInputs; k++)
        if ((*(inputs[k])).size1() != fun.inputSX(k).size1()){
            cerr << "(cpp) error: fun.inputSX(k).size1(): " << fun.inputSX(k).size1() << " but got " << (*(inputs[k])).size1() << endl;
            badDims = 1;
        }
    for (int k=0; k<numInputs; k++)
        if ((*(inputs[k])).size2() != fun.inputSX(k).size2()){
            cerr << "(cpp) error: fun.inputSX(k).size2(): " << fun.inputSX(k).size2() << " but got " << (*(inputs[k])).size2() << endl;
            badDims = 1;
        }

    if (badDims)
        throw 1;
}

int sxFunctionGetOutput ( const int outputIdx, const int valSize, double val[], FX & fun ) {
    if (fun.output(outputIdx).size() != valSize)
        return fun.output(outputIdx).size();
    fun.getOutput( val, outputIdx );
    return -1;
}

int sxFunctionSetInput ( const int inputIdx, const int valSize, const double val[], FX & fun ) {
    if (fun.input(inputIdx).size() != valSize)
        return fun.input(inputIdx).size();
    fun.setInput( val, inputIdx );
    return -1;
}

int sxFunctionEvalDouble( const int numInputs, const double * inputs[], const int inputSizes[],
                          const int numOutputs, double * outputs[], const int outputSizes[],
                          FX & fun ){
    if (numInputs != fun.getNumInputs()){
        cerr << "(cpp) sxFunctionEvaluateDouble got numInputs: " << numInputs
             << " but fun.getNumInputs() is: " << fun.getNumInputs() << endl;
        return 1;
    }
    if (numOutputs != fun.getNumOutputs()){
        cerr << "(cpp) sxFunctionEvaluateDouble got numOutputs: " << numOutputs
             << " but fun.getNumOutputs() is: " << fun.getNumOutputs() << endl;
        return 2;
    }

    for (int k=0; k<numInputs; k++){
        if (fun.input(k).size() == inputSizes[k])
            fun.setInput( inputs[k], k );
        else {
            cerr << "(cpp) sxFunctionEvaluateDouble got fun.input(" << k << ").size(): " << fun.input(k).size()
                 << "but inputSizes[" << k << "] is: " << inputSizes[k] << endl;
            return 3;
        }
    }

    fun.evaluate();

    for (int k=0; k<numOutputs; k++){
        if (fun.output(k).size() == outputSizes[k])
            fun.getOutput( outputs[k], k );
        else {
            cerr << "(cpp) sxFunctionEvaluateDouble got fun.output(" << k << ").size(): " << fun.output(k).size()
                 << "but outputSizes[" << k << "] is: " << outputSizes[k] << endl;
            return 4;
        }
    }
}

void sxFunctionEvaluateSXMatrix(int numInputs, const SXMatrix * inputs[],
				int numOutputs, SXMatrix * outputs[],
				SXFunction & fun){
    sxFunctionAssertGoodDimensions( numInputs, inputs, numOutputs, fun );

    vector<SXMatrix> inputVec;
    for (int k=0; k<numInputs; k++)
        inputVec.push_back( *(inputs[k]) );

    for (int k=0; k<numInputs; k++)
        makeDense( inputVec.at(k) );

    vector<SXMatrix> outputVec( fun.eval(inputVec) );

    for (int k=0; k<numOutputs; k++)
        *(outputs[k]) = outputVec.at(k);
}

// set options (unsafe!!)
void sxFunctionSetOptionDouble(const char name[], const double val, SXFunction & fun){
    fun.setOption(name, val);
}
void sxFunctionSetOptionString(const char name[], const char val[], SXFunction & fun){
    fun.setOption(name, val);
}
void sxFunctionSetOptionStringList( const char name[],
                                    const int numStrings, const char * const val[],
                                    SXFunction & fun )
{
    vector<string> strings;
    for (int k=0; k<numStrings; k++)
        strings.push_back( string(val[k]) );
    fun.setOption(name, strings);
}
void sxFunctionSetOptionInt(const char name[], const int val, SXFunction & fun){
    fun.setOption(name, val);
}
void sxFunctionSetOptionBool(const char name[], const int val, SXFunction & fun){
    if (val) fun.setOption(name, true);
    else     fun.setOption(name, false);
}
