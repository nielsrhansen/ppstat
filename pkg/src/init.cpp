#include <R_ext/Rdynload.h>

// Configuration of SglOptimizer
// Debugging
#ifndef NDEBUG
#define SGL_DEBUG
#endif

// Runtime checking for numerical problems
#define SGL_RUNTIME_CHECKS

// Check dimension of input objects
#define SGL_DIM_CHECKS

// Converges checks
#define SGL_CONVERGENCE_CHECK

// Exception handling
#define SGL_CATCH_EXCEPTIONS

// Should the timers be activated (only needed for profiling the code)
//#define SGL_TIMING

// Should openmp be used
#ifndef _OPENMP
// No openmp
// openmp (multithreading) not supported on this system - compiling without openmp support
#else
// Use openmp
#define SGL_USE_OPENMP
#endif

#include <sgl.h>

/*********************************
 *
 *  explin sparse module
 *
 *********************************/

// Module name
#define MODULE_NAME explin_sparse

// Objective
#include "explin_objective.h"

#define OBJECTIVE explin_spx
#define DATA sgl::WeightedResponseGroupedMatrixData < sgl::sparse_matrix , sgl::vector >

#include <sgl/RInterface/sgl_lambda_seq.h>
#include <sgl/RInterface/sgl_fit.h>

#define PREDICTOR sgl::LinearPredictor < sgl::sparse_matrix , sgl::LinearResponse >

#include <sgl/RInterface/sgl_predict.h>
#include <sgl/RInterface/sgl_subsampling.h>

/* **********************************
 *
 *  Registration of methods
 *
 ***********************************/

static const R_CallMethodDef CallEntries[] = {
  SGL_LAMBDA(explin_sparse),
  SGL_FIT(explin_sparse),
  {NULL, NULL, 0}
};

void  R_init_ppstat(DllInfo *info)
{ 
  // Print warnings
  #ifndef SGL_OPENMP_SUPP
  Rcout << "SglOptim warning: openmp (multithreading) not supported on this system" << std::endl;
  #endif
  
  #ifdef SGL_DEBUG
  Rcout
  << "SglOptim warning: compiled with debugging on -- this may slow down the runtime of the sgl routines"
  << std::endl;
  #endif
  R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
}