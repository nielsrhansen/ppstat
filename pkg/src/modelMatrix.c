/*  Routines for computing the model matrix for the ppstat package.  
 *  These are intended for use with R.
 *
 *     Copyright (C) 2010 Niels Richard Hansen.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * These functions are distributed in the hope that they will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 */

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void F77_NAME(sgram)(double *sg0, double *sg1, double *sg2,double *sg3, double *tb, int *nb);

SEXP computeFilterMatrix(SEXP t, SEXP B, SEXP delta, SEXP s, SEXP zero, SEXP type);

static const R_FortranMethodDef FortEntries[] = {
  {"sgram", (DL_FUNC) &F77_SUB(sgram), 6},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
  {"computeFilterMatrix", (DL_FUNC) &computeFilterMatrix, 6},
  {NULL, NULL, 0}
};


void  R_init_ppstat(DllInfo *info)
{ 
  R_registerRoutines(info, NULL, CallEntries, FortEntries, NULL);
}

SEXP computePointProcessFilterMatrix(SEXP t, SEXP B, SEXP delta, SEXP s, SEXP zero){

  /*  We assume that t and s are in increasing order 
   *  t:     time points where filter matrix is evaluated
   *  B:     matrix of basis function evaluations
   *  delta: distance between basis function evaluations in B
             (they are assumed equidistant)
   *  s:     observed points
   */

  int i, j, nt, ns, nss, *nB, lookupIndex, entry, col;
  double *xt, *xs, *xB, *xZ, d, w, diff, antip, target;
  SEXP Z, BDIM;

  if(!isMatrix(B)) error("B must be a matrix when using computePointProcessFilterMatrix");
 
  nt = length(t);
  BDIM = getAttrib(B, R_DimSymbol);
  PROTECT(BDIM = coerceVector(BDIM,INTSXP));
  nB = INTEGER(BDIM);
  PROTECT(Z = allocMatrix(REALSXP,nt,nB[1]));
  PROTECT(t = coerceVector(t,REALSXP));
  PROTECT(B = coerceVector(B,REALSXP));
  PROTECT(delta = coerceVector(delta,REALSXP));
  PROTECT(s = coerceVector(s,REALSXP));
  PROTECT(zero = coerceVector(zero,REALSXP));
  xt = REAL(t);
  xB = REAL(B);
  xs = REAL(s);
  xZ = REAL(Z);

  d = REAL(delta)[0];
 
  antip = d*REAL(zero)[0];
  w = d*(nB[0]-1);
  
  for(j = 0; j < nB[1]; j++) {
    ns = 0;
    col =  nB[0]*j;
    for(i = 0; i < nt; i++) {
      entry = i + nt*j;
      xZ[entry] = 0;
      target = xt[i] + antip;
      while(ns < length(s)-1 && target > xs[ns+1]) ns++;
      nss = ns;
      diff = target - xs[ns];
      if(diff > 0) {
	while(diff <= w) 
	  { 
	    lookupIndex = floor(diff/d + 0.5);
	    xZ[entry] += xB[lookupIndex + col];
	    ns--;
	    if(ns < 0) break;
	    diff = target - xs[ns];
	  }
      }
      ns = nss;
    }
  }

  UNPROTECT(7);
  return(Z);
}

SEXP computeContinuousProcessFilterMatrix(SEXP t, SEXP B, SEXP delta, SEXP s, SEXP zero){

  /*  We assume that t and s are in increasing order
   *  t:     time points where filter matrix is evaluated
   *  B:     matrix of basis function evaluations
   *  delta: distance between basis function evaluations in B
             (they are assumed equidistant)
   *  s:     observed values
   */

  int i, j, k, nt, *nB, lookupIndexLeft, lookupIndexRight, entry, col;
  double *xt, *xs, *xB, *xZ, *BB, d;
  SEXP Z, BDIM;

  if(!isMatrix(B)) error("B must be a matrix when using computeModelMatrix.");
  if(length(t) != length(s)) error("Lengths of vectors of observed values and observation points differ.");

  nt = length(t);
  BDIM = getAttrib(B, R_DimSymbol);
  PROTECT(BDIM = coerceVector(BDIM,INTSXP));
  nB = INTEGER(BDIM);
  PROTECT(Z = allocMatrix(REALSXP,nt,nB[1]));
  PROTECT(t = coerceVector(t,REALSXP));
  PROTECT(B = coerceVector(B,REALSXP));
  PROTECT(delta = coerceVector(delta,REALSXP));
  PROTECT(s = coerceVector(s,REALSXP));
  xt = REAL(t);
  xB = REAL(B);
  xs = REAL(s);
  xZ = REAL(Z);

  BB = (double *) R_alloc(nB[0]+1, sizeof(double));

  d = REAL(delta)[0];
  
  for(j = 0; j < nB[1]; j++) {
    col =  nB[0]*j;
    xZ[nt*j] = 0;
    // Integration of basis functions
    BB[nB[0]] = 0; 
    for(i = nB[0]-1; i >= 0; i--) 
      BB[i] = BB[i+1] + d*xB[i + col];
      
    // Computation of filter basis 
    for(i = 1; i < nt; i++) {
      entry = i + nt*j; 
      k = i-1;
      xZ[entry] = 0;
      lookupIndexRight = 0;
      while(k >= 0 && (lookupIndexLeft = floor((xt[i] - xt[k])/d + 0.5)) <= nB[0])
	{ 
	  xZ[entry] += (BB[lookupIndexRight] - BB[lookupIndexLeft])*xs[k];
	  lookupIndexRight = lookupIndexLeft;
	  k--;
	}
    }
   
    // Boundary
    i = 0;
    while(i < nt && (k = floor((xt[i] - xt[0])/d + 0.5)) < nB[0]) 
      {
	xZ[i + nt*j] += BB[k]*xs[0];
	i++;
      }
  }
  
  UNPROTECT(6);
  return(Z);
}

SEXP computeFilterMatrix(SEXP t, SEXP B, SEXP delta, SEXP s, SEXP zero, SEXP type){
  SEXP Z;
  PROTECT(type = coerceVector(type,STRSXP));
  switch(*CHAR(STRING_ELT(type,0)))
      {
      case 'p':
	PROTECT(Z = computePointProcessFilterMatrix(t, B, delta, s, zero));
	break;
      case 'c':
	PROTECT(Z = computeContinuousProcessFilterMatrix(t, B, delta, s, zero));
	break;
      default:
	PROTECT(Z = allocMatrix(REALSXP,1,1));
      }
  UNPROTECT(2);
  return(Z);
}
