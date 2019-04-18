#pragma once
#include "R_ext/libextern.h"
#include <R.h>
#include <Rinternals.h>


SEXP testFunc(SEXP x);
SEXP createSharedMemory(SEXP R_x, SEXP R_type, SEXP R_total_size,SEXP R_pid);
SEXP readSharedMemory(SEXP R_DID);
SEXP getValue_32(SEXP data, SEXP type_id, SEXP i);
SEXP clearAll(SEXP output);
SEXP clearObj(SEXP objID);


//altrep
SEXP createAltrep(SEXP R_address, SEXP R_type, SEXP R_length, SEXP R_size);

SEXP R_getDataCount();

SEXP R_getFreedKeys();

SEXP R_getProcessInfo();
SEXP R_getDataInfo(SEXP R_pid);