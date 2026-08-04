#include <Rcpp.h>
#include <map>
#include "R_ext/Altrep.h"
#include "sharedMemory.h"
#include "altrep.h"
#include "utils.h"

using namespace Rcpp;
using std::string;

/*
##########################################
## shared object APIs
##########################################
*/
// [[Rcpp::export]]
SEXP C_getDataInfoTemplate()
{
	return getDataInfoTemplate();
}
// [[Rcpp::export]]
SEXP C_getStringDataInfoTemplate()
{
	return getStringDataInfoTemplate();
}
/*
Create new shared object and make it auto
release after use.
*/
// [[Rcpp::export]]
SEXP C_createEmptySharedObject(int type, uint64_t length,
							   bool copyOnWrite, bool sharedSubset, bool sharedCopy,
							   SEXP attributes)
{
	return createEmptySharedObject(type, length,
								   copyOnWrite, sharedSubset, sharedCopy,
								   attributes);
}

// Attributes must be a pairlist
// [[Rcpp::export]]
SEXP C_createSharedObjectFromSource(
	SEXP x, bool copyOnWrite, bool sharedSubset, bool sharedCopy,
	SEXP attributes, bool setS4)
{
	PROTECT_GUARD guard;
	SEXP result = guard.protect(createSharedObjectFromSource(
		x, copyOnWrite, sharedSubset, sharedCopy,
		attributes));
	if (setS4)
		result = Rf_asS4(result, TRUE, 0);
	return result;
}

// copyOnWrite,sharedSubset,sharedCopy will be disabled
// [[Rcpp::export]]
SEXP C_createSharedStringFromSource(SEXP x, bool copyOnWrite, SEXP attributes,
									bool setS4)
{
	PROTECT_GUARD guard;
	SEXP result = guard.protect(createSharedStringFromSource(x, copyOnWrite, attributes));
	if (setS4)
		result = Rf_asS4(result, TRUE, 0);
	return result;
}

// [[Rcpp::export]]
SEXP C_readSharedObject(SEXP dataInfo)
{
	return readSharedObject(dataInfo);
}

// [[Rcpp::export]]
SEXP C_unshare(SEXP x, SEXP attributes)
{
	return unshare(x, attributes);
}
// [[Rcpp::export]]
SEXP C_unshareString(SEXP x, SEXP attributes)
{
	return unshareString(x, attributes);
}

//Function to set the ownership of a shared object
// [[Rcpp::export]]
void C_setSharedObjectOwership(SEXP x, bool ownData)
{
	packagePrint("set owndata :%d\n", ownData);
	SEXP extPtr = R_altrep_data1(x);
	string id = as<string>(R_ExternalPtrTag(extPtr));
	autoReleaseAfterUse(id, ownData);
}
// [[Rcpp::export]]
bool C_getSharedObjectOwership(SEXP x)
{
	packagePrint("get owndata\n");
	SEXP extPtr = R_altrep_data1(x);
	string id = as<string>(R_ExternalPtrTag(extPtr));
	return autoReleaseAfterUse(id);
}
/*
##########################################
## ALTREP related C API
##########################################
*/
// [[Rcpp::export]]
R_xlen_t C_xlength(SEXP x)
{
	return XLENGTH(x);
}

// [[Rcpp::export]]
bool C_ALTREP(SEXP x)
{
	return ALTREP(x);
}

// [[Rcpp::export]]
SEXP C_getAltData1(SEXP x)
{
	return R_altrep_data1(x);
}
// [[Rcpp::export]]
SEXP C_getAltData2(SEXP x)
{
	return R_altrep_data2(x);
}

// [[Rcpp::export]]
bool C_isShared(SEXP x)
{
	if (!ALTREP(x))
		return false;
	bool result;
	int type = TYPEOF(x);
	switch (type)
	{
	case REALSXP:
	case INTSXP:
	case LGLSXP:
	case RAWSXP:
	case CPLXSXP:
	case STRSXP:
		result = R_altrep_inherits(x, getAltClass(type));
		break;
	default:
		result = false;
	}
	//If x is not a shared object, it might be a wrapper.
	if (!result)
	{
		result = C_isShared(R_altrep_data1(x));
	}
	return result;
}

// [[Rcpp::export]]
void C_setAltData1(SEXP x, SEXP data)
{
	R_set_altrep_data1(x, data);
}
// [[Rcpp::export]]
void C_setAltData2(SEXP x, SEXP data)
{
	R_set_altrep_data2(x, data);
}
/*
##########################################
## R SEXP APIs
##########################################
*/
// [[Rcpp::export]]
bool C_ISS4(SEXP x)
{
	return Rf_isS4(x);
}
// [[Rcpp::export]]
SEXP C_setS4(SEXP x, bool flag)
{
	return Rf_asS4(x, flag ? TRUE : FALSE, 0);
}
// [[Rcpp::export]]
bool C_isSameObject(SEXP x, SEXP y)
{
	return ((void *)x) == ((void *)y);
}

// [[Rcpp::export]]
int C_getDataTypeId(string type)
{
	if (type == "raw")
	{
		return RAWSXP;
	}
	if (type == "logical")
	{
		return LGLSXP;
	}
	if (type == "integer")
	{
		return INTSXP;
	}
	if (type == "real"||type=="numeric")
	{
		return REALSXP;
	}
	if (type == "complex")
	{
		return CPLXSXP;
	}
	if (type == "character")
	{
		return STRSXP;
	}
	Rf_error("Cannot find the type id for the type <%s>\n", type.c_str());
	return 0;
}

/*
##########################################
## Export sharedMemory function to R
##########################################
*/
// [[Rcpp::export]]
void C_initialPkgData()
{
	initialPkgData();
}
// [[Rcpp::export]]
void C_releasePkgData()
{
	releasePkgData();
}

// [[Rcpp::export]]
int32_t C_getLastIndex()
{
	return getLastIndex();
}
// [[Rcpp::export]]
string C_allocateSharedMemory(size_t size_in_byte, string name = "")
{
	return allocateSharedMemory(size_in_byte, name);
}
// [[Rcpp::export]]
SEXP C_mapSharedMemory(string id)
{
	return R_MakeExternalPtr(mapSharedMemory(id), R_NilValue, R_NilValue);
}
// [[Rcpp::export]]
void C_unmapSharedMemory(string id)
{
	return unmapSharedMemory(id);
}
// [[Rcpp::export]]
void C_freeSharedMemory(string id)
{
	return freeSharedMemory(id);
}
// [[Rcpp::export]]
bool C_hasSharedMemory(string id)
{
	return hasSharedMemory(id);
}
// [[Rcpp::export]]
uint64_t C_getSharedMemorySize(string id)
{
	return getSharedMemorySize(id);
}

// [[Rcpp::export]]
string C_getSharedMemoryPath()
{
	return getSharedMemoryPath();
}
