#include <string>
#include "Rcpp.h"
#include "altrep.h"
#include "altrepMacro.h"
#include "utils.h"
#include "sharedMemory.h"
using std::string;
using namespace Rcpp;

Rboolean sharedVector_Inspect(SEXP x, int pre, int deep, int pvec,
							  void (*inspect_subtree)(SEXP, int, int, int))
{
	Rprintf(" (len=%llu, type=%d, COW=%d, SC=%d, SS=%d) shared object ID:%s\n", (unsigned long long)Rf_xlength(x), TYPEOF(x),
			Rcpp::as<int>(GET_ALT_SLOT(x, INFO_COPYONWRITE)),
			Rcpp::as<int>(GET_ALT_SLOT(x, INFO_SHAREDCOPY)),
			Rcpp::as<int>(GET_ALT_SLOT(x, INFO_SHAREDSUBSET)),
			Rcpp::as<string>(GET_ALT_SLOT(x, INFO_DATAID)).c_str());
	return TRUE;
}

R_xlen_t sharedVector_length(SEXP x)
{
	R_xlen_t size = Rcpp::as<R_xlen_t>(GET_ALT_SLOT(x, INFO_LENGTH));
	altrepPrint("accessing length:%llu\n", size);
	return size;
}

void *sharedVector_dataptr(SEXP x, Rboolean writeable)
{
	altrepPrint("accessing data pointer\n");
	return ALT_EXTPTR(x);
}
const void *sharedVector_dataptr_or_null(SEXP x)
{
	altrepPrint("accessing data pointer or null\n");
	return sharedVector_dataptr(x, Rboolean::TRUE);
}

SEXP sharedVector_duplicate(SEXP x, Rboolean deep)
{
	bool copyOnWrite = as<bool>(GET_ALT_SLOT(x, INFO_COPYONWRITE));
	bool sharedSubset = as<bool>(GET_ALT_SLOT(x, INFO_SHAREDSUBSET));
	bool sharedCopy = as<bool>(GET_ALT_SLOT(x, INFO_SHAREDCOPY));
	altrepPrint("Duplicating data, deep: %d, copy on write: %d, shared copy %d\n",
				deep, copyOnWrite, sharedCopy);
	if (copyOnWrite)
	{
		if (sharedCopy)
		{
			List newDataInfo = Rf_duplicate(ALT_DATAINFO(x));
			SEXP result = createSharedObjectFromSource(x,
														 copyOnWrite,
														 sharedSubset,
														 sharedCopy);
			return (result);
		}
		else
		{
			PROTECT_GUARD guard;
			SEXP result = guard.protect(Rf_allocVector(TYPEOF(x), XLENGTH(x)));
			memcpy(getWritableDataPtr(result), DATAPTR_RO(x), as<R_xlen_t>(GET_ALT_SLOT(x, INFO_TOTALSIZE)));
			return result;
		}
	}
	else
	{
		List newDataInfo = Rf_duplicate(ALT_DATAINFO(x));
		SEXP result = readSharedObject(newDataInfo);
		return (result);
	}
}

SEXP sharedVector_serialized_state(SEXP x)
{
	altrepPrint("serialize state\n");
	//We check the memory before serialize the object
	std::string id = Rcpp::as<std::string>(GET_ALT_SLOT(x, INFO_DATAID));
	if (!hasSharedMemory(id))
	{
		Rf_warning("The shared memory does not exist(id: %s), the unshared data will be exported instead\n", id.c_str());
		PROTECT_GUARD guard;
		SEXP unsharedData = guard.protect(Rf_allocVector(TYPEOF(x), XLENGTH(x)));
		memcpy(getWritableDataPtr(unsharedData), DATAPTR_RO(x), getObjectSize(x));
		return unsharedData;
	}else{
		return (ALT_DATAINFO(x));
	}
}

SEXP sharedVector_unserialize(SEXP R_class, SEXP dataInfo)
{
	altrepPrint("unserializing data\n");
	if(TYPEOF(dataInfo) != VECSXP){
		Rf_warning("The shared memory has been released! Receiving unshared object.\n");
		return dataInfo;
	}else{
		SEXP result;
		try
		{
			//loadLibrary();
			altrepPrint("Library loaded\n");
			result = readSharedObject(dataInfo);
		}
		catch (const std::exception & ex){
			Rf_warning("Error in unserializing an altrep\n%s", ex.what());
			result = R_NilValue;
		}
		return result;
	}
}

SEXP sharedVector_subset(SEXP x, SEXP indx, SEXP call)
{
	if (TYPEOF(indx) != INTSXP && TYPEOF(indx) != REALSXP)
		return NULL;

	// C NULL tells R to run its regular subset implementation. This is not
	// R_NilValue, which would be an actual R NULL result.
	switch (TYPEOF(x))
	{
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case RAWSXP:
		break;
	default:
		return NULL;
	}

	bool copyOnWrite = as<bool>(GET_ALT_SLOT(x, INFO_COPYONWRITE));
	bool sharedSubset = as<bool>(GET_ALT_SLOT(x, INFO_SHAREDSUBSET));
	bool sharedCopy = as<bool>(GET_ALT_SLOT(x, INFO_SHAREDCOPY));
	altrepPrint("Accessing subset, sharedSubset: %d\n", sharedSubset);

	//Allocate the subset vector and assign values
	uint64_t length = Rf_xlength(indx);
	PROTECT_GUARD guard;
	SEXP subVector;
	if (sharedSubset)
	{
		subVector = guard.protect(
			createEmptySharedObject(TYPEOF(x), length,
								copyOnWrite, sharedSubset, sharedCopy));
	}
	else
	{
		subVector = guard.protect(Rf_allocVector(TYPEOF(x), length));
	}
	const size_t typeSize = getTypeSize(TYPEOF(x));
	const char *src_ptr = static_cast<const char *>(DATAPTR_RO(x));
	char *dest_ptr = static_cast<char *>(getWritableDataPtr(subVector));
	const R_xlen_t sourceLength = XLENGTH(x);
	const int *integerIndex = TYPEOF(indx) == INTSXP ? INTEGER_RO(indx) : NULL;
	const double *realIndex = TYPEOF(indx) == REALSXP ? REAL_RO(indx) : NULL;
	for (uint64_t i = 0; i < length; i++)
	{
		R_xlen_t sourceIndex;
		bool validIndex;
		if (integerIndex != NULL)
		{
			int value = integerIndex[i];
			validIndex = value > 0 && value <= sourceLength;
			sourceIndex = validIndex ? static_cast<R_xlen_t>(value) - 1 : 0;
		}
		else
		{
			double value = realIndex[i];
			validIndex = R_FINITE(value) && value >= 1 && value <= sourceLength;
			if (validIndex)
			{
				sourceIndex = static_cast<R_xlen_t>(value - 1);
			}
			else
			{
				sourceIndex = 0;
			}
		}

		if (validIndex)
		{
			memcpy(dest_ptr + i * typeSize, src_ptr + sourceIndex * typeSize, typeSize);
		}
		else
		{
			switch (TYPEOF(x))
			{
			case LGLSXP:
			case INTSXP:
				reinterpret_cast<int *>(dest_ptr)[i] = NA_INTEGER;
				break;
			case REALSXP:
				reinterpret_cast<double *>(dest_ptr)[i] = NA_REAL;
				break;
			case CPLXSXP:
				reinterpret_cast<Rcomplex *>(dest_ptr)[i].r = NA_REAL;
				reinterpret_cast<Rcomplex *>(dest_ptr)[i].i = NA_REAL;
				break;
			case RAWSXP:
				reinterpret_cast<Rbyte *>(dest_ptr)[i] = 0;
				break;
			}
		}
	}
	return subVector;
}
