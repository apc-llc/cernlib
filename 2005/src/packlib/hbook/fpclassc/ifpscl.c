/*
 * $Id: ifpscl.c,v 1.5 1997/09/02 13:09:05 mclareni Exp $
 *
 * $Log: ifpscl.c,v $
 * Revision 1.5  1997/09/02 13:09:05  mclareni
 * WINNT correction
 *
 * Revision 1.4  1997/03/14 17:05:26  mclareni
 * WNT mods
 *
 * Revision 1.3  1996/02/21 16:09:13  cernlib
 * Correct a typo
 *
 * Revision 1.2  1996/02/20 16:49:00  cernlib
 * Clean up the code; use elif/else instead of the patchy syntax.
 *
 * Revision 1.1.1.1  1996/01/16 17:08:12  mclareni
 * First import
 *
 */
#include "hbook/pilot.h"
/*CMZ :          17/05/95  17.23.35  by  Julian Bunn*/
/*-- Author :    Francois Dardare   03/03/95*/
/******************************************************************************
 *                                                                            *
 * NAME:                                                                      *
 *                                                                            *
 *     ifpscl() - floating-point operand classification FORTRAN               *
 *               callable function.                                           *
 *                                                                            *
 * SYNOPSIS:                                                                  *
 *                                                                            *
 *     I = IFPSCL(X)                                                          *
 *                                                                            *
 * DESCRIPTION:                                                               *
 *                                                                            *
 *     ifpscl() returns a non-negative integer value that specifies           *
 *     the IEEE operand class to which the argument x belongs. The            *
 *     value returned are defined as follow:                                  *
 *                                                                            *
 *               0  =  Positive normalized                                    *
 *               1  =  Negative normalized                                    *
 *               2  =  Positive zero                                          *
 *               3  =  Negative zero                                          *
 *               4  =  Positive infinity                                      *
 *               5  =  Negative infinity                                      *
 *               6  =  Positive denormalized                                  *
 *               7  =  Negative denormalized                                  *
 *               8  =  Signalling NaN                                         *
 *               9  =  Quiet NaN                                              *
 *                                                                            *
 ******************************************************************************/
#if defined(CERNLIB_DECS) || defined(CERNLIB_SGI) || defined(CERNLIB_ALPHA_OSF)
#include <fp_class.h>
#elif defined(CERNLIB_HPUX)
#include <math.h>
#elif defined(CERNLIB_IBMRT)
#include <math.h>
#include <float.h>
#elif (defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))
#include <floatingpoint.h>
#elif defined(CERNLIB_SOLARIS)
#include <ieeefp.h>
#endif
 
#if defined(CERNLIB_UNIX) && defined(CERNLIB_QX_SC)
#define ifpscl ifpscl_
#elif defined(CERNLIB_QXCAPT)
#define ifpscl type_of_call IFPSCL
#elif defined(CERNLIB_CRAY)
#define ifpscl IFPSCL
#elif defined(CERNLIB_IBM)
#pragma linkage (ifpscl ,FORTRAN)
#endif
 
int ifpscl(x)
float *x;
{
#if defined(CERNLIB_DECS) && ! defined(CERNLIB_ALPHA_OSF)
int i;
i=fp_class_d(*x);
switch(i)
   {
   case 0 :
      return 8;
   case 1 :
      return 9;
   case 2 :
      return 4;
   case 3 :
      return 5;
   case 4 :
      return 0;
   case 5 :
      return 1;
   case 6 :
      return 6;
   case 7 :
      return 7;
   case 8 :
      return 2;
   case 9 :
      return 3;
   }
}
#elif defined(CERNLIB_ALPHA_OSF)
int i;
i=fp_classf(*x);
switch(i)
   {
   case FP_SNAN :
      return 8;
   case FP_QNAN :
      return 9;
   case FP_POS_INF :
      return 4;
   case FP_NEG_INF :
      return 5;
   case FP_POS_NORM :
      return 0;
   case FP_NEG_NORM :
      return 1;
   case FP_POS_DENORM :
      return 6;
   case FP_NEG_DENORM :
      return 7;
   case FP_POS_ZERO :
      return 2;
   case FP_NEG_ZERO :
      return 3;
   }
}
#elif defined(CERNLIB_HPUX)
return fpclassifyf(*x);
}
#elif defined(CERNLIB_IBMRT)
int i;
i = class(*x);
switch(i)
  {
   case FP_PLUS_NORM :
      return 0;
   case FP_MINUS_NORM :
      return 1;
   case FP_PLUS_DENORM :
      return 6;
   case FP_MINUS_DENORM :
      return 7;
   case FP_PLUS_ZERO:
      return 2;
   case FP_MINUS_ZERO:
      return 3;
   case FP_PLUS_INF :
      return 4;
   case FP_MINUS_INF :
      return 5;
   case 8 :
      return 8;
   case 9 :
      return 9;
  }
}
#elif defined(CERNLIB_SGI)
int i;
i=fp_class_f(*x);
switch(i)
   {
   case 0 :
      return 8;
   case 1 :
      return 9;
   case 2 :
      return 4;
   case 3 :
      return 5;
   case 4 :
      return 0;
   case 5 :
      return 1;
   case 6 :
      return 6;
   case 7 :
      return 7;
   case 8 :
      return 2;
   case 9 :
      return 3;
   }
}
#elif defined(CERNLIB_SUN) && !defined(CERNLIB_SOLARIS)
int i;
i=fp_class(*x);
switch(i)
   {
   case 0:
      if (signbit(*x)==1) {
         return 3;
      } else {
         return 2;
      }
   case 1:
      if (*x<0) {
         return 7;
      } else {
         return 6;
      }
   case 2:
      if (*x<0) {
         return 1;
      } else {
         return 0;
      }
   case 3:
      if (*x<0) {
         return 5;
      } else {
         return 4;
      }
   case 4:
      return 9;
   case 5:
      return 8;
  }
}
#elif defined(CERNLIB_SOLARIS)
fpclass_t i;
i=fpclass(*x);
switch(i)
   {
   case FP_SNAN:
      return 8;
   case FP_QNAN  :
      return 9;
   case FP_NINF :
      return 5;
   case FP_PINF :
      return 4;
   case  FP_NDENORM:
      return 7;
   case  FP_PDENORM:
      return 6;
   case  FP_NZERO :
      return 3;
   case FP_PZERO :
      return 2;
   case  FP_NNORM:
      return 1;
   case FP_PNORM :
      return 0;
   }
}
#else
if (*x<0)  return 1;
if (*x==0) return 2;
if (*x>0)  return 0;
return 9;
}
#endif
 
 
 
