/*
 * $Id: ifp.c,v 1.4 1998/09/25 09:27:10 mclareni Exp $
 *
 * $Log: ifp.c,v $
 * Revision 1.4  1998/09/25 09:27:10  mclareni
 * Modifications for the Mklinux port flagged by CERNLIB_PPC
 *
 * Revision 1.3  1997/03/14 17:05:25  mclareni
 * WNT mods
 *
 * Revision 1.2  1996/09/20 14:27:07  cernlib
 * add Linux
 *
 * Revision 1.1.1.1  1996/01/16 17:08:12  mclareni
 * First import
 *
 */
#include "hbook/pilot.h"
/*CMZ :          26/06/95  14.06.04  by  Julian Bunn*/
/*-- Author :    Francois Dardare   15/03/95*/
#if ( defined(CERNLIB_SUN)||defined(CERNLIB_SOLARIS)||defined(CERNLIB_SGI)\
     ||defined(CERNLIB_DECS)||defined(CERNLIB_ALPHA)||defined(CERNLIB_HPUX)\
     ||defined(CERNLIB_IBMRT)||defined(CERNLIB_LINUX))\
    &&(!defined(CERNLIB_VAX))
#define ifps ifps_
#define ifpd ifpd_
#endif
#if defined(CERNLIB_QXCAPT)
#define ifps type_of_call IFPS
#define ifpd type_of_call IFPD
#endif
#if defined(CERNLIB_CRAY)
#define ifps IFPS
#define ifpd IFPD
#endif
#if defined(CERNLIB_IBM)
#pragma linkage (ifps ,FORTRAN)
#pragma linkage (ifpd ,FORTRAN)
#endif
 
/****************************/
/*test if the float is finit*/
/****************************/
 
typedef union {
        float flt;
        unsigned uint;
} float_rep;
 
int ifps(x)
float_rep *x;
{
#if ( defined(CERNLIB_SUN)||defined(CERNLIB_SOLARIS)||defined(CERNLIB_SGI)\
     ||defined(CERNLIB_DECS)||defined(CERNLIB_ALPHA)||defined(CERNLIB_HPUX)\
     ||defined(CERNLIB_IBMRT)||defined(CERNLIB_LINUX))\
    &&(!defined(CERNLIB_VAX))
/*test the exponent part of the float*/
if ((x->uint & 0x7f800000)==0x7f800000)
return 0;
else
return 1;
#endif
#if (!defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))\
  &&(!defined(CERNLIB_SGI))&&(!defined(CERNLIB_DECS))\
  &&(!defined(CERNLIB_ALPHA))&&(!defined(CERNLIB_HPUX))\
  &&(!defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_LINUX))
return 1;
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_ALPHA))
return 1;
#endif
}
 
/*****************************/
/*test if the double is finit*/
/*****************************/
 
/* +SELF,IF=SUN,SOLARIS,SGI,DECS,ALPHA,HPUX,IBMRT. */
typedef union {
        double dbl;
        unsigned uint[2];
} double_rep;
/* +SELF. */
 
int ifpd(x)
double_rep *x;
{
/*test the exponent part of the double*/
#if defined(CERNLIB_SUN)||defined(CERNLIB_SOLARIS)||defined(CERNLIB_SGI)||defined(CERNLIB_HPUX)||defined(CERNLIB_IBMRT)||defined(CERNLIB_PPC)
if ((x->uint[0] & 0x7ff00000)==0x7ff00000)
return 0;
else
return 1;
#endif
#if (defined(CERNLIB_DECS)||defined(CERNLIB_ALPHA))&&(!defined(CERNLIB_VAX)) \
  || (defined(CERNLIB_LINUX) && !defined(CERNLIB_PPC))
if ((x->uint[1] & 0x7ff00000)==0x7ff00000)
return 0;
else
return 1;
#endif
#if (!defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))\
  &&(!defined(CERNLIB_SGI))&&(!defined(CERNLIB_DECS))\
  &&(!defined(CERNLIB_ALPHA))&&(!defined(CERNLIB_HPUX))\
  &&(!defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_LINUX))
return 1;
#endif
#if (defined(CERNLIB_VAX))&&(defined(CERNLIB_ALPHA))
return 1;
#endif
}
