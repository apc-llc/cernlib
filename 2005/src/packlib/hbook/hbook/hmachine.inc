*
* $Id: hmachine.inc,v 1.1.1.1 1996/01/16 17:07:52 mclareni Exp $
*
* $Log: hmachine.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:52  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HMACHINE_INC
#define CERNLIB_HBOOK_HMACHINE_INC
*
*
* hmachine.inc
*
*CMZ :  4.19/00 19/04/93  09.58.06  by  Rene Brun
*-- Author :
#if defined(CERNLIB_CDC)
#include "hbook/hmac1.inc"
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_NORD)
#include "hbook/hmac2.inc"
#endif
#if defined(CERNLIB_UNIVAC)
#include "hbook/hmac3.inc"
#endif
#if defined(CERNLIB_VAX)
#include "hbook/hmac4.inc"
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_DOUBLE))
#include "hbook/hmac5.inc"
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_DOUBLE))
#include "hbook/hmac6.inc"
#endif
 

#endif
