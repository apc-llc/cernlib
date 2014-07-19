*
* $Id: hcqcor.inc,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hcqcor.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCQCOR_INC
#define CERNLIB_HBOOK_HCQCOR_INC
*
*
* hcqcor.inc
*
*CMZ :  4.19/00 13/04/93  16.36.40  by  Rene Brun
*-- Author :
#if !defined(CERNLIB_DOUBLE)
#include "hbook/hcqcor1.inc"
#endif
#if defined(CERNLIB_DOUBLE)
#include "hbook/hcqcor2.inc"
#endif

#endif
