*
* $Id: himeta.inc,v 1.1.1.1 1996/02/14 13:10:14 mclareni Exp $
*
* $Log: himeta.inc,v $
* Revision 1.1.1.1  1996/02/14 13:10:14  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIMETA_INC
#define CERNLIB_HIGZ_HIMETA_INC
*
*
* himeta.inc
*
*CMZ :  1.18/00 23/04/93  17.27.57  by  O.Couet
*-- Author :
      COMMON /HIMETA/ IDMETA,XMFACT,YMFACT,TEKACT,METACT,FILOPN
      LOGICAL TEKACT,METACT,FILOPN
      COMMON /HIMETC/ CHMETA
      CHARACTER*24 CHMETA
#if defined(CERNLIB_MSDOS)
#include "higz/himdos.inc"
#endif


#endif
