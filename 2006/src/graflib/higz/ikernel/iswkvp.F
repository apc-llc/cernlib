*
* $Id: iswkvp.F,v 1.1.1.1 1996/02/14 13:10:58 mclareni Exp $
*
* $Log: iswkvp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:58  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/02 29/08/95  14.29.43  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISWKVP(IWKID,X1,X2,Y1,Y2)
      ENTRY      GSWKVP(IWKID,X1,X2,Y1,Y2)
#include "higz/hiatt.inc"
      I=IGIWIN(IWKID)
      IF(I.LE.0)THEN
         CALL IGERR('Invalid workstation window parameters','ISWKVP')
         RETURN
      ENDIF
      RDVXMI   = X1
      RDVXMA   = X2
      RDVYMI   = Y1
      RDVYMA   = Y2
      XWKV1(I) = X1
      XWKV2(I) = X2
      YWKV1(I) = Y1
      YWKV2(I) = Y2
#if defined(CERNLIB_PHIGS)
      CALL PSWKV(IWKID,X1,X2,Y1,Y2)
#endif
      END
