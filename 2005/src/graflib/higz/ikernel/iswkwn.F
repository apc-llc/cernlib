*
* $Id: iswkwn.F,v 1.1.1.1 1996/02/14 13:10:58 mclareni Exp $
*
* $Log: iswkwn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:58  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.14/11 24/06/92  12.46.29  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISWKWN(IWKID,X1,X2,Y1,Y2)
      ENTRY      GSWKWN(IWKID,X1,X2,Y1,Y2)
#include "higz/hiatt.inc"
      I=IGIWIN(IWKID)
      IF((X1.LT.0).OR.(X1.GT.1)
     +            .OR.
     +   (X2.LT.0).OR.(X2.GT.1)
     +            .OR.
     +   (Y1.LT.0).OR.(Y1.GT.1)
     +            .OR.
     +   (Y2.LT.0).OR.(Y2.GT.1).OR.(I.LE.0))THEN
         CALL IGERR('Invalid workstation window parameters','ISWKWN')
         RETURN
      ENDIF
      RDWXMI   = X1
      RDWXMA   = X2
      RDWYMI   = Y1
      RDWYMA   = Y2
      XWKW1(I) = X1
      XWKW2(I) = X2
      YWKW1(I) = Y1
      YWKW2(I) = Y2
#if defined(CERNLIB_PHIGS)
      CALL PSWKW(IWKID,X1,X2,Y1,Y2)
#endif
      END
