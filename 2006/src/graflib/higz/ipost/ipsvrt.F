*
* $Id: ipsvrt.F,v 1.1.1.1 1996/02/14 13:11:07 mclareni Exp $
*
* $Log: ipsvrt.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 24/05/95  10.21.27  by  O.Couet
*-- Author :    O.Couet   24/05/95
      SUBROUTINE IPSVRT(IFLAG)
#include "higz/hipost.inc"
      IF (IFLAG.EQ.1) THEN
         CALL IPPSTF(7,' gsave ')
         NBSAVE = NBSAVE+1
      ELSE
         CALL IPPSTF(4,' gr ')
         NBSAVE = NBSAVE-1
      ENDIF
      END
