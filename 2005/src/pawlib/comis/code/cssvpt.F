*
* $Id: cssvpt.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: cssvpt.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.34  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSSVPT
***-----------------------
*     saves pointers
***-----------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      I=IPGB
      ASSIGN 1 TO LAB1


      GO TO 4
 1    IPNT(1)=I
      I=IPGP
      ASSIGN 2 TO LAB1
      GO TO 4
 2    IPNT(2)=I
      I=IPGI
      ASSIGN 3 TO LAB1
      GO TO 4
 3    IPNT(3)=I
      RETURN
 4    INXT=I
 5    IF(INXT.EQ.0) GO TO LAB1
      I=INXT
      INXT=IQ(I)
      GO TO 5
      END
