*
* $Id: cdoptc.F,v 1.1.1.1 1996/02/28 16:24:39 mclareni Exp $
*
* $Log: cdoptc.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOPTC(CHOPT,CHPOSS,IOPT)
*
*     Decode character option string ignoring case
*
      CHARACTER*(*) CHOPT,CHPOSS
      CHARACTER*255 CAPOPT
      DIMENSION     IOPT(2)
 
      LCHOPT = LEN(CHOPT)
      IF(LCHOPT.EQ.0) THEN
         CALL VZERO(IOPT,LEN(CHPOSS))
         RETURN
      ENDIF
 
      CAPOPT = CHOPT(1:LCHOPT)
 
      CALL CLTOU(CAPOPT(1:LCHOPT))
 
      DO 10 JOPT=1,LEN(CHPOSS)
 
         IF(INDEX(CAPOPT(1:LCHOPT),CHPOSS(JOPT:JOPT)).NE.0) THEN
            IOPT(JOPT) = 1
         ELSE
            IOPT(JOPT) = 0
         ENDIF
 
10    CONTINUE
 
      END
