*
* $Id: fmoptc.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fmoptc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMOPTC(CHOPT,CHPOSS,IOPT)
*CMZ :          20/08/91  17.23.54  by  Jamie Shiers
*-- Author :    Jamie Shiers   20/08/91
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
