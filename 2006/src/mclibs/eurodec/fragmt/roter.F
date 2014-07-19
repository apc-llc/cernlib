*
* $Id: roter.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: roter.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE ROTER(CT,ST,CF,SF,P)
C.----------------------------------------------------------------------
C.
C.    ROTATE THE 3-VECTOR P
C.    ST = SQRT(1.-CT**2), CF = COS(F), SF = SIN(F)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
      DIMENSION P(3),ROT(3,3)
      ROT(1,1)=CT*CF
      ROT(1,2)=-SF
      ROT(1,3)=ST*CF
      ROT(2,1)=SF*CT
      ROT(2,2)=CF
      ROT(2,3)=ST*SF
      ROT(3,1)=-ST
      ROT(3,2)=0.
      ROT(3,3)=CT
      PX=P(1)
      PY=P(2)
      PZ=P(3)
      DO 10 J=1,3
   10 P(J)=ROT(J,1)*PX+ROT(J,2)*PY+ROT(J,3)*PZ
      RETURN
      END
