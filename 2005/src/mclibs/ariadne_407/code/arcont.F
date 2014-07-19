*
* $Id: arcont.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arcont.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arcont.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARCONT
 
C...ARiadne subroutine CONTinue dipole cascade
 
C...Continues a dipole cascade peviously started with ARCASC
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
C...Perform the evolution
      CALL AREVOL(SQRT(PT2LST),0.0)
 
C...Check momentum and dump to /LUJETS/
      IF (.NOT.QDUMP) CALL ARDUMP
      IF (MSTA(9).GT.0) CALL ARCHEM(0)
 
      RETURN
 
C**** END OF ARCONT ****************************************************
      END
