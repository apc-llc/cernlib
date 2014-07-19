*
* $Id: arsume.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arsume.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arsume.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARSUME(NULL,BSX,BSY,BSZ,BSE,BSM,NI,I)
 
C...ARiadne subroutine SUM Energy and momentum
 
C...Sum energy and mometum of NI partons in /ARPART/
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
 
      DIMENSION I(NI)
 
 
      IF (NULL.EQ.0) THEN
        BSX=0.0
        BSY=0.0
        BSZ=0.0
        BSE=0.0
      ENDIF
 
      DO 100 IJ=1,NI
        II=I(IJ)
        BSX=BSX+BP(II,1)
        BSY=BSY+BP(II,2)
        BSZ=BSZ+BP(II,3)
        BSE=BSE+BP(II,4)
 100  CONTINUE
 
      B0=0.0
      BSM=SQRT(MAX(B0,BSE**2-BSZ**2-BSY**2-BSX**2))
 
      RETURN
 
C**** END OF ARSUME ****************************************************
      END
