*
* $Id: arclus.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arclus.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arclus.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARCLUS(NJET)
 
C...ARiadne subroutine jet CLUStering
 
C...Clusters particle in the /LUJETS/ common block into jets according
C...the dipole clustering algorithm.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
C...Reset error flag.
      MSTA(13)=0
 
C...Copy all particle to be considered as jet-initiators to the end
C...of the event record.
      CALL ARCOPJ
 
C...The total number of jetinitiators = current number of jets.
      I1=0
      I3=0
 
      IF (IDIPS.LT.MAX(MSTU(47),2)) THEN
        CALL ARORDJ(NJET)
        NJET=-1
        RETURN
      ENDIF
 
C...Loop over all possible three-jets to find the three jets with
C...smallest invariant p_t^2
 
C...Exit if minimum number of jets reached
 100  IF (IDIPS.EQ.2) THEN
        CALL ARORDJ(NJET)
        RETURN
      ENDIF
 
      J1=0
      J2=0
      J3=0
      PT2MIN=PARA(31)
 
      DO 110 I2=1,IPART
        IF (QDONE(I2)) GOTO 110
        CALL ARUPDJ(I2,I1,I3)
        IF (PT2IN(I2).LT.PT2MIN) THEN
          J1=IDI(I2)
          J2=I2
          J3=IDO(I2)
          PT2MIN=PT2IN(I2)
        ENDIF
 110  CONTINUE
 
      PARU(63)=SQRT(PT2MIN)
 
C...Exit if smallest p_t^2 is above cutoff
      IF (J1.EQ.0.OR.IDIPS.EQ.MAX(MSTU(47),2)) THEN
        CALL ARORDJ(NJET)
        RETURN
      ENDIF
 
C...Else join the three jets into two and redo the procedure.
      CALL ARJOIN(J1,J2,J3)
      I1=J1
      I3=J3
 
      GOTO 100
 
C**** END OF ARCLUS ****************************************************
      END
