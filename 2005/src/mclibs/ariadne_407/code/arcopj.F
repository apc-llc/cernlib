*
* $Id: arcopj.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arcopj.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arcopj.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARCOPJ
 
C...ARiadne subroutine COPy Jet
 
C...Copies particles into jet initiators in /LUCLUS/
 
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
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      COMMON /LUDAT2/ KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      SAVE /LUDAT2/
 
      DIMENSION DSUMP(5)
 
 
C...If continuing jet clustering return immediately
      IF (MSTU(48).EQ.1.AND.IPART.GT.0) RETURN
 
C...Reset momentum sum
      DO 100 J=1,4
        DSUMP(J)=0.0
 100  CONTINUE
 
C...Reset jet counter
      IPART=0
 
C...Loop over all particles in the event record
      DO 300 I=1,N
 
C...Disregard all decayed particles and unknown entries
        IF (K(I,1).LE.0.OR.K(I,1).GE.10) GOTO 300
 
C...Disregard neutrinos and neutral particles according to MSTU(41)
        IF (MSTU(41).GE.2) THEN
          KC=LUCOMP(K(I,2))
          IF (KC.EQ.0.OR.KC.EQ.12.OR.KC.EQ.14
     $         .OR.KC.EQ.16.OR.KC.EQ.18)
     $      GOTO 300
          IF (MSTU(41).GE.3.AND.KCHG(KC,2).EQ.0.AND.
     $         LUCHGE(K(I,2)).EQ.0)
     $      GOTO 300
        ENDIF
 
        IF (IPART.GE.MAXPAR.OR.IPART.GE.MAXDIP) THEN
          CALL ARERRM('ARCOPJ',21,0)
          IPART=0
          RETURN
        ENDIF
 
C...Tag found jet-initiator
        IPART=IPART+1
        DO 400 J=1,5
          BP(IPART,J)=P(I,J)
          DSUMP(J)=DSUMP(J)+BP(IPART,J)
 400    CONTINUE
        PT2IN(IPART)=0.0
        BX1(IPART)=BP(IPART,5)**2
        QDONE(IPART)=.FALSE.
        IDI(IPART)=0
        IDO(IPART)=0
 300  CONTINUE
 
      IDIPS=IPART
      MSTU(62)=IPART
      PARU(61)=SQRT(MAX(0.0D0,DSUMP(4)**2-
     $     DSUMP(3)**2-DSUMP(2)**2-DSUMP(1)**2))
 
      RETURN
 
C**** END OF ARCOPJ ****************************************************
      END
