*
* $Id: arordj.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: arordj.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arordj.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARORDJ(NJET)
 
C...ARiadne subroutine ORDer Jets
 
C...Orders jets in falling energy
 
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
 
 
C...Error if no space left in /ARPART/
      IF (IPART+N.GT.MSTU(4)) THEN
        CALL LUERRM(11,'(ARCLUS:) no more memory left in LUJETS')
        NJET=-1
        MSTU(3)=0
        RETURN
      ENDIF
 
C...Tag all jets to be copied
      NJET=0
      DO 100 I=1,IPART
        QQ(I)=QDONE(I)
        IF (.NOT.QDONE(I)) NJET=NJET+1
 100  CONTINUE
 
C...Loop twice to get jets in falling order in energy
      IJ=N
      DO 200 II=1,NJET
        EMAX=-1.0
        IEMAX=0
        DO 210 I=1,IPART
          IF (QQ(I)) GOTO 210
          IF (BP(I,4).LE.EMAX) GOTO 210
          EMAX=BP(I,4)
          IEMAX=I
 210    CONTINUE
 
C...Copy jet into /LUJETS/
 
        IJ=IJ+1
        DO 220 J=1,5
          P(IJ,J)=BP(IEMAX,J)
          K(IJ,J)=0
 220    CONTINUE
        K(IJ,1)=31
        K(IJ,2)=97
        QQ(IEMAX)=.TRUE.
 
 200  CONTINUE
 
      MSTU(3)=NJET
 
      RETURN
 
C**** END OF ARORDJ ****************************************************
      END
