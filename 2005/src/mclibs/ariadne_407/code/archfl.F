*
* $Id: archfl.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: archfl.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: archfl.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARCHFL
 
C...ARiadne subroutine CHeck colour FLow
 
C...Checks colour flow consistency in the dipole record
 
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
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      DIMENSION ICP(MAXPAR),ICD(MAXDIP)
      INXT(I)=IP3(IDO(I))
 
 
      DO 100 I=1,IPART
        ICP(I)=0
 100  CONTINUE
      DO 110 I=1,IDIPS
        ICD(I)=0
 110  CONTINUE
      NPC=0
 
      IF (ISTRS.LE.0.OR.ISTRS.GT.MAXSTR) CALL ARERRM('ARCHFL',5,0)
C...Loop over all strings in dipole record
      DO 200 IS=1,ISTRS
 
C...Loop over all particles in each string
        I=IPF(IS)
 210    NPC=NPC+1
        IF (NPC.GT.IPART) CALL ARERRM('ARCHFL',5,0)
        IF (I.LE.0.OR.I.GT.IPART) CALL ARERRM('ARCHFL',5,0)
        IF (ICP(I).NE.0) CALL ARERRM('ARCHFL',5,0)
        ICP(I)=1
        IF (I.NE.IPL(IS)) THEN
          ID=IDO(I)
          IF (ID.LE.0.OR.ID.GT.IDIPS) CALL ARERRM('ARCHFL',5,0)
          IF (ICD(ID).NE.0) CALL ARERRM('ARCHFL',5,0)
          ICD(ID)=1
          I=IP3(ID)
          GOTO 210
        ENDIF
        IF (IFLOW(IS).EQ.2) THEN
          ID=IDO(I)
          IF (ID.LE.0.OR.ID.GT.IDIPS) CALL ARERRM('ARCHFL',5,0)
          IF (ICD(ID).NE.0) CALL ARERRM('ARCHFL',5,0)
          ICD(ID)=1
        ENDIF
 200  CONTINUE
 
      NT=1
      DO 300 I=1,IPART
        NT=NT*ICP(I)
 300  CONTINUE
      DO 310 I=1,IDIPS
        IF (.NOT.QEM(I)) NT=NT*ICD(I)
 310  CONTINUE
 
      IF (NT.EQ.0) CALL ARERRM('ARCHFL',5,0)
 
      RETURN
 
C**** END OF ARCHFL ****************************************************
      END
