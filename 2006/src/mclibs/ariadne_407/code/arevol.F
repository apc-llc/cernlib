*
* $Id: arevol.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arevol.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arevol.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE AREVOL(PTMAX,PTMIN)
 
C...ARiadne subroutine EVOLute with dipole emissions
 
C...Evolves a string in the ariadne event record using dipole
C...successive emissions between the scale PTMAX and PTMIN
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
 
 
C...Set limits
      PT2LST=PTMAX**2
      PSAV3=PARA(3)
      PSAV5=PARA(5)
      PARA(3)=MAX(PARA(3),PTMIN)
      PARA(5)=MAX(PARA(5),PTMIN)
      QABOVE=(MSTA(35).GT.0.AND.PT2LST.GT.ABS(PHAR(112)).AND.
     $     PHAR(112).NE.0)
 
C...Loop over all dipole to find largest possible p_t^2
 100  ISEL=0
      IF (MSTA(35).GT.0.AND.(IO.GT.0.OR.MHAR(108).GT.0)) CALL AREARR
      PT2MAX=0.0
      DO 110 I=1,IDIPS
        PT2I=ARGPT2(I)
        IF (PT2I.GT.PT2MAX) THEN
          PT2MAX=PT2I
          ISEL=I
        ENDIF
 110  CONTINUE
 
C...Check that largest p_t^2 is above cuts.
      IF (ISEL.GT.0) THEN
        IF ((QEM(ISEL).AND.PT2MAX.LE.PARA(5)**2).OR.
     $     ((.NOT.QEM(ISEL)).AND.PT2MAX.LE.PARA(3)**2)) ISEL=0
      ENDIF
 
      IF (MSTA(6).GE.0.AND.IO.GE.MSTA(6)) ISEL=0
 
C...Check if reconnection between strings are possible.
      IF (QABOVE.AND.(PT2MAX.LT.ABS(PHAR(112)).OR.ISEL.EQ.0)) THEN
        QABOVE=.FALSE.
        DO 200 ID=1,IDIPS
          IF (QEM(ID)) GOTO 200
          ICOLI(ID)=MOD(ICOLI(ID),1000)
 200    CONTINUE
      ENDIF
 
C...Exit if below cuts or limit of number of emissions is reached
      IF (ISEL.EQ.0) THEN
        IF (MHAR(107).EQ.-1) THEN
          DO 210 ID=1,IDIPS
            IF (QEM(ID)) GOTO 210
            ICOLI(ID)=MOD(ICOLI(ID),1000)
 210      CONTINUE
        ENDIF
        IF (MSTA(35).NE.0) CALL AREARR
        PARA(3)=PSAV3
        PARA(5)=PSAV5
        RETURN
      ENDIF
 
C...Perform the emission
      IO=IO+1
      PT2LST=PT2MAX
      CALL AREMIT(ISEL)
      QDUMP=.FALSE.
 
C...Check total momentum and dump according to debug mode
      IF (MSTA(9).GT.2) CALL ARDUMP
      IF (MSTA(9).GT.1) CALL ARCHEM(0)
      GOTO 100
 
C**** END OF ARCASC ****************************************************
      END
