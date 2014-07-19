*
* $Id: arcasc.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: arcasc.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arcasc.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
 
      SUBROUTINE ARCASC
 
C...ARiadne subroutine perform dipole CASCade
 
C...Performs a colour dipole cascade on string put in the ariadne
C...event record.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
C...Calculate total momentum of strings for debugging
      IF(MSTA(9).GT.0) CALL ARCHEM(1)
 
C...Reset counter
      IO=0
 
C...Loop over all dipole to find largest possible p_t^2
 100  ISEL=0
      PT2MAX=0.0
      DO 110 I=1,IDIPS
        PT2I=ARGPT2(I)
        IF(PT2I.GT.PT2MAX) THEN
          PT2MAX=PT2I
          ISEL=I
        ENDIF
 110  CONTINUE
 
C...Check that largest p_t^2 is above cuts.
      IF(ISEL.GT.0) THEN
        IF((QEM(ISEL).AND.PT2MAX.LE.PARA(5)**2).OR.
     $     ((.NOT.QEM(ISEL)).AND.PT2MAX.LE.PARA(3)**2)) ISEL=0
      ENDIF
 
      IF(MSTA(6).GE.0.AND.IO.GE.MSTA(6)) ISEL=0
 
C...Exit if below cuts or limit of number of emissions is reached
      IF(ISEL.EQ.0) THEN
        CALL ARDUMP
        IF(MSTA(9).GT.0) CALL ARCHEM(0)
        RETURN
      ENDIF
 
C...Perform the emission
      IO=IO+1
      PT2LST=PT2MAX
      CALL AREMIT(ISEL)
      QDUMP=.FALSE.
 
C...Check total momentum and dump according to debug mode
      IF(MSTA(9).GT.2) CALL ARDUMP
      IF(MSTA(9).GT.1) CALL ARCHEM(0)
      GOTO 100
 
C**** END OF ARCASC ****************************************************
      END
