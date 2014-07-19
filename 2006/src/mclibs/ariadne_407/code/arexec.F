*
* $Id: arexec.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arexec.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arexec.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE AREXEC
 
C...ARiadne subroutine EXECute ariadne
 
C...The Main driver routine in Ariadne.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,XY,W2,XQ2,U
      SAVE /LEPTOU/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
 
C...Step counter
      MSTA(4)=MSTA(4)+1
 
C...Reset some stuff
      MSTA(13)=0
      MHAR(121)=0
      MHAR(129)=0
 
C...Error if ARINIT has not been called
      IF (MSTA(2).EQ.0) CALL ARERRM('AREXEC',12,0)
 
C...Unmark unused special positions in dipole record
      DO 100 I=MAXPAR-4,MAXPAR-2
        QQ(I)=.FALSE.
 100  CONTINUE
 
C...If ariadne mode just pass event through to ARPARS
      IF (MSTA(1).EQ.0) THEN
        CALL ARPARS(1,N)
 
C...If JETSET mode should work by just passing event on to ARPARS
      ELSEIF (MSTA(1).EQ.1) THEN
        CALL ARPARS(1,N)
 
C...If PYTHIA mode tag extended partons etc.
      ELSEIF (MSTA(1).EQ.2) THEN
 
        CALL ARPYTH
 
C...If LEPTO mode tag extended partons
      ELSEIF (MSTA(1).EQ.3) THEN
        IF (MSTA(32).GT.0) THEN
          CALL ARLEPT
          GOTO 900
        ENDIF
 
C...Boost to hadronic cm to avoid precision problems
        CALL ARBOLE(THEL,PHI1,PHI2,DBXL,DBYL,DBZL)
 
        IF (LST(24).EQ.1) THEN
 
          IF (MSTA(30).LT.2) THEN
            K(5,4)=0
          ELSE
            K(5,4)=3
            PARA(13)=SQRT(XQ2)
          ENDIF
          IF (MSTA(30).EQ.0) THEN
            K(6,4)=1
          ELSE
            K(6,4)=2
            PARA(12)=PARA(11)/(1.0-X)
          ENDIF
          CALL ARPARS(5,6)
        ELSEIF (LST(24).EQ.3) THEN
 
          IF (MSTA(30).LT.2) THEN
            K(5,4)=0
          ELSE
            K(5,4)=3
            PARA(13)=SQRT(XQ2)
          ENDIF
          IF (MSTA(30).EQ.0) THEN
            K(6,4)=1
          ELSE
            K(6,4)=2
            PARA(12)=PARA(11)/(1.0-X)
          ENDIF
          CALL ARPARS(5,6)
          IF (MSTA(30).LT.2) THEN
            K(7,4)=0
          ELSE
            K(7,4)=3
            PARA(13)=SQRT(XQ2)
          ENDIF
          IF (MSTA(30).EQ.0) THEN
            K(8,4)=1
          ELSE
            K(8,4)=2
            PARA(12)=PARA(11)/(1.0-X)
          ENDIF
          CALL ARPARS(7,8)
        ENDIF
        CALL LUDBRB(1,N,0.0,PHI2,0.0D0,0.0D0,0.0D0)
        CALL LUDBRB(1,N,THEL,PHI1,DBXL,DBYL,DBZL)
      ENDIF
 
C...Perform fragmentation if requested
 900  IF (MSTA(5).EQ.1) CALL LUEXEC
 
      RETURN
 
C**** END OF AREXEC ****************************************************
      END
