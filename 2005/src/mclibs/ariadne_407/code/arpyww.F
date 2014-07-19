*
* $Id: arpyww.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arpyww.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arpyww.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARPYWW
 
C...ARiadne subroutine PYthia WW event
 
C...Handle a e+e- -> W+W- or e+e- -> Z0Z0 event from PYTHIA
 
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
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
      DIMENSION IR(1)
 
 
      IR(1)=0
      CALL ARSCAN(MSTI(4)+1,N,1,IR)
      MHAR(108)=1
      CALL ARCASC
      IF (PARA(28).GT.0.0.AND.MHAR(101).EQ.2.AND.MSTA(35).EQ.2) THEN
        DO 110 ID=1,IDIPS
          IF (QEM(ID)) GOTO 110
          QDONE(ID)=.FALSE.
          ICOLI(ID)=MOD(ICOLI(ID),1000)
 110    CONTINUE
        IF (MHAR(111).GT.0) THEN
          PARA(28)=-PARA(28)
          PT2LST=PARA(40)
          CALL ARCONT
          PARA(28)=-PARA(28)
        ENDIF
      ENDIF
      MHAR(108)=0
 
      RETURN
 
C**** END OF ARPYWW ****************************************************
      END
