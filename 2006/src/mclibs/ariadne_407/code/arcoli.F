*
* $Id: arcoli.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: arcoli.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arcoli.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARCOLI(ID,IDR)
 
C...Ariadne subroutine assign COLour Index
 
C...Assigns a colour index to dipole ID, requiring it to be different
C...IDR if IDR > 0
 
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
 
 
      ICOLI(ID)=0
      IF (PARA(26).LE.1.0.OR.QEM(ID).OR.MSTA(35).EQ.0) RETURN
      NCOL=INT(PARA(26)+0.5)
      IDP=IDI(IP1(ID))
      IDN=IDO(IP3(ID))
 100  ICOL=INT(RLU(0)*REAL(NCOL))+1
      ICOLI(ID)=ICOL
      IF (IDR.LT.0.AND.(MSTA(35).EQ.1.OR.MSTA(35).EQ.2)) THEN
        ICOLI(ID)=ICOL-1000*IDR
      ENDIF
      IF (IDR.GT.0) THEN
        IF ((MHAR(107).GE.3.OR.MHAR(107).LT.0).AND.
     $       PT2LST.GT.ABS(PHAR(112))) THEN
          ICOLI(ID)=ICOL+1000*ISTRS
        ELSE
          ICOLI(ID)=ICOL+1000*(ICOLI(IDR)/1000)
          IF (MHAR(107).GT.1.OR.(MHAR(107).EQ.1.AND.IO.EQ.1)) THEN
            IF (ICOLI(IDR).EQ.ICOLI(ID)) GOTO 100
          ENDIF
        ENDIF
      ENDIF
      IF (IDN.GT.0.AND.(.NOT.QEM(IDN))) THEN
        IF (ICOLI(IDN).EQ.ICOLI(ID)) GOTO 100
      ENDIF
      IF (IDP.GT.0.AND.(.NOT.QEM(IDP))) THEN
        IF (ICOLI(IDP).EQ.ICOLI(ID)) GOTO 100
      ENDIF
 
      RETURN
 
C**** END OF ARCOLI ****************************************************
      END
