*
* $Id: arremd.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arremd.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arremd.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARREMD(IREM)
 
C...ARiadne subroutine REMove Dipole
 
C...Remove dipole entry from /ARDIPS/ and purge event record
 
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
 
 
      IDP=-1
      IF (IP1(IREM).GT.0) IDP=IDI(IP1(IREM))
      IDN=-1
      IF (IP3(IREM).GT.0) IDN=IDO(IP3(IREM))
      IF (IDP.GT.IREM) IDP=IDP-1
      IF (IDN.GT.IREM) IDN=IDN-1
 
C...Purge the dipole vector
      DO 100 ID=IREM+1,IDIPS
        I=ID-1
        BX1(I)=BX1(ID)
        BX3(I)=BX3(ID)
        PT2IN(I)=PT2IN(ID)
        SDIP(I)=SDIP(ID)
        IP1(I)=IP1(ID)
        IP3(I)=IP3(ID)
        AEX1(I)=AEX1(ID)
        AEX3(I)=AEX3(ID)
        QDONE(I)=QDONE(ID)
        QEM(I)=QEM(ID)
        IRAD(I)=IRAD(ID)
        ISTR(I)=ISTR(ID)
        ICOLI(I)=ICOLI(ID)
 100  CONTINUE
 
      DO 200 IP=1,IPART
        IF (IDO(IP).EQ.IREM) THEN
          IDO(IP)=0
        ELSEIF (IDO(IP).GT.IREM.AND.IDO(IP).LE.IDIPS) THEN
          IDO(IP)=IDO(IP)-1
        ENDIF
        IF (IDI(IP).EQ.IREM) THEN
          IDI(IP)=0
        ELSEIF (IDI(IP).GE.IREM.AND.IDI(IP).LE.IDIPS) THEN
          IDI(IP)=IDI(IP)-1
        ENDIF
 200  CONTINUE
 
      IDIPS=IDIPS-1
 
      IF (IDP.GT.0.AND.IDN.GT.0) THEN
        IF (ICOLI(IDP).EQ.ICOLI(IDN)) THEN
          ISCOL=ICOLI(IDP)/1000
          ICOLI(IDP)=0
          ICOLI(IDN)=0
          CALL ARCOLI(IDP,-ISCOL)
          CALL ARCOLI(IDN,-ISCOL)
        ENDIF
      ENDIF
 
      RETURN
 
C**** END OF ARREMD ****************************************************
      END
