*
* $Id: arremg.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arremg.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arremg.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARREMG(IGI)
 
C...ARiadne subroutine REMove Gluon
 
C...Removes the gluon entry IG and reconnects neighboring dipoles
 
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
 
C...Check that we don't have a gluon ring...
      IG=IGI
      IF (IFLOW(ISTR(IDO(IG))).EQ.2.AND.
     $     IP3(IDO(IG)).EQ.IP1(IDI(IG))) RETURN
 
C...First change pointers making IG and one neighboring dipole orphans
      IDREM=IDO(IG)
      IP=IP3(IDREM)
      ID=IDI(IG)
      IP3(ID)=IP
      IDI(IP)=ID
      IDP=IDI(IG)
      IF (IDP.GT.IDREM) IDP=IDP-1
 
C...Purge the parton vector
      DO 100 IP=IG+1,IPART
        I=IP-1
        DO 110 J=1,5
          BP(I,J)=BP(IP,J)
 110    CONTINUE
        IFL(I)=IFL(IP)
        QEX(I)=QEX(IP)
        QQ(I)=QQ(IP)
        IDI(I)=IDI(IP)
        IDO(I)=IDO(IP)
        INO(I)=INO(IP)
        INQ(I)=INQ(IP)
        XPMU(I)=XPMU(IP)
        XPA(I)=XPA(IP)
        PT2GG(I)=PT2GG(IP)
 100  CONTINUE
      IPART=IPART-1
 
C...Purge the dipole vector
      DO 200 ID=IDREM+1,IDIPS
        I=ID-1
        BX1(I)=BX1(ID)
        BX3(I)=BX3(ID)
        PT2IN(I)=PT2IN(ID)
        SDIP(I)=SDIP(ID)
        IP1(I)=IP1(ID)
        IP3(I)=IP3(ID)
        AEX1(I)=AEX1(ID)
        AEX1(I)=AEX3(ID)
        QDONE(I)=QDONE(ID)
        QEM(I)=QEM(ID)
        IRAD(I)=IRAD(ID)
        ISTR(I)=ISTR(ID)
        ICOLI(I)=ICOLI(ID)
 200  CONTINUE
      IDIPS=IDIPS-1
 
C...Reset changed pointers
 
      DO 300 IP=1,IPART
        IF (IDO(IP).GE.IDREM) IDO(IP)=IDO(IP)-1
        IF (IDI(IP).GE.IDREM) IDI(IP)=IDI(IP)-1
 300  CONTINUE
      DO 310 ID=1,IDIPS
        IF (IP3(ID).GE.IG) IP3(ID)=IP3(ID)-1
        IF (IP1(ID).GE.IG) IP1(ID)=IP1(ID)-1
 310  CONTINUE
      DO 320 IS=1,ISTRS
        IF (IPF(IS).GE.IG) IPF(IS)=IPF(IS)-1
        IF (IPL(IS).GE.IG) IPL(IS)=IPL(IS)-1
 320  CONTINUE
 
C...Fix up colour indices
      IDN=IDO(IP3(IDP))
      IF (IDN.GT.0) THEN
        IF (ICOLI(IDN).EQ.ICOLI(IDP)) THEN
          ISCOL=ICOLI(IDN)/1000
          ICOLI(IDN)=0
          ICOLI(IDP)=0
          CALL ARCOLI(IDN,-ISCOL)
          CALL ARCOLI(IDP,-ISCOL)
        ENDIF
      ENDIF
 
      RETURN
 
C**** END OF ARREMG ****************************************************
      END
