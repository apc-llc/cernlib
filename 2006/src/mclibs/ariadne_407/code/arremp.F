*
* $Id: arremp.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arremp.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arremp.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARREMP(IREM)
 
C...ARiadne subroutine REMove Parton
 
C...Remove parton entry from /ARPART/ and purge event record
 
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
 
 
C...Purge the parton vector
      DO 100 IP=IREM+1,IPART
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
 
C...Fix pointers for special entries
      DO 120 IP=MAXPAR-4,MAXPAR-3
        IF (.NOT.QQ(IP)) GOTO 120
        IF (INQ(IP).EQ.IREM) THEN
          INQ(IP)=0
        ELSEIF (INQ(IP).GT.IREM.AND.INQ(IP).LE.IPART) THEN
          INQ(IP)=INQ(IP)-1
        ENDIF
        IF (IDI(IP).EQ.IREM) THEN
          IDI(IP)=0
        ELSEIF (IDI(IP).GT.IREM.AND.IDI(IP).LE.IPART) THEN
          IDI(IP)=IDI(IP)-1
        ENDIF
 120  CONTINUE
 
C...Reset changed pointers
 
      DO 200 ID=1,IDIPS
        IF (IP1(ID).EQ.IREM) THEN
          IP1(ID)=0
        ELSEIF (IP1(ID).GT.IREM.AND.IP1(ID).LE.IPART) THEN
          IP1(ID)=IP1(ID)-1
        ENDIF
        IF (IP3(ID).EQ.IREM) THEN
          IP3(ID)=0
        ELSEIF (IP3(ID).GT.IREM.AND.IP3(ID).LE.IPART) THEN
          IP3(ID)=IP3(ID)-1
        ENDIF
 200  CONTINUE
 
      DO 210 IS=1,ISTRS
        IF (IPF(IS).GE.IREM.AND.IPF(IS).LE.IPART) IPF(IS)=IPF(IS)-1
        IF (IPL(IS).GE.IREM.AND.IPL(IS).LE.IPART) IPL(IS)=IPL(IS)-1
 210  CONTINUE
 
      IPART=IPART-1
 
      RETURN
 
C**** END OF ARREMP ****************************************************
      END
