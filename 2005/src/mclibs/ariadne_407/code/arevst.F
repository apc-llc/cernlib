*
* $Id: arevst.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arevst.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arevst.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE AREVST(ISIN)
 
C...Ariadne subroutine REVerse colour flow of STring entry
 
C...Reverse the colour flow of string entry ISIN in /ARSTRS/
 
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
 
 
C...Purely glounic strings have no determined flow
      IF (IFLOW(ISIN).EQ.2) RETURN
 
C...The first shall be the last
      IS=ISIN
      IP=IPF(IS)
 100  ICO=IDO(IP)
      IDO(IP)=IDI(IP)
      IDI(IP)=ICO
      IF (ICO.GT.0) THEN
        IPS=IP3(ICO)
        IP3(ICO)=IP1(ICO)
        IP1(ICO)=IPS
        BX=BX1(ICO)
        BX1(ICO)=BX3(ICO)
        BX3(ICO)=BX
        AEX=AEX1(ICO)
        AEX1(ICO)=AEX3(ICO)
        AEX3(ICO)=AEX
        IF (ABS(IRAD(ICO)).LT.10) IRAD(ICO)=-IRAD(ICO)
      ENDIF
      IF (IP.NE.IPL(IS)) THEN
        IP=IPS
        GOTO 100
      ENDIF
 
      IFLOW(IS)=-IFLOW(IS)
      IP=IPF(IS)
      IPF(IS)=IPL(IS)
      IPL(IS)=IP
 
      RETURN
 
C**** END OF AREVST ****************************************************
      END
