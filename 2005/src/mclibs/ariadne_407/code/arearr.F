*
* $Id: arearr.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: arearr.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arearr.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE AREARR
 
C...Ariadne subroutine REARRange colour flow.
 
C...Reconnects partons to alternative colour flows if this decreases
C...total 'lambda'
 
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
 
 
      IF (PARA(26).LE.1.0.OR.MSTA(35).EQ.0) RETURN
      DO 100 ID=1,IDIPS
        IF (QEM(ID)) GOTO 100
        IF (ICOLI(ID).EQ.0) CALL ARERRM('AREARR',32,0)
        SDIP(ID)=ARMAS2(IP1(ID),IP3(ID))
 100  CONTINUE
 
      DO 110 IS=1,ISTRS
        IF (IFLOW(IS).LT.0) CALL AREVST(IS)
 110  CONTINUE
 
 300  IX1SEL=0
      IX2SEL=0
      XLDIFF=0.0
      DO 200 ID1=1,IDIPS
        IF (QEM(ID1)) GOTO 200
        IA=IP1(ID1)
        IB=IP3(ID1)
        SAB=SDIP(ID1)
        AFAC=1.0
        BFAC=1.0
        IF (PHAR(107).GT.0.AND.ABS(IFL(IA)).GT.1000) AFAC=PHAR(107)
        IF (PHAR(107).GT.0.AND.ABS(IFL(IB)).GT.1000) BFAC=PHAR(107)
        ALA=0.0
        XMA=1.0
        ALB=0.0
        XMB=1.0
        IF (MHAR(106).EQ.1) THEN
          IF (QEX(IA)) THEN
            ALA=XPA(IA)
            XMA=XPMU(IA)
          ENDIF
          IF (QEX(IB)) THEN
            ALB=XPA(IB)
            XMB=XPMU(IB)
          ENDIF
        ENDIF
        SLAB=2.0*(LOG(SAB*AFAC*BFAC)+
     $       ALA*LOG(XMA)+ALB*LOG(XMB))/(2.0+ALA+ALB)
        DO 210 ID2=1,IDIPS
          IF (QEM(ID2)) GOTO 210
          IF (ID1.EQ.ID2) GOTO 210
          IF (ICOLI(ID1).NE.ICOLI(ID2)) GOTO 210
          IC=IP1(ID2)
          ID=IP3(ID2)
          SCD=SDIP(ID2)
          CFAC=1.0
          DFAC=1.0
          IF (PHAR(107).GT.0.AND.ABS(IFL(IC)).GT.1000) CFAC=PHAR(107)
          IF (PHAR(107).GT.0.AND.ABS(IFL(ID)).GT.1000) DFAC=PHAR(107)
          ALC=0.0
          XMC=1.0
          ALD=0.0
          XMD=1.0
          IF (MHAR(106).EQ.1) THEN
            IF (QEX(IC)) THEN
              ALC=XPA(IC)
              XMC=XPMU(IC)
            ENDIF
            IF (QEX(ID)) THEN
              ALD=XPA(ID)
              XMD=XPMU(ID)
            ENDIF
          ENDIF
          SAD=ARMAS2(IA,ID)
          SBC=ARMAS2(IB,IC)
          SLCD=2.0*(LOG(SCD*CFAC*DFAC)+
     $         ALC*LOG(XMC)+ALD*LOG(XMD))/(2.0+ALC+ALD)
          SLBC=2.0*(LOG(SBC*BFAC*CFAC)+
     $         ALC*LOG(XMC)+ALB*LOG(XMB))/(2.0+ALC+ALB)
          SLAD=2.0*(LOG(SAD*AFAC*DFAC)+
     $         ALA*LOG(XMA)+ALD*LOG(XMD))/(2.0+ALA+ALD)
          XLD=SLAB+SLCD-SLBC-SLAD
          IF (XLD.LE.XLDIFF) GOTO 210
          IF ((MHAR(106).EQ.-1.OR.MHAR(106).EQ.2).AND.
     $         MAX(SLAD,SLBC).GT.MIN(SLAB,SLCD)) GOTO 210
          IX1SEL=ID1
          IX2SEL=ID2
          XLDIFF=XLD
 210    CONTINUE
 200  CONTINUE
      IF (IX1SEL.EQ.0.OR.IX2SEL.EQ.0) RETURN
      CALL ARSWAP(IX1SEL,IX2SEL)
      GOTO 300
 
C**** END OF AREARR ****************************************************
      END
