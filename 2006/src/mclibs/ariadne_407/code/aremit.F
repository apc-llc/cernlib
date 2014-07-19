*
* $Id: aremit.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: aremit.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: aremit.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE AREMIT(ID)
 
C...ARiadne subroutine EMIT
 
C...Administers the an emission from dipole ID
 
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
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,BPDY,BMDY,
     $                BMRP1,BMR1,BMRP3,BMR3,KQ3,KF3,KQ1,KF1,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,
     $                QE1,QE3,ALP1,ALP3,XMU1,XMU3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,IFL1,IFL3,
     $                XT2MP,XT2M,XT2C,XTS,XT3,XT1,XT2GG1,XT2GG3,
     $                YINT,YMAX,YMIN,SQ2,YFAC,PTTRUE,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,ZSQEV,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3,QFAIL,QEXDY
      SAVE /ARINT1/
 
      INXT(I)=IDO(IP3(I))
 
      IF (MHAR(101).EQ.2) THEN
        CALL AREMI3(ID)
        RETURN
      ELSEIF (MSTA(32).GT.1.OR.MHAR(101).EQ.1) THEN
        CALL AREMI2(ID)
        RETURN
      ENDIF
 
C...If FSR photon emission go a head
      IF (QEM(ID)) THEN
        CALL ARRADP(ID)
        RETURN
 
C...If q-qbar splitting go a head
      ELSEIF (IRAD(ID).NE.0) THEN
        CALL ARRADQ(ID)
        RETURN
 
C...If gluon emission from point-like dipole or if no p_t-ordered
C...recoil gluon, go a head
      ELSEIF (((.NOT.QEX(IP1(ID))).AND.(.NOT.QEX(IP3(ID))))
     $             .OR.MSTA(18).EQ.0) THEN
        CALL ARRADG(ID,0,SNR,PT21,PT23)
        RETURN
      ENDIF
 
C...If p_t-ordered recoil gluon, first save initial configuration
C...Then perform trial emission
      CALL ARSTOR(ID,IDS,IS1,IS3)
      CALL ARRADG(ID,0,SNR,PT21,PT23)
 
C...If no recoil gluon was produces keep trial emission
      IF (SNR.LE.1.0) RETURN
 
C...If two recoil gluons, tag the smallest one for p_t-ordering
      IF (AEX1(ID).LT.1.0.AND.AEX3(ID).LT.1.0) THEN
        INEWD=3
        IF ((MSTA(17).GE.2.AND.PT21.GE.PT23).OR.
     $       (MSTA(17).LT.2.AND.BX3(ID).GE.BX1(ID))) THEN
          IGR=3
        ELSE
          IGR=1
        ENDIF
 
C...If only one recoil gluon, tag it for p_t-ordering
      ELSEIF (AEX1(ID).LT.1.0.AND.AEX3(ID).GE.1.0) THEN
        IGR=1
        INEWD=2
      ELSEIF (AEX1(ID).GE.1.0.AND.AEX3(ID).LT.1.0) THEN
        IGR=3
        INEWD=2
      ENDIF
 
      IDT=MAXDIP-1
 
C...Calculate the p_t^2 of a possibly earlier emission in place
C...of the recoil gluon. If this p_t^2 is lower than that of the
C...recoil gluon it could not have been emitted earlier and hence
C...the recoil gluon from the trial emission is kept.
      IF (IGR.EQ.1) THEN
        QABOVE=((MSTA(18).GT.2.OR.PT21.GT.PARA(3)**2).AND.
     $       (MSTA(18).GT.1.OR.PT21.GT.XPMU(IS1)**2))
        S=SNR
        XT2MP=PT2IN(IDS)/SNR
        QQ1=QQ(IS1)
        QQ3=.FALSE.
        QE1=QEX(IS1)
        QE3=.FALSE.
        XT2GG3=XT2MP
        XT2GG1=-1.0
        IF ((.NOT.QQ1).AND.(.NOT.QE1)) THEN
          XT2GG1=XT2MP
          IF(INO(IS1).NE.0) XT2GG1=PT2GG(IS1)
        ENDIF
        ALP1=XPA(IS1)
        ALP3=0.0
        XMU1=XPMU(IS1)
        XMU3=0.0
        SY1=BP(IS1,5)/SQRT(SNR)
        SY3=0.0
        IFL1=IFL(IS1)
        IFL3=21
        CALL ARGQCD(-IDT)
        IF (PT2IN(IDT).LT.PT21.AND.QABOVE) RETURN
      ELSE
        QABOVE=((MSTA(18).GT.2.OR.PT23.GT.PARA(3)**2).AND.
     $       (MSTA(18).GT.1.OR.PT23.GT.XPMU(IS3)**2))
        S=SNR
        IF (INO(IS3).NE.0) XT2GG3=PT2GG(IS3)
        QQ1=.FALSE.
        QQ3=QQ(IS3)
        QE1=.FALSE.
        QE3=QEX(IS3)
        XT2GG1=XT2MP
        XT2GG3=-1.0
        IF ((.NOT.QQ3).AND.(.NOT.QE3)) THEN
          XT2GG3=XT2MP
          IF(INO(IS3).NE.0) XT2GG3=PT2GG(IS3)
        ENDIF
        ALP1=0.0
        ALP3=XPA(IS3)
        XMU1=0.0
        XMU3=XPMU(IS3)
        SY1=0.0
        SY3=BP(IS3,5)/SQRT(SNR)
        IFL1=21
        IFL3=IFL(IS3)
        CALL ARGQCD(-IDT)
        IF (PT2IN(IDT).LT.PT23.AND.QABOVE) RETURN
      ENDIF
 
C...A gluon can be emittes in place of the recoil gluon at an earlier
C...time. Recall the initial configuration and redo the emission without
C...recoil gluon
      CALL ARRECA(ID,IDS,IS1,IS3)
 
      IDIPS=IDIPS-INEWD
      IPART=IPART-INEWD
      CALL ARRADG(ID,IGR,SNREF,PT21,PT23)
 
C...Set p_t^2 for the emission in place of the recoil gluon
      IDS=ID
      IF (IGR.EQ.3) THEN
        IDS=INXT(ID)
        IF (INEWD.EQ.3.AND.PT21.GT.0.0) IDS=INXT(IDS)
      ENDIF
 
      CALL ARSTOR(IDS,IDSS,ISS1,ISS3)
      IP1(IDSS)=ISS1
      IP3(IDSS)=ISS3
      CALL ARBOCM(IDSS)
 
      QDONE(IDS)=.TRUE.
      SDIP(IDS)=ARMAS2(ISS1,ISS3)
      BX1(IDS)=BX1(IDT)
      BX3(IDS)=BX3(IDT)
      AEX1(IDS)=AEX1(IDT)
      AEX3(IDS)=AEX3(IDT)
      IRAD(IDS)=IRAD(IDT)
      PT2IN(IDS)=PT2IN(IDT)
 
      CALL ARCHKI(IDS,IOK)
      IF (IOK.EQ.0.AND.PT2IN(IDS).GT.PARA(3)**2) THEN
        QDONE(IDS)=.FALSE.
      ENDIF
 
      RETURN
 
C**** END OF AREMIT ****************************************************
      END
