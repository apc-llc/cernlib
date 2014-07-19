*
* $Id: arprgc.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: arprgc.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arprgc.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARPRGC(ID)
 
C...ARiadne subroutine PRepare Gluon Check.
 
C...Set up variables to be used by ARCHKI to perform safety check on
C...gluon emission to prevent trouble for subsequent initial state gluon
C...splitting.
 
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
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /ARLIST/ B1SAVE(2),B3SAVE(2),IPTOT(MAXPAR),NPTOT,
     $     IPSTQ(MAXPAR),NPSTQ,IPREM(MAXPAR),NPREM,IRDIR(2),
     $     YIQQ(2),PT2IQQ(2),PT2SAV(2),IRASAV(2),A1SAVE(2),A3SAVE(2)
 
      SAVE /ARLIST/
      COMMON /LUJETS/ N,K(4000,5),P(4000,5),V(4000,5)
      SAVE /LUJETS/
 
 
      KQ1=0
      KQ3=0
      IF (MSTA(32).LT.2) RETURN
 
      IF (.NOT.(QQ(MAXPAR-3).OR.QQ(MAXPAR-4))) RETURN
      IRP=MAXPAR-3
      IF (QQ(IRP)) THEN
        IT=2
        IDIR=IRDIR(IT)
        IR=INQ(IRP)
        PMT=P(IT,4)+IDIR*P(IT,3)
        IF (IR.EQ.IP1(ID)) THEN
          KF1=K(IT,2)
          KQ1=IDO(IRP)
          BMRP1=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR1=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ELSEIF (IR.EQ.IP3(ID)) THEN
          KF3=K(IT,2)
          KQ3=IDO(IRP)
          BMRP3=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR3=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ENDIF
      ENDIF
      IRP=MAXPAR-4
      IF (QQ(IRP)) THEN
        IT=1
        IDIR=IRDIR(IT)
        IR=INQ(IRP)
        PMT=P(IT,4)+IDIR*P(IT,3)
        IF (IR.EQ.IP1(ID)) THEN
          KF1=K(IT,2)
          KQ1=IDO(IRP)
          BMRP1=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR1=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ELSEIF (IR.EQ.IP3(ID)) THEN
          KF3=K(IT,2)
          KQ3=IDO(IRP)
          BMRP3=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR3=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ENDIF
      ENDIF
 
      RETURN
 
C**** END OF ARGQCQ ****************************************************
      END
