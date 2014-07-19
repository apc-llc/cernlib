*
* $Id: argqcd.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: argqcd.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argqcd.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARGQCD(IDIN)
 
C...ARiadne subroutine Generate pt2 for QCD emission.
 
C...Generates a p_t^2 for a possible QCD emission from dipole ID
 
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
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
C...Copy some information from dipole record if ID < 1 these information
C...Has already been copied
C...S      = the invariant mass squared
C...W      = total energy in dipole
C...XT2MP  = maximum allowed fractional p_t^2 (x_t^2) for restricted
C...         phase space option
C...QQ1(3) = Boolean variable 'is quark' for parton 1(3)
C...QE1(3) = true if parton 1(3) is extended
C...ALP1(3)= alpha parameter of parton 1(3)
C...XMU1(3)= mu parameter of parton 1(3)
C...SY1(3) = fractional mass of parton 1(3)
 
      IF (IDIN.GT.0) THEN
        ID=IDIN
        PT2IN(ID)=0.0
        S=SDIP(ID)
        IF (S.LT.4.0*PARA(3)**2) RETURN
        W=SQRT(S)
        XT2MP=PT2LST/S
        QQ1=QQ(IP1(ID))
        QQ3=QQ(IP3(ID))
        QE1=QEX(IP1(ID))
        QE3=QEX(IP3(ID))
        ALP1=XPA(IP1(ID))
        ALP3=XPA(IP3(ID))
        XMU1=XPMU(IP1(ID))
        XMU3=XPMU(IP3(ID))
        SY1=BP(IP1(ID),5)/W
        SY3=BP(IP3(ID),5)/W
        IFL1=IFL(IP1(ID))
        IFL3=IFL(IP3(ID))
        XT2GG1=-1.0
        XT2GG3=-1.0
        IF ((.NOT.QQ1).AND.(.NOT.QE1)) THEN
          XT2GG1=XT2MP
          IF (INO(IP1(ID)).NE.1) XT2GG1=PT2GG(IP1(ID))/S
        ENDIF
        IF ((.NOT.QQ3).AND.(.NOT.QE3)) THEN
          XT2GG3=XT2MP
          IF (INO(IP3(ID)).NE.1) XT2GG3=PT2GG(IP3(ID))/S
        ENDIF
        IF (PARA(19).LT.0.0) CALL ARPRGC(ID)
      ELSE
        ID=-IDIN
        PT2IN(ID)=0.0
        IF (S.LT.4.0*PARA(3)**2) RETURN
        W=SQRT(S)
      ENDIF
 
      IF (S.LT.4.0*PARA(3)**2) RETURN
 
C...XLAM = scaled lambda_QCD squared
      XLAM2=PARA(1)**2/S
 
C...alpha_0 for alpha_QCD = alpha_0/ln(p_t^2/lambda_QCD^2)
      XNUMFL=MAX(ARNOFL(W,MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)
 
C...Normal gluon emission
      CALL ARGQCG(ID)
 
C...q-qbar emission
      IF (MSTA(15).GT.0) CALL ARGQCQ(ID)
 
      RETURN
 
C**** END OF ARGQCD ****************************************************
      END
