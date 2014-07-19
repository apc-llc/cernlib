*
* $Id: argqcg.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: argqcg.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argqcg.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARGQCG(ID)
 
C...ARiadne subroutine Generate pt2 for QCD emission.
 
C...Generates a p_t^2 for a possible QCD emission from dipole ID
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
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
      COMMON /ARINT2/ DBEX,DBEY,DBEZ,PHI,THE
      SAVE /ARINT2/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      EXTERNAL ARNDX1,ARNDX2,ARNDY1,ARNDY2,ARVET3,ARVET4
      REAL ARNDX1,ARNDX2,ARNDY1,ARNDY2,ARVET3,ARVET4
 
 
C...C = colour factors etc. in cross section
      C=6.0/(4.0*PARU(1))
      IF (QQ1.AND.QQ3) C=4.0/(3.0*PARU(1))
      SY2=0.0
      SQ2=0.0
 
C...Set exponents in cross section
      NXP1=3
      NXP3=3
      IF (QQ1) NXP1=2
      IF (QQ3) NXP3=2
 
C...Flavour of this emission 0 = gluon emission
      IFLG=0
 
C...Calculate mass dependent parameters
      CALL ARMADE
 
C...Allow extra phase space when Drell-Yan process
      QEXDY=.FALSE.
      IF (((ABS(MHAR(123)).EQ.1.AND.IO.EQ.0).OR.MHAR(123).GT.1.OR.
     $     (MHAR(131).NE.0.and.IO.EQ.0))
     $     .AND.QQ(MAXPAR-2)) THEN
        QEXDY=.TRUE.
        IDY=MAXPAR-2
        CALL ARBOCM(ID)
        CALL AROBO1(0.0,0.0,-DBEX,-DBEY,-DBEZ,IDY)
        CALL AROBO1(0.0,-PHI,0.0D0,0.0D0,0.0D0,IDY)
        CALL AROBO1(-THE,0.0,0.0D0,0.0D0,0.0D0,IDY)
        BPDY=BP(IDY,4)+BP(IDY,3)
        BMDY=BP(IDY,4)-BP(IDY,3)
        CALL AROBO1(THE,PHI,DBEX,DBEY,DBEZ,IDY)
        CALL AROBO2(0.0,0.0,DBEX,DBEY,DBEZ,IP1(ID),IP3(ID))
      ENDIF
 
C...Minimum x_t^2
      XT2C=MAX(PT2IN(ID),PARA(3)**2)/S
      XT2=0.0
 
C...Set maximum x_t^2
      IF (MSTA(11).LT.4) XT2M=MIN(XT2M,XT2MP)
 
      IF (XT2M.LE.XT2C) GOTO 900
 
C...Set additional parameters and call the veto algorith with
C...Suitable random functions
      IF (MSTA(12).GT.0) THEN
C.......Running alpha_QDC
        YINT=2.0*LOG(0.5/SQRT(XLAM2)+SQRT(0.25/XLAM2-1.0))
        CN=1.0/(YINT*C*ALPHA0)
        IF (QE1.OR.QE3) THEN
C.........Extended dipole
          CALL ARMCDI(ARNDX1,ARNDY2,ARVET4)
        ELSE
C.........Pointlike dipole
          CALL ARMCDI(ARNDX1,ARNDY1,ARVET4)
        ENDIF
      ELSE
C.......Constant alpha_QCD
        YINT=1.0
        CN=2.0/(C*PARA(2))
        IF (QE1.OR.QE3) THEN
C.........Extended dipole
          CALL ARMCDI(ARNDX2,ARNDY2,ARVET3)
        ELSE
C.........Pointlike dipole
          CALL ARMCDI(ARNDX2,ARNDY1,ARVET3)
        ENDIF
      ENDIF
 
C...Save the generated values of p_t^2, x1, x3, a1 and a3
      IF (XT2.GT.XT2C) THEN
        PT2IN(ID)=XT2*S
        BX1(ID)=B1
        BX3(ID)=B3
        AEX1(ID)=AE1
        AEX3(ID)=AE3
        IRAD(ID)=0
      ENDIF
 
 900  CONTINUE
 
      QEXDY=.FALSE.
 
      RETURN
 
C**** END OF ARGQCG ****************************************************
      END
