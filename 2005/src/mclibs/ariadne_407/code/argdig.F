*
* $Id: argdig.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: argdig.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argdig.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARGDIG(ID)
 
C...ARiadne subroutine Generate first Deep Inelastic Scattering emission
 
C...Generates a p-t^2 for a possible emission from an original dipole in
C...a DIS scattering according to O(alpha_S) matrix elements
 
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
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
      EXTERNAL ARNDX1,ARNDX2,ARNDY2,ARVET6,ARVET7
      REAL ARNDX1,ARNDX2,ARNDY2,ARVET6,ARVET7
 
 
C...First gluon emission
      SY2=0.0
      IFLG=0
 
C...Calculate mass dependent parameters
      CALL ARMADE
 
C...C = colour factors etc. in cross section
      C=2.0*(3.0+4.0*YFAC/27.0)/(3.0*PARU(1))
      IF (MHAR(116).LE.0) THEN
        C=2.0*(6.0+0.25*YFAC)/(3.0*PARU(1))
      ENDIF
 
C...Minimum x_t^2
      XT2C=MAX(PT2IN(ID),PARA(3)**2)/S
      XT2=0.0
 
      IF (XT2M.LE.XT2C) GOTO 900
 
C...Set additional parameters and call the veto algorith with
C...Suitable random functions
      IF (MSTA(12).GT.0) THEN
C.......Running alpha_QDC
        YINT=2.0*LOG(0.5/SQRT(XLAM2)+SQRT(0.25/XLAM2-1.0))
        CN=1.0/(YINT*C*ALPHA0)
        CALL ARMCDI(ARNDX1,ARNDY2,ARVET6)
      ELSE
C.......Constant alpha_QCD
        YINT=1.0
        CN=2.0/(C*PARA(2))
        CALL ARMCDI(ARNDX2,ARNDY2,ARVET7)
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
 
 
      RETURN
 
C**** END OF ARGDIG ****************************************************
      END
