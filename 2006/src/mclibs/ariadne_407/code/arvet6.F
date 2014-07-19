*
* $Id: arvet6.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arvet6.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
 
      REAL FUNCTION ARVET6()
 
C...ARiadne function VETo factor version 6
 
C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for gluon emission with running alpha_QCD according to
C...O(alpha_S) gluon emission for lepto production
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
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
      COMMON /ARINT4/ BASS(5),BASSX1,BASSX3,IFLASS
      SAVE /ARINT4/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
 
 
      BCNST0=0.0
      Y3P=ZSQEV*Y3
      Y2P=ZSQEV*Y2
      Y1P=ZSQEV*Y1
      B3P=1.0-ZSQEV*(1.0-B3)
      B1P=B1-(Y1P-Y3P)*(1.0-ZSQEV)/ZSQEV
      B2P=2.0-B1P-B3P
      BASSX1=B1P
      BASSX3=B3P
 
      B13=MAX(1.0-B3P+Y3P,BCNST0)
      B12=MAX(1.0-B2P+Y2P,BCNST0)
      B11=MAX(1.0-B1P+Y1P,BCNST0)
      B13Q=B13+SQ2
      CG1=((B12/B3P)**2+(SQ2/B13Q)**2)*SQ2/B13Q
      CG2=2.0*(B12*SQ2/(B13Q*B3P)+1.0)*B13*B11*SQ2/(B3P*B13Q**2)
      CG3=4.0*YFAC*B11*B12*B13*(SQ2**2)/((B3P**2)*B13Q**3)
      CG0=(3.0+4.0*YFAC/27.0)
      IF (MHAR(116).LE.0) THEN
        CG1=(B12/B3P)**2+(SQ2/B13Q)**2
        CG2=2.0*(B12*SQ2/(B13Q*B3P)+1.0)*B13*B11/(B3P*B13Q)
        CG3=4.0*YFAC*B11*B12*B13*SQ2/((B3P**2)*B13Q**2)
        CG0=(6.0+0.25*YFAC)
        IF (MHAR(116).LT.0) CG0=CG0*SQRT(B13Q/SQ2)
      ENDIF
 
      ARVET6=(YMAX-YMIN)*(CG1+CG2+CG3)/(YINT*CG0)
 
      IF (MSTA(19).EQ.0) RETURN
 
      IF (MSTA(19).EQ.2) ARVET6=ARVET6*
     $     MIN(1.0,LOG(XT2/XLAM2)/LOG(PARA(21)*SQ2/XLAM2))
 
      ARVET6=ARVET6*ARVETH()
 
 
      RETURN
 
C**** END OF ARVET6 ****************************************************
      END
