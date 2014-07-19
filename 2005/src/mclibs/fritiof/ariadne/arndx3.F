*
* $Id: arndx3.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arndx3.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
 
      REAL FUNCTION ARNDX3()
 
C...Ariadne function RNDom Xt2 version 3
 
C...Generate an x_t^2 according to a Sudakov suppressed distribution.
C...Suitable for constant alpha_QCD q-qbar emission
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,NE1,NE3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,
     $                XT2MP,XT2ME,XT2M,XT2C,XTS,XT3,XT1,
     $                YINT,YMAX,YMIN,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3
      SAVE /ARINT1/
 
 
      ARNDX3=0.0
      ARG=RLU(IDUM)
      IF(LOG(ARG)*CN.LT.LOG(XT2C/XT2M)) RETURN
      ARNDX3=XT2M*(ARG**CN)
 
      RETURN
 
C**** END OF ARNDX3 ****************************************************
      END
