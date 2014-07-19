*
* $Id: arndx2.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
*
* $Log: arndx2.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:03  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
 
      REAL FUNCTION ARNDX2()
 
C...Ariadne function RNDom Xt2 version 2
 
C...Generate an x_t^2 according to a Sudakov suppressed distribution.
C...Suitable for constant alpha_QCD and QED emission
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
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
 
 
      ARNDX2=0.0
      ARG=RLU(IDUM)
      IF (CN*LOG(ARG).LT.(LOG(XT2M))**2-(LOG(XT2C))**2) RETURN
      ARNDX2=EXP(-SQRT((LOG(XT2M))**2-LOG(ARG)*CN))
 
      RETURN
 
C**** END OF ARNDX2 ****************************************************
      END
