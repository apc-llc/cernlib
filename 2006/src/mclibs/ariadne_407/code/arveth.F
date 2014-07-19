*
* $Id: arveth.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arveth.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
 
      REAL FUNCTION ARVETH()
 
C...ARiadne function Heavy VETo factor
 
C...Extra acceptance factor for heavy dipoles
 
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
 
 
      ARVETH=0.0
      BX1=1.0-B1+Y1-Y3
      BX3=1.0-B3+Y3-Y1
      IF (B2.GE.1.0.OR.BX1.LE.0.OR.BX3.LE.0) RETURN
      BXM=BX1/BX3
      ARVETH=1.0-(Y1*BXM+Y3/BXM)/(1.0-B2)
 
      RETURN
 
C**** END OF ARVETH ****************************************************
      END
