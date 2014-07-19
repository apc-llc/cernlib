*
* $Id: armade.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: armade.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: armade.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      SUBROUTINE ARMADE
 
C...ARiadne subroutine set MAss DEpendencies
 
C...Sets some mass dependencies needed for ARMCDI
 
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
 
 
      SSY=SY1+SY2+SY3
      Y1=SY1**2
      Y2=SY2**2
      Y3=SY3**2
 
      BC1=DBLE(Y1)+1.0D0-DBLE(SY2+SY3)**2
      IF (IFLG.GT.0.AND.MSTA(23).EQ.2) BC1=DBLE(Y1)+1.0D0
      BC3=DBLE(Y3)+1.0D0-DBLE(SY2+SY1)**2
      XT2M=0.0
      IF (SQRT(0.25+Y2)-1.0+(BC1+BC3)/2.0.LT.0.0) RETURN
      XTS=(SQRT(0.25+Y2)-1.0+(BC1+BC3)/2.0)**2
      XT1=BC1-2.0*SY1
      XT3=BC3-2.0*SY3
      IF (XT1.LT.0.0) RETURN
      IF (XT3.LT.0.0) RETURN
      XT2M=MIN(XTS,XT1*XT3)
 
      BZP=0.5*(1.0+Y1-Y3+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
      BZM=0.5*(1.0+Y3-Y1+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
 
      RETURN
 
C**** END OF ARMADE ****************************************************
      END
