*
* $Id: armade.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: armade.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: armade.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARMADE
 
C...ARiadne subroutine set MAss DEpendencies
 
C...Sets some mass dependencies needed for ARMCDI
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARINT1/ BC1,BC3,BZM,BZP,BP1,BM1,BP3,BM3,
     $                B1,B2,B3,XT2,XT,Y,QQ1,QQ3,NE1,NE3,
     $                S,W,C,CN,ALPHA0,XLAM2,IFLG,
     $                XT2MP,XT2ME,XT2M,XT2C,XTS,XT3,XT1,
     $                YINT,YMAX,YMIN,
     $                Y1,Y2,Y3,SY1,SY2,SY3,SSY,
     $                AE1,AE3,NXP1,NXP3,FQ1,FQ3
      SAVE /ARINT1/
 
 
      SSY=SY1+SY2+SY3
      Y1=SY1**2
      Y2=SY2**2
      Y3=SY3**2
 
      BC1=-DBLE(Y1)-1.0D0+DBLE(SY2+SY3)**2
      BC3=-DBLE(Y3)-1.0D0+DBLE(SY2+SY1)**2
      XT2M=0.0
      IF(SQRT(0.25+Y2)-1.0-(BC1+BC3)/2.0.LT.0.0) RETURN
      XTS=(SQRT(0.25+Y2)-1.0-(BC1+BC3)/2.0)**2
      XT1=-2.0*SY1-BC1
      XT3=-2.0*SY3-BC3
      IF(XT1.LT.0.0) RETURN
      IF(XT3.LT.0.0) RETURN
      XT2M=MIN(XTS,XT1*XT3)
 
      BZP=0.5*(1.0+Y1-Y3+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
      BZM=0.5*(1.0+Y3-Y1+SQRT(1.0+(Y1-Y3)**2-2.0*(Y1+Y3)))
 
      RETURN
 
C**** END OF ARMADE ****************************************************
      END
