*
* $Id: arvet5.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arvet5.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
 
      REAL FUNCTION ARVET5()
 
C...ARiadne function VETo factor version 5
 
C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for q-qbar emission
 
 
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
 
 
      ARVET5=((1.0D0-B3+Y3)**2+(1.0D0-B2+Y2)**2)*XT*
     $         (EXP(-YMIN)-EXP(-YMAX))/YINT
 
      RETURN
 
C**** END OF ARVET5 ****************************************************
      END
