*
* $Id: arvet1.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arvet1.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arvet1.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      REAL FUNCTION ARVET1()
 
C...ARiadne function VETo factor version 1
 
C...Determine the acceptance factor for chosen x_t^2 and y
C...Suitable for photon emission with constant alpha_EM
 
 
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
 
 
      ARVET1=0.0
      IF(B2.LE.0) RETURN
      ARVET1=-((FQ1*(1.0-B1)/B2-FQ3*(1.0-B3)/B2)**2)*
     $         (B1**NXP1+B3**NXP3)*(YMAX-YMIN)*0.5/LOG(XT2)
 
      IF(MSTA(19).EQ.0) RETURN
 
      ARVET1=ARVET1*ARVETH()
 
      RETURN
 
C**** END OF ARVET1 ****************************************************
      END
