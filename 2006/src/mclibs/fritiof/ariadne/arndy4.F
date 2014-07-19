*
* $Id: arndy4.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arndy4.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
 
      REAL FUNCTION ARNDY4()
 
C...Ariadne function RaNDom Y version 4
 
C...Generates a properly distributed Y
C...Suitable for q-qbar emission from extended dipole
 
 
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
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
      ZMAX=SQRT(XTS/XT2)+SQRT(MAX(XTS/XT2-1.0,0.0))
      ZMIN=MIN(ZMAX,XT1/XT)
      ZMAX=MIN(ZMAX,XT3/XT)
 
      AE1=1.0
      AE3=1.0
      IF(NE1.GT.0) AE1=(PARA(10+NE1)/(XT*W))**PARA(10)
      IF(NE3.GT.0) AE3=(PARA(10+NE3)/(XT*W))**PARA(10)
      BP1=(1.0-AE1)*BZP
      IF(BP1.LE.SY1) THEN
        BP1=0.0
        BM1=0.0
      ELSE
        BM1=Y1/BP1
      ENDIF
      BM3=(1.0-AE3)*BZM
      IF(BM3.LE.SY3) THEN
        BM3=0.0
        BP3=0.0
      ELSE
        BP3=Y3/BM3
      ENDIF
      AZ1=1.0-BP1-BP3
      AZ3=1.0-BM1-BM3
      A=(0.5+SQRT(MAX(0.25-XT2/(AZ1*AZ3),0.0)))/XT
 
      ZMAX=MIN(ZMAX,ABS(AZ1)*A)
      ZMIN=MIN(ZMIN,ABS(AZ3)*A)
 
      YMAX=LOG(ZMAX)
      YMIN=-LOG(ZMIN)
 
      ARNDY4=-LOG(1.0/ZMAX+RLU(IDUM)*(ZMIN-1.0/ZMAX))
 
      RETURN
 
C**** END OF ARNDY4 ****************************************************
      END
