*
* $Id: arndy4.F,v 1.1.1.1 1996/03/08 16:51:03 mclareni Exp $
*
* $Log: arndy4.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:03  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
 
      REAL FUNCTION ARNDY4()
 
C...Ariadne function RaNDom Y version 4
 
C...Generates a properly distributed Y
C...Suitable for q-qbar emission from extended dipole
 
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
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
      AE1=1.0
      AE3=1.0
      PTTRU1=XT*W
      PTTRU3=PTTRU1
      ARNDY4=ARNDY3()
      B1=BC1-XT*EXP(ARNDY4)
      B3=BC3-XT*EXP(-ARNDY4)
      B2=2.0-B1-B3
      IF (MSTA(25).EQ.2) THEN
        IF ((1.0-B1+Y1-(SY2+SY3)**2)*(1.0-B3+Y3-(SY1+SY2)**2).GE.
     $       0.25*(1.0-B2+Y2-(SY1+SY3)**2)) THEN
          PTTRU1=0.5*W
          PTTRU3=PTTRU1
        ELSE
          PTTRU1=W*SQRT((1.0-B1+Y1-(SY2+SY3)**2)*
     $         (1.0-B3+Y3-(SY1+SY2)**2)/(1.0-B2+Y2-(SY1+SY3)**2))
          PTTRU3=PTTRU1
        ENDIF
      ENDIF
      IF (MSTA(25).EQ.4.OR.MSTA(25).EQ.5) THEN
        BZ12=B1**2-4.0*Y1
        BZ22=B2**2-4.0*Y2
        BZ32=B3**2-4.0*Y3
        BCOS=2.0*BZ12*BZ22+2.0*BZ12*BZ32+2.0*BZ22*BZ32
     $       -BZ12**2-BZ22**2-BZ32**2
        IF (BCOS.LE.0.0) THEN
          QFAIL=.TRUE.
          RETURN
        ENDIF
        IF (MSTA(25).EQ.4) THEN
          PTTRU1=0.25*W*SQRT(BCOS/BZ32)
          PTTRU3=0.25*W*SQRT(BCOS/BZ12)
        ELSE
          PTTRU1=0.25*W*SQRT(BCOS/BZ12)
          PTTRU3=0.25*W*SQRT(BCOS/BZ32)
        ENDIF
      ENDIF
 
      IF (ALP1.GE.0.0) THEN
        ALPHA1=ALP1
        THEMU1=XMU1
      ELSEIF (PTTRU1.GT.ABS(ALP1)) THEN
        ALPHA1=2.0
        THEMU1=SQRT(ABS(XMU1*ALP1))
      ELSE
        ALPHA1=1.0
        THEMU1=XMU1
      ENDIF
      IF (ALP3.GE.0.0) THEN
        ALPHA3=ALP3
        THEMU3=XMU3
      ELSEIF (PTTRU3.GT.ABS(ALP3)) THEN
        ALPHA3=2.0
        THEMU3=SQRT(ABS(XMU3*ALP3))
      ELSE
        ALPHA3=1.0
        THEMU3=XMU3
      ENDIF
 
      IF (MSTA(25).GT.0) THEN
        IF (QE1) AE1=((THEMU1/PTTRU1)**ALPHA1)*
     $       (1.0/RLU(IDUM)-1.0)**PARA(25)
        IF (QE3) AE3=((THEMU3/PTTRU3)**ALPHA3)*
     $       (1.0/RLU(IDUM)-1.0)**PARA(25)
        IF (MSTA(25).EQ.3.AND.PTTRU1.LE.THEMU1) AE1=1.0
        IF (MSTA(25).EQ.3.AND.PTTRU3.LE.THEMU3) AE3=1.0
      ELSE
        IF (QE1) AE1=(THEMU1/PTTRU1)**ALPHA1
        IF (QE3) AE3=(THEMU3/PTTRU3)**ALPHA3
      ENDIF
 
      BP1=(1.0-AE1)*(BZP-SY1)+SY1
      IF (BP1.LE.SY1) THEN
        BP1=0.0
        BM1=0.0
      ELSE
        BM1=Y1/BP1
      ENDIF
      BM3=(1.0-AE3)*(BZM-SY3)+SY3
      IF (BM3.LE.SY3) THEN
        BM3=0.0
        BP3=0.0
      ELSE
        BP3=Y3/BM3
      ENDIF
 
      ZMAX=SQRT(XTS/XT2)+SQRT(MAX(XTS/XT2-1.0,0.0))
      ZMIN=MIN(ZMAX,XT1/XT)
      ZMAX=MIN(ZMAX,XT3/XT)
 
      AZ1=1.0-BP1-BP3
      AZ3=1.0-BM1-BM3
      A=0.5/XT
      IF (AZ1*AZ3.GT.0.0) A=(0.5+SQRT(MAX(0.25-XT2/(AZ1*AZ3),0.0)))/XT
 
      ZMAX=MIN(ZMAX,ABS(AZ1)*A)
      ZMIN=MIN(ZMIN,ABS(AZ3)*A)
 
      IF (ZMAX.LE.0.0.OR.ZMIN.LE.0.0) THEN
        QFAIL=.TRUE.
      ELSEIF (ARNDY4.GT.LOG(ZMAX).OR.ARNDY4.LT.-LOG(ZMIN)) THEN
        QFAIL=.TRUE.
      ENDIF
 
      RETURN
 
C**** END OF ARNDY4 ****************************************************
      END
