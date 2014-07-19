*
* $Id: igarc1.F,v 1.1.1.1 1996/02/14 13:10:31 mclareni Exp $
*
* $Log: igarc1.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 06/12/91  17.15.23  by  O.Couet
*-- Author :
      SUBROUTINE IGARC1(XC,YC,R,PHI1,PHI2)
*.===========>
*.
*.   Underlaying routine for IGARC .
*.
*..==========> (O.Couet R.Nierhaus)
#include "higz/higraf.inc"
#include "higz/hiflag.inc"
      LOGICAL ZFSAV
      PARAMETER (PI=3.1415926,PI2=2*PI)
*.______________________________________
*

      ANGLE=PHI2-PHI1
      IF(ANGLE.LT.0.)ANGLE=PI2+ANGLE
      NPOINT=INT(99.*ANGLE/PI2+.5)
      NPOINT=MAX(1,NPOINT)
      DELTA=ANGLE/FLOAT(NPOINT)
      XX=COS(PHI1)
      YY=SIN(PHI1)
      TCOS=COS(DELTA)
      TSIN=SIN(DELTA)
      NPOINT=NPOINT+1
      DO 10 I=1,NPOINT
         XGRAF(I)=XC+R*XX
         YGRAF(I)=YC+R*YY
         XXX=XX*TCOS-YY*TSIN
         YY=XX*TSIN+YY*TCOS
         XX=XXX
   10 CONTINUE
      ZFSAV=ZFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL IPL(NPOINT,XGRAF,YGRAF)
      ZFLAG=ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
