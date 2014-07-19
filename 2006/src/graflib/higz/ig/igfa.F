*
* $Id: igfa.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: igfa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.08/00 05/10/89  10.14.18  by  O.Couet
*-- Author :    O.Couet   27/09/89
      SUBROUTINE IGFA(N,X,Y)
*.===========>
*.
*.   This routine draws a fill area with HIGZ hatch style.
*. The convention for the Hatch is the following:
*.
*.            CALL ISFAIS(3)
*.            CALL ISFASI(ijk)
*.
*.    i (1-9) : specify the space between each hatch
*.              1 = 1/2mm  9 = 6mm
*.
*.    j (0-9) : specify angle between 0 and 90 degrees
*.
*.              0 = 0
*.              1 = 10
*.              2 = 20
*.              3 = 30
*.              4 = 45
*.              5 = Not drawn
*.              6 = 60
*.              7 = 70
*.              8 = 80
*.              9 = 90
*.
*.    k (0-9) : specify angle between 90 and 180 degrees
*.              0 = 180
*.              1 = 170
*.              2 = 160
*.              3 = 150
*.              4 = 135
*.              5 = Not drawn
*.              6 = 120
*.              7 = 110
*.              8 = 100
*.              9 = 90
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points .
*.  REAL X(N) : X coordinates .
*.  REAL Y(N) : Y coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*)
      DIMENSION ANG1(10),ANG2(10)
      LOGICAL ZFSAV,MFSAV
      DATA ANG1 /  0., 10., 20., 30., 45.,5., 60., 70., 80., 90./
      DATA ANG2 /180.,170.,160.,150.,135.,5.,120.,110.,100., 90./
*.______________________________________
*
      ZFSAV=ZFLAG
      MFSAV=MFLAG
      ZFLAG=.FALSE.
      MFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      IDSPA=INT(IFASI/100)
      IANG2=INT((IFASI-100*IDSPA)/10)
      IANG1=MOD(IFASI,10)
      DY=0.003*FLOAT(IDSPA)
      IANG1=IANG1+1
      IANG2=IANG2+1
      IF(ANG1(IANG1).NE.5.)CALL IGHATC(DY,ANG1(IANG1),N,X,Y)
      IF(ANG2(IANG2).NE.5.)CALL IGHATC(DY,ANG2(IANG2),N,X,Y)
*
      ZFLAG=ZFSAV
      MFLAG=MFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
