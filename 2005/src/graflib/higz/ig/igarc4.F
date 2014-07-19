*
* $Id: igarc4.F,v 1.1.1.1 1996/02/14 13:10:32 mclareni Exp $
*
* $Log: igarc4.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:32  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/05 27/01/95  16.04.37  by  O.Couet
*-- Author :
      SUBROUTINE IGARC4(XC,YC,R1,R2,PHI1,PHI2)
*.===========>
*.
*.   Underlaying routine for IGARC .
*.
*..==========> (O.Couet R.Nierhaus)
#include "higz/higraf.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      LOGICAL ZFSAV
      PARAMETER (PI=3.1415926,PI2=2*PI)
      PARAMETER (EPSIL=0.0001)
*.______________________________________
*
      ANGLE = PHI2-PHI1
      IF (ANGLE.LT.0.) ANGLE=PI2+ANGLE
      NPOINT = INT((99./2.-1)*ANGLE/PI2+.5)
      NPOINT = MAX(1,NPOINT)
      DELTA  = ANGLE/FLOAT(NPOINT)
      XX     = COS(PHI1)
      YY     = SIN(PHI1)
      TCOS   = COS(DELTA)
      TSIN   = SIN(DELTA)
*
      DO 10 I=1,NPOINT+1
         XGRAF(I)            = XC+R1*XX
         YGRAF(I)            = YC+R1*YY
         XGRAF(2*NPOINT+3-I) = XC+R2*XX
         YGRAF(2*NPOINT+3-I) = YC+R2*YY
         XXX                 = XX*TCOS-YY*TSIN
         YY                  = XX*TSIN+YY*TCOS
         XX                  = XXX
   10 CONTINUE
*
      ZFSAV  = ZFLAG
      ZFLAG  = .FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      CALL IFA(2*NPOINT+2,XGRAF,YGRAF)
*
      IF ((IBORD.NE.0).AND.(IFAIS.NE.0)) THEN
         IF (ABS(XGRAF(1)-XGRAF(NPOINT+1)).GT.EPSIL.OR.
     +       ABS(YGRAF(1)-YGRAF(NPOINT+1)).GT.EPSIL) THEN
            NPL = 2*NPOINT+3
            IF (NPL.LE.NPMAX) THEN
               XGRAF(NPL) = XGRAF(1)
               YGRAF(NPL) = YGRAF(1)
               CALL IPL(NPL,XGRAF,YGRAF)
            ELSE
               CALL IGERR('Too many points','IGARC')
            ENDIF
         ELSE
            CALL IPL(NPOINT+1,XGRAF,YGRAF)
            CALL IPL(NPOINT+1,XGRAF(NPOINT+2),YGRAF(NPOINT+2))
         ENDIF
      ENDIF
*
      ZFLAG  = ZFSAV
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
