*
* $Id: igarc2.F,v 1.1.1.1 1996/02/14 13:10:32 mclareni Exp $
*
* $Log: igarc2.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:32  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/05 27/01/95  16.04.37  by  O.Couet
*-- Author :
      SUBROUTINE IGARC2(XC,YC,R)
*.===========>
*.
*.   Underlaying routine for IGARC .
*.
*..==========> (O.Couet R.Nierhaus)
#include "higz/higraf.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      LOGICAL ZFSAV
*.______________________________________
*
      NPOINT = 99
      DELTA  = 2.*3.14159/FLOAT(NPOINT)
      XX     = 1.
      YY     = 0.
      TCOS   = COS(DELTA)
      TSIN   = SIN(DELTA)
*
      DO 10 I=1,NPOINT
         XGRAF(I) = XC+R*XX
         YGRAF(I) = YC+R*YY
         XXX      = XX*TCOS-YY*TSIN
         YY       = XX*TSIN+YY*TCOS
         XX       = XXX
   10 CONTINUE
*
      ZFSAV  = ZFLAG
      ZFLAG  = .FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      CALL IFA(NPOINT,XGRAF,YGRAF)
*
      IF((IBORD.NE.0).AND.(IFAIS.NE.0))THEN
         NPOINT        = NPOINT+1
         XGRAF(NPOINT) = XGRAF(1)
         YGRAF(NPOINT) = YGRAF(1)
         CALL IPL(NPOINT,XGRAF,YGRAF)
      ENDIF
*
      ZFLAG  = ZFSAV
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
