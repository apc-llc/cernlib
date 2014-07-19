*
* $Id: izgnt.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izgnt.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
*CMZ :  1.19/10 15/09/93  09.46.39  by  O.Couet
*-- Author :
      SUBROUTINE IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
*.===========>
*.
*.   This routine searchs in the NT storage if the transformation NT
*. exits. And returns the adress and the priority of the transformation
*. NT if IFIND.NE.0 .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalisation transformation number .
*.
*. _Output parameters:
*.
*. INTEGER IFIND : IFIND.NE.0 if NT already exist (IFIND = Adress of the
*.                 priority word).
*. INTEGER IAWV : Transformation parameters adress .
*. INTEGER IPRIO : NT priority .
*. INTEGER IHPRIO : Highest NT priority .
*.
*..==========> (O.Couet, R.Wilhelm)
#include "higz/hint.inc"
#include "higz/hiatt.inc"
*.______________________________________
*
      IFIND=0
      IHPRIO=NT0PRI
      IF(NTSTOR(1).EQ.0)RETURN
*
      DO 10 I=0,NTSTOR(1)-1
         IOFF=INWDNT*I
         IOF3=IOFF+3
         IF(NTSTOR(2+IOFF).EQ.NT)THEN
            IFIND=IOF3
            IPRIO=NTSTOR(IFIND)
            IAWV=I*8+1
         ENDIF
         IF(NTSTOR(IOF3).GT.IHPRIO)IHPRIO=NTSTOR(IOF3)
   10 CONTINUE
*
      END
#endif
