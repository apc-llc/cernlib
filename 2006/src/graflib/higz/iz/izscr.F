*
* $Id: izscr.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: izscr.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.04  by  O.Couet
*-- Author :
      SUBROUTINE IZSCR(PNAME,ICYCLE)
*.===========>
*.
*.   This routine manages the SCRatch of the pictures on RZ files .
*.
*. _Input parameters:
*.
*.  CHARACTER PNAME : Picture name .
*.  INTEGER ICYCLE : Number of version of the picture on the RZ file .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
      CHARACTER*(*) PNAME
      DIMENSION NAME(5)
*.______________________________________
*
      LPSAV=LPICT
      IF(LHIGZ.EQ.0)THEN
         CALL MZBOOK(IXHIGZ,LPICT,LHIGZ,1,'PICT',1,1,5,2,0)
      ENDIF
      NCH=LENOCC(PNAME)
      CALL VBLANK(NAME,5)
      CALL UCTOH(PNAME,NAME,4,NCH)
*
      CALL RZDELK(NAME,ICYCLE,' ')
      CALL RZSAVE
*
      END
#endif
