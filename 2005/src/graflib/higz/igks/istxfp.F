*
* $Id: istxfp.F,v 1.3 1998/01/30 15:22:31 couet Exp $
*
* $Log: istxfp.F,v $
* Revision 1.3  1998/01/30 15:22:31  couet
* - APOLLO version removed
*
* Revision 1.2  1996/06/05 10:21:00  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/06 16/03/89  08.58.19  by  O.Couet
* SUN Version available
*-- Author :
      SUBROUTINE ISTXFP(JFONT,JPREC)
*.===========>
*.
*.   This routine sets the text font and precision .
*.
*. _Input parameters:
*.
*.  INTEGER FONT : Text font .
*.  INTEGER PREC : Text precision .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF((JFONT.EQ.IFONT).AND.(JPREC.EQ.IPREC))RETURN
      IFONT=JFONT
      IPREC=JPREC
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IFTPR=10*JFONT+ISIGN(1,JFONT)*JPREC
      IF(GLFLAG)CALL IZATT(ITXFCO)
#endif
      IF((JFONT.EQ.0).AND.(JPREC.EQ.2))RETURN
      IF(GFLAG)CALL GSTXFP(JFONT,JPREC)
*
      END
#endif
