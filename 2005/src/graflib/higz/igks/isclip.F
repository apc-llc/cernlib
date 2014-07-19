*
* $Id: isclip.F,v 1.2 1996/06/05 10:20:41 cernlib Exp $
*
* $Log: isclip.F,v $
* Revision 1.2  1996/06/05 10:20:41  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:45  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.21/01 11/05/94  13.30.36  by  O.Couet
*-- Author :
      SUBROUTINE ISCLIP(CLSW)
*.===========>
*.
*.   This routine sets the window (in the world space) clipping on or off .
*.
*. _Input parameters:
*.
*.  INTEGER CLSW : Clipping indicator .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      INTEGER CLSW
*.______________________________________
*
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSCLI(CLSW)
#endif
      IF(CLSW.EQ.ICLIP)RETURN
      ICLIP=CLSW
      IF(GFLAG)CALL GSCLIP(CLSW)
*
      END
#endif
