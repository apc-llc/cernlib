*
* $Id: ismk.F,v 1.2 1996/06/05 10:20:55 cernlib Exp $
*
* $Log: ismk.F,v $
* Revision 1.2  1996/06/05 10:20:55  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.10/02 28/05/90  10.30.19  by  O.Couet
*-- Author :
      SUBROUTINE ISMK(MTYPE)
*.===========>
*.
*.   This routine sets the marker type .
*.
*. _Input parameters:
*.
*.  INTEGER MTYPE : Marker type .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
*.______________________________________
*
      IF(MTYPE.EQ.IMK.OR.MTYPE.GT.31)RETURN
      IMK=MTYPE
      IF(GFLAG)CALL GSMK(MTYPE)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IMKCO)
#endif
*
      END
#endif
