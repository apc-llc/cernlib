*
* $Id: iscr.F,v 1.2 1996/06/05 10:20:41 cernlib Exp $
*
* $Log: iscr.F,v $
* Revision 1.2  1996/06/05 10:20:41  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:45  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.10/01 04/05/90  10.43.35  by  O.Couet
*-- Author :
      SUBROUTINE ISCR(IWKID,ICI,CR,CG,CB)
*.===========>
*.
*.   This routine sets the color representation .
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  INTEGER CI : Color index .
*.  REAL CR : Red .
*.  REAL CG : Green .
*.  REAL CB : Blue .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hilut.inc"
*.______________________________________
*
      I=MIN(NBCLUT-1,ABS(ICI))+1
      REDLUT(I)=CR
      GRNLUT(I)=CG
      BLULUT(I)=CB
      CALL IGSG(0)
      IF(GFLAG)CALL GSCR(IWKID,ICI,CR,CG,CB)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZSCOR(IWKID,ICI,CR,CG,CB)
#endif
*
      END
#endif
