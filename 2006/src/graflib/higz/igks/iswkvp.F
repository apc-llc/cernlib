*
* $Id: iswkvp.F,v 1.2 1996/06/05 10:21:02 cernlib Exp $
*
* $Log: iswkvp.F,v $
* Revision 1.2  1996/06/05 10:21:02  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.06/03 12/12/88  15.20.46  by  O.Couet
*-- Author :
      SUBROUTINE ISWKVP(WKID,X1,X2,Y1,Y2)
*.===========>
*.
*.   This routine sets the workstation transformation viewport .
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  REAL X1,Y1 : X,Y down left corner viewport coordinates .
*.  REAL X2,Y2 : X,Y right up corner viewport coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      INTEGER WKID
*.______________________________________
*
      RDVXMI=X1
      RDVXMA=X2
      RDVYMI=Y1
      RDVYMA=Y2
      IF(IGIWTY(WKID).LT.0)RETURN
*
#if defined(CERNLIB_GKS)
      IF(GFLAG)CALL GSWKVP(WKID,X1,X2,Y1,Y2)
#endif
*
       END
#endif
