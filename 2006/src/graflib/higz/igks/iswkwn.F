*
* $Id: iswkwn.F,v 1.2 1996/06/05 10:21:03 cernlib Exp $
*
* $Log: iswkwn.F,v $
* Revision 1.2  1996/06/05 10:21:03  cernlib
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
      SUBROUTINE ISWKWN(WKID,X1,X2,Y1,Y2)
*.===========>
*.
*.   This routine sets the workstation transformation window .
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Workstation identifier .
*.  REAL X1,Y1 : X,Y down left corner window coordinates .
*.  REAL X2,Y2 : X,Y right up corner window coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      INTEGER WKID
*.______________________________________
*
      IF((X1.LT.0).OR.(X1.GT.1)
     +            .OR.
     +   (X2.LT.0).OR.(X2.GT.1)
     +            .OR.
     +   (Y1.LT.0).OR.(Y1.GT.1)
     +            .OR.
     +   (Y2.LT.0).OR.(Y2.GT.1))THEN
         CALL IGERR('Bad window or viewport size','ISWKWN')
         RETURN
      ENDIF
*
      RDWXMI=X1
      RDWXMA=X2
      RDWYMI=Y1
      RDWYMA=Y2
      IF(IGIWTY(WKID).LT.0)RETURN
*
#if defined(CERNLIB_GKS)
      IF(GFLAG)CALL GSWKWN(WKID,X1,X2,Y1,Y2)
#endif
*
      END
#endif
