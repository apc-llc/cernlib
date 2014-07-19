*
* $Id: iuwk.F,v 1.2 1996/06/05 10:21:05 cernlib Exp $
*
* $Log: iuwk.F,v $
* Revision 1.2  1996/06/05 10:21:05  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.12/16 13/08/91  16.29.43  by  O.Couet
*-- Author :
      SUBROUTINE IUWK(IWKID,IRFLG)
*.===========>
*.
*.   Update workstation IWKID. Send all buffered output to the
*. screen and process pending changes in the view.
*.
*. _Input parameters:
*.
*.  INTEGER IWKID : Workstation identifier (0= all workstation)
*.  INTEGER IRFLG : Regeneration flag:
*.                 0 = postpone update workstation (GKS only)
*.                 1 = refresh entire display
*.                 2 = update current view
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
*.______________________________________
*
      IF(IWKID.NE.0)THEN
         IF(IGIWTY(IWKID).GT.0.AND.GFLAG)CALL GUWK(IWKID,IRFLG)
      ELSE
         DO 10 I=1,INOPWK
            IF(IGIWTY(ILOPWK(I)).GT.0.AND.GFLAG)
     +         CALL GUWK(ILOPWK(I),IRFLG)
   10    CONTINUE
      ENDIF
*
      END
#endif
