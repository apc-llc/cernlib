*
* $Id: pautit.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: pautit.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/06 03/07/95  17.57.20  by  O.Couet
*-- Author :    O.Couet   26/06/90
      SUBROUTINE PAUTIT(CHTIT)
*.===========>
*.
*.   Plot the user title if the option 'UTIT' is on
*. or draw CHTIT as histogram title if it is not ' '.
*.
*..==========>
#include "paw/pcchar.inc"
#include "paw/quest.inc"
#include "paw/pcwk.inc"
      CHARACTER*(*) CHTIT
*.______________________________________
*
      IF (IWK.NE.0) THEN
         CALL HPLOPT('UTIT',-1)
         IF (IQUEST(11).NE.0) THEN
            IF (CHUTIT.NE.' ') CALL HPLTIT(CHUTIT)
         ELSE
            IF (CHTIT.NE.' ') CALL HPLTIT(CHTIT)
         ENDIF
      ENDIF
*
      END
