*
* $Id: dzdsfb.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdsfb.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSFB
*      IMPLICIT NONE
#include "dzdzbrinc.inc"
      CHARACTER*4 CHMASK
      INTEGER NCH,I
      CALL KUGETC(CHMASK,NCH)
      IF(NCH.GT.0)THEN
         CIDMSK=CHMASK
         DO 10 I=1,4
10       IF(CIDMSK(I:I).EQ.'*')CIDMSK(I:I)=' '
      ELSE
         CIDMSK=' '
      ENDIF
      CALL KUGETI(NIDMSK)
      END
***********************************************************************
