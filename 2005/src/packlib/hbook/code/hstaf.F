*
* $Id: hstaf.F,v 1.1.1.1 1996/01/16 17:07:48 mclareni Exp $
*
* $Log: hstaf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:48  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/05 29/11/93  14.18.24  by  Fons Rademakers
*-- Author :    Rene Brun   27/11/93
      SUBROUTINE HSTAF(CHOPTT)
*.==========>
*.           To set option for filling statistics
*..=========> ( R.Brun )
#include "hbook/hcopt.inc"
      CHARACTER*(*) CHOPTT
      CHARACTER*4 CHOPT
*.___________________________________________
      CHOPT=CHOPTT
      CALL CLTOU(CHOPT)
      IF(INDEX(CHOPT,'YES').NE.0)THEN
         ISTAF=1
      ELSE
         ISTAF=0
      ENDIF
#if defined(CERNLIB_CZ)
      CALL HOPTPF
#endif
      END
