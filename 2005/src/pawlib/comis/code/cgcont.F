*
* $Id: cgcont.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: cgcont.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.26.35  by  Rene Brun
*-- Author : V.Berezhnoi
         INTEGER FUNCTION CGCONT(J,N)
***-----------------------------------
*         reads continues line
***-----------------------------------
         COMMON/CSCONT/LCONT
         COMMON/CSGSCM/IGS,JGS,NGS,CSJUNK(3)
***
*   ISTATE
*      1     CS>
*      2     FSD>
*      3     MND>
*      4     PAU>
*      5     ???>
***
      CGCONT=0
      IF(NGS.GT.0) RETURN
      IF(LCONT.NE.0)RETURN
        CALL CSRD(J,N)
        IF(N.GT.0)CGCONT=1
        NGS=N
      END
