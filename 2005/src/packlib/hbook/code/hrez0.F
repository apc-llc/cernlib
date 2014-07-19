*
* $Id: hrez0.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hrez0.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.53  by  Rene Brun
*-- Author :
      SUBROUTINE HREZ0(LBANK)
*.==========>
*.           RESETS TO 0. DATA STRUCTURE POINTED BY LBANK
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
*.___________________________________________
      IF(LBANK.NE.0)THEN
         CALL VZERO(IQ(LBANK+KNOENT),IQ(LBANK-1)-1)
         LW=LQ(LBANK)
         IF(LW.NE.0)THEN
            CALL VZERO(IQ(LW+1),IQ(LW-1))
            LN=LQ(LW)
            IF(LN.NE.0)THEN
               CALL VZERO(IQ(LN+1),IQ(LN-1))
            ENDIF
         ENDIF
         LFUNC=LQ(LBANK-1)
         IF(LFUNC.NE.0)CALL VZERO(IQ(LFUNC+3),IQ(LFUNC-1)-2)
      ENDIF
      END
