*
* $Id: hmaxim.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmaxim.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          16/05/95  10.12.49  by  O.Couet
*-- Author :
      SUBROUTINE HMAXIM(ID2,AMAX)
*.==========>
*.           Set maximum scale of the histogram ID2. When a
*.           maximum is fixed in the histogram data structure
*.           the flag I20 is set.
*..=========> ( R.Brun )
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
*.___________________________________________
*
      IRET=3
*
   10 CALL HLOOP(ID2,'HMAXIM',IRET)
      IF(IRET.EQ.0)GOTO 999
*
      CALL HDCOFL
*
*              1D histograms
*
      IF(I1.NE.0)THEN
         Q(LCID+KMAX1)=FLOARG(AMAX)
         IF(I21.NE.0.AND.Q(LCID+KMAX1).LE.Q(LCID+KMIN1))THEN
            CALL HSIFLA(20,0)
            CALL HSIFLA(21,0)
         ELSE
            CALL HSIFLA(20,1)
         ENDIF
      ENDIF
*
*              2D histograms
*
      IF(I230.NE.0)THEN
         Q(LCID+KMAX2)=FLOARG(AMAX)
         IF(I21.NE.0.AND.Q(LCID+KMAX2).LE.Q(LCID+KMIN2))THEN
            CALL HSIFLA(20,0)
            CALL HSIFLA(21,0)
         ELSE
            CALL HSIFLA(20,1)
         ENDIF
      ENDIF
*
      IRET=2
      GOTO 10
*
  999 END
