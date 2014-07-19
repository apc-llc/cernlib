*
* $Id: hkf1q.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hkf1q.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :    Rene Brun   17/02/94
      SUBROUTINE HKF1Q(LINE,ID1,X,W)
*.==========>
*.           Special fast filling routine for COMIS
*.           All calls to HF1 point initially to this routine
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcfast.inc"
      EXTERNAL HKF1,HKFF1,HKNUL1
*.___________________________________________
      LINABS=ABS(LINE)
      CALL HFILL(ID1,X,0.,W)
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID1)
      IF(IDPOS.LE.0)THEN
         CALL CSHFAST(LINABS,HKNUL1)
         RETURN
      ENDIF
      IF(I8.EQ.0)THEN
         IF(I5.EQ.0.AND.I6.EQ.0.AND.I7.EQ.0)THEN
            IF(LINE.GT.0.AND.LINE.LT.1000)THEN
               NID(LINE)=0
               CALL CSHFAST(LINABS,HKFF1)
            ELSE
               CALL CSHFAST(LINABS,HKF1)
            ENDIF
         ELSE
            CALL CSHFAST(LINABS,HKF1)
         ENDIF
      ENDIF
      END
