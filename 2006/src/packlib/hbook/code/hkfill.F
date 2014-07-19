*
* $Id: hkfill.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hkfill.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :    Rene Brun   14/02/94
      SUBROUTINE HKFILL(LINE,ID1,X,Y,W)
*.==========>
*.           Special fast filling routine for COMIS
*.           All calls to HFILL point initially to this routine
*..=========> ( R.Brun )
#include "hbook/pilot.h"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcfast.inc"
      EXTERNAL HKFI1,HKFFI1,HKFF2,HKFILPF,HKFIL2,HKNULI
*.___________________________________________
      LINABS=ABS(LINE)
      CALL HFILL(ID1,X,Y,W)
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID1)
      IF(IDPOS.LE.0)THEN
         CALL CSHFAST(LINABS,HKNULI)
         RETURN
      ENDIF
*
      IF(I1.NE.0)THEN
*
*             1-DIM HISTOGRAM OR PROFILE
*
         IF(I8.EQ.0)THEN
            IF(I5.EQ.0.AND.I6.EQ.0.AND.I7.EQ.0)THEN
               IF(LINE.GT.0.AND.LINE.LT.1000)THEN
                  NID(LINE)=0
                  CALL CSHFAST(LINABS,HKFFI1)
               ELSE
                  CALL CSHFAST(LINABS,HKFI1)
               ENDIF
            ELSE
               CALL CSHFAST(LINABS,HKFI1)
            ENDIF
         ELSE
            CALL CSHFAST(LINABS,HKFILPF)
         ENDIF
      ELSE
*
*             2-DIM HISTOGRAM
*
         IF(LQ(LCID-2).EQ.0.AND.LQ(LCID-3).EQ.0.AND.LQ(LCID-4).EQ.0.AND.
     +     LQ(LCID-5).EQ.0.AND.LQ(LCID-6).EQ.0.AND.LQ(LCID-2).EQ.0)THEN
            IF(LINE.GT.0.AND.LINE.LT.1000)THEN
               NID(LINE)=0
               CALL CSHFAST(LINABS,HKFF2)
            ELSE
               CALL CSHFAST(LINABS,HKF2)
            ENDIF
         ELSE
            CALL CSHFAST(LINABS,HKFIL2)
         ENDIF
      ENDIF
      END
