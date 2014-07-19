*
* $Id: hplu.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplu.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 19/01/89  09.42.31  by  O.Couet
*-- Author :
      SUBROUTINE HPLU(ID,JTYPE)
*.==========>
*.           a call is done to HPLUSR
*.           after each plot.
*.           trap recursive calls to
*            HPLOT via HPLUSR
*..=========>
#include "hbook/hcbits.inc"
#include "hplot/hpl1.inc"
      LOGICAL LTWICE
      SAVE LTWICE
      CHARACTER*4 KICASE(9),KCASE
      DATA KICASE/'1DIM','2DIM','PROX','PROY',
     +            'SLIX','SLIY','BANX','BANY','FUNC'/
      DATA LTWICE/.FALSE./
*._____________________________
*----
*     the variable 'LTWICE' is used to trap recursive calls to
*     HPLOT via HPLUSR
*----
      IF(.NOT.LTWICE)THEN
         KID=1
         IF(LOZERO) KID=0
         KCASE=KICASE(JTYPE)
         IF(I3.NE.0) KCASE='TABL'
         LTWICE=.TRUE.
         CALL HPLUSR(ID,KCASE,KID)
         LTWICE=.FALSE.
      ELSE
         CALL HBUG('HPLOT called from HPLUSR : Program terminated',
     +              'HPLUSR',0)
         CALL HPLEND
         STOP
      ENDIF
      END

