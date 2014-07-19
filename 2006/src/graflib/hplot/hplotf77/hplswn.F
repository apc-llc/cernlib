*
* $Id: hplswn.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplswn.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.16/01 30/04/93  17.25.41  by  Rene Brun
*-- Author :
      SUBROUTINE HPLSWN
*.==========>
*.       Set system coordinates and the appropriate window according
*.       to transformation.
*.       Draws page surround, if request, and histogram surround.
*.       If option "Same" is selected when plotting nothing is done.
*.       If option "Update" is selected when plotting we must verify if
*.       scale is OK (otherwise we have to redraw axes and complete
*.       histogram), and select the right transformation.
*.       Initialise and selects histogram transformation NTHIST
*.       (see HPLIVE and HPLJ).
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl13.inc"
#include "hbook/hcbook.inc"
*._____________________________
*
      LOSCAL=.FALSE.
      IF (LOSAME) THEN
           IF (NTWIN.NE.NTHIST) CALL ISELNT (NTHIST)
           NTWIN = NTHIST
           RETURN
       ENDIF
*
      IF(IOPTU.NE.0)THEN
         CALL HPLIVE
      ELSE IF(IOPTP.NE.0)THEN
         NTZEB=IQ(LHPLIP+2)
         CALL ISELNT(NTZEB)
         NTWIN=NTZEB
         NTHIST=NTZEB
      ELSE
         IF (.NOT.LOWIND) THEN
            CALL HPLI
         ELSE
            IWIN=IWIN+1
            NCHIST=NCHIST+1
            NWIN=IXWIN*IYWIN
            IF ((NCHIST.GT.NWIN).AND.(.NOT.LWSAME)) NCHIST=1
            IF (IWIN.GT.NWIN) IWIN=1
            IF ((IWIN.EQ.IWIN0).AND.(.NOT.LWSAME)) CALL HPLI
         ENDIF
         CALL HPLJ
      ENDIF
*
      END

