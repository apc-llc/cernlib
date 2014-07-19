*
* $Id: hplzom.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplzom.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.14/09 27/11/92  13.49.55  by  O.Couet
*-- Author :
      SUBROUTINE HPLZOM(IDDD,CHOPT,IMIN,IMAX)
*.==========>
*.        HPLOT basic routine for histogram zooms.
*.
*.       Input parameter:
*.
*.       IDDD                : Histogram IDentifier.
*.       CHARACTER*(*) CHOPT : cf. HPLOT (idem)
*.       IMIN                : first channel number
*.       IMAX                : last channel number
*.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hbook/hcbook.inc"
      CHARACTER*(*) CHOPT
*._____________________________
*
      IDD    = IDDD
      LOZOOM = .TRUE.
      CALL HFIND(IDD,'HPLZOM')
      IF(LCID.LE.0)GO TO 99
      NCMIN  = MAX(1,IMIN)
      NCX    = IQ(LCID+KNCX)
      NCMAX  = MIN(IMAX,NCX)
      IF(NCMIN.GT.NCMAX)THEN
         CALL HBUG('Bad values for IMIN, IMAX','HPLZOM',IDD)
         GOTO 99
      ENDIF
*
      CALL HPLOT(IDD,CHOPT,' ',0)
*
   99 LOZOOM=.FALSE.
*
      END
