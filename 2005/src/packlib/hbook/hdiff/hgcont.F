*
* $Id: hgcont.F,v 1.1.1.1 1996/01/16 17:07:56 mclareni Exp $
*
* $Log: hgcont.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  10.16.43  by  Julian Bunn
*-- Author :    R. J. Genik II   23/10/92
      FUNCTION HGCONT(IDDD, IX, IY, FUNC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the content of the histogram IDDD at
C-                         bin (IX,IY).
C-
C-   Returned value  :     Real value with content of (IX,IY)
C-   Inputs  : IDDD        HBook ID pointer for histogram.
C-             IX           X bin of point
C-             IY           Y bin of point
C-             Func        Specified function to perform
C-                              1 = Get 1-D content
C-                              2 = Get 1-D error bar
C-                              3 = Get # entries from profile hist
C-   Outputs : None
C-   Controls: TWODIM from /HDBCOM/
C-
C-   Created  24-JUL-1989   Jason McCampbell (MSU)
C-   Updated  17-SEP-1992   James T. Linnemann  IX, IY; add profile hist; allow
C-   for errors on histos w/o error bars
C-
C----------------------------------------------------------------------
C
C
#include "hbook/hcbook.inc"
#include "hbook/hcdifb.inc"
C---------------------------------------------------------------------
C     Local and passed declarations
C---------------------------------------------------------------------
C
      INTEGER IDDD, IX, IY, FUNC
      INTEGER LW, LN
      REAL    HGCONT, HCX, HCXY
C
C=====================================================================
C    Get contents...
C=====================================================================
C
      CALL HFIND(IDDD,'HGCONT')
      IF ( TWODIM ) THEN
        HGCONT = HCXY(IX, IY, 1)
        IF (FUNC.EQ.2) HGCONT = HCXY(IX, IY, 2)
      ELSE
        IF (FUNC.EQ.1) THEN
          HGCONT = HCX(IX,1)
        ELSEIF (FUNC.EQ.2) THEN
C
C---------------------------------------------------------------------
C...check if the histogram had an error bar; if not use sqrt errors
C---------------------------------------------------------------------
C
          IF ( LQ(LQ(LCID-1)).NE.0) THEN
            HGCONT = HCX(IX, FUNC)
          ELSE
            HGCONT = HCX(IX,1)
            HGCONT = SQRT(ABS(HGCONT))
          ENDIF
        ELSE
C
C---------------------------------------------------------------------
C     want profile # of counts; Find the pointers
C---------------------------------------------------------------------
C
          LCONT=LQ(LCID-1)
          LW=LQ(LCONT)
          LN=LQ(LW)
          HGCONT = Q(LN+IX)
        ENDIF
      ENDIF
      END
