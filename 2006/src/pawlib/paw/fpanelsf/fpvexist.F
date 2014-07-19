*
* $Id: fpvexist.F,v 1.3 1996/05/31 16:24:40 couet Exp $
*
* $Log: fpvexist.F,v $
* Revision 1.3  1996/05/31 16:24:40  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:06  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.13.50  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVEXIST(VNAM,IERFLG)
      CHARACTER*(*) VNAM
      INTEGER IERFLG
*.===========> Author: G. Kozlovsky, 1994
*.
*.  Check if a KUIP REAL vector VNAM exists.
*.  If vector exists ERFLG is set to 0.
*.  If vector is not of real type IERFLG is set to 1.
*.  If vector does not exist, IERFLG is set to 2.
*.
*..==========>
*
#include "hbook/hcbook.inc"
#include "paw/quest.inc"
*
      IERFLG = 0

      CALL CLTOU(VNAM)

      CALL KUVECT(VNAM,ILO,IHI)

      IF (ILO .EQ. 0) THEN
          IERFLG = 2
      ELSE IF (IQUEST(14) .NE. 1) THEN
          IERFLG = 1
      ENDIF

      END
