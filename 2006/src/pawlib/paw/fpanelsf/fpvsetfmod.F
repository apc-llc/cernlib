*
* $Id: fpvsetfmod.F,v 1.3 1996/05/31 16:24:46 couet Exp $
*
* $Log: fpvsetfmod.F,v $
* Revision 1.3  1996/05/31 16:24:46  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:13  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.18.05  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVSETFMOD(FMNAM,FCN,IERFLG)
      CHARACTER*(*) FMNAM
      INTEGER FCN
      INTEGER IERFLG
*.===========> Author: G. Kozlovsky, 1994
*.
*. Set fit model, return its pointer
*. if error, return IERFLG=1, 0 otherwise
*.
*..==========>
*
#include "hbook/hcfit2.inc"
*
      EXTERNAL HFCNV
*
      IERFLG = 0
*
      CALL CLTOU(FMNAM)
*
      IF (FMNAM(1:5) .EQ. 'CHIS2') THEN
          LINEAR = 0
      ELSE
          IERFLG = 1
      ENDIF

      FCN = JUMPAD(HFCNV)

      END
