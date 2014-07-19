*
* $Id: fpvgetnam.F,v 1.3 1996/05/31 16:24:42 couet Exp $
*
* $Log: fpvgetnam.F,v $
* Revision 1.3  1996/05/31 16:24:42  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:07:08  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  23.12.49  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPVGETNAM(XNAM,YNAM,EYNAM)
      CHARACTER*(*) XNAM,YNAM,EYNAM
*.===========> Author: G. Kozlovsky, 1994
*.
*. Get KUIP names of vector arguments X, Y, and EY of vector/fit
*..==========>
*
#include "paw/fpcoms.inc"

      XNAM = XVECNAM
      YNAM = YVECNAM
      EYNAM = EYVECNAM
      END
