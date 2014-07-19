*
* $Id: fpgetpnam.F,v 1.3 1996/05/31 16:24:29 couet Exp $
*
* $Log: fpgetpnam.F,v $
* Revision 1.3  1996/05/31 16:24:29  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:41  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:09  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/07 11/08/95  11.57.31  by  O.Couet
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPGETPNAM(PNAM,LNAM,UNAM,SNAM,ENUM)
      CHARACTER*(*) PNAM,LNAM,UNAM,SNAM,ENUM
*
*.===========> Author: G. Kozlovsky, 1994
*.
*. Get KUIP names of vector arguments of histo/fit or vector/fit
*. commands.
*..==========>
*
#include "paw/fpcoms.inc"

      PNAM = VPARNAM
      LNAM = VLOWNAM
      UNAM = VUPPNAM
      SNAM = VSTEPNAM
      ENUM = VERRORNUM
      END
