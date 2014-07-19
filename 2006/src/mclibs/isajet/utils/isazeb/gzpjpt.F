*
* $Id: gzpjpt.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzpjpt.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      INTEGER FUNCTION GZPJPT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to PJPT bank
C-
C-   Returned value  : Link to 1st element of PJPT linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   18-DEC-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "izpjpt.inc"
      INTEGER LPJET,GZPJET
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPJPT=0
C
C--   GET LINK TO SUPPORTING PJHD BANK
      LPJET=GZPJET()
C
C--   CHECK LPJHD
      IF(LPJET.LE.0) GO TO 999
C
C--   FIND LINK TO PJPT
      GZPJPT=LQ(LPJET-IZPJPT)
C
  999 RETURN
      END
