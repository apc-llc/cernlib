*
* $Id: gzpjet.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzpjet.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZPJET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to PJET bank
C-
C-   Returned value  : Link to 1st element of PJET linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZPJET
#include "zebcom.inc"
#include "izpjet.inc"
      INTEGER LPJHD,GZPJHD
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPJET=0
C
C--   GET LINK TO SUPPORTING PJHD BANK
      LPJHD=GZPJHD()
C
C--   CHECK LPJHD
      IF(LPJHD.LE.0) GOTO 999
C
C--   FIND LINK TO PJET
      GZPJET=LQ(LPJHD-IZPJET)

C
  999 RETURN
      END
