*
* $Id: gzpjhd.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzpjhd.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZPJHD()
C------------------------------------------------------------------- ---
C-
C-   Purpose and Methods : Returns the Link to PJHD bank
C-
C-   Returned value  : Link to 1st element of PJHD linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989 17:57:58.00  Chip Stewart
C-   Updated  17-JAN-1990   Boaz Klima
C-      CHECK NUMBER OF STRUCTURAL LINKS
C-
C------------------------------------------------------------------- ---
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZPJHD
#include "zebcom.inc"
#include "izpjhd.inc"
      INTEGER NS
C------------------------------------------------------------------- ---
C
C--   INITIALIZE
      GZPJHD=0
C
C--   CHECK LISAE
      IF(LISAE.LE.0) GO TO 999
C
C ****  Get number of structural links
C
      NS = IQ(LISAE-2)
      IF ( NS .GT. IZPJHD ) THEN
C--   FIND LINK TO PJHD
        GZPJHD=LQ(LISAE-IZPJHD)
      ENDIF
C
  999 RETURN
      END
