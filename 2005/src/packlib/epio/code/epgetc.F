*
* $Id: epgetc.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: epgetc.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPGETC(NWCONT,IWCONT)
C.
C. OUTPUT:
C.
C. NWCONT    NO. OF OVERALL CONTROL WORDS (WORD 3 OF COMMON /C/).
C.
C. IWCONT    CONTROL WORDS (WORDS 1...NWCONT OF LIST OF COMMON /C/).
C.
#include "epio/epiocom.inc"
      DIMENSION IWCONT(2)
      NWCONT=NCONT
      CALL UCOPY(LIST,IWCONT,NCONT)
      RETURN
      END
