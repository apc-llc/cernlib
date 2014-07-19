*
* $Id: epsetc.F,v 1.1.1.1 1996/03/08 15:21:44 mclareni Exp $
*
* $Log: epsetc.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:44  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPSETC(NWCONT,IWCONT)
C.
C. INPUT:
C.
C. NWCONT   NO. OF OVERALL CONTROL WORDS TO SET (LIMITED TO NCONT)
C.
C. IWCONT   NEW OVERALL CONTROL WORDS.
C.
C. REMARK:  HANDLE WITH CARE.
C.
#include "epio/epiocom.inc"
      DIMENSION IWCONT(2)
      N=MAX0(0,MIN0(NCONT,NWCONT))
      CALL UCOPY(IWCONT,LIST,N)
      RETURN
      END
