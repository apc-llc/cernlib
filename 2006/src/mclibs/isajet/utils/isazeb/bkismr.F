*
* $Id: bkismr.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkismr.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISMR(LISMR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     book ISMR bank
C-
C-   Outputs :
C-      LISMR = pointer to ISMR bank
C-
C-   Created  18-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "izismr.inc"
      INTEGER LISMR,LISAC,GZISAC,GZISMR
C----------------------------------------------------------------------
      LISMR=GZISMR()
      IF(LISMR.NE.0) GOTO 999  ! already exists
      LISAC=GZISAC()
      CALL MZBOOK(IXMAIN,LISMR,LISAC,-IZISMR,
     $            'ISMR',0,0,9,3,-1)
  999 RETURN
      END
