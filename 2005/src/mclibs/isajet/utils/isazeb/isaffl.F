*
* $Id: isaffl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isaffl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAFFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book and fill ISAF (ISAJET end-of-run) bank
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "idrun.inc"
#include "final.inc"
#include "primar.inc"
#include "zebcom.inc"
      INTEGER IOISAF,LISAF
      LOGICAL FIRST
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C
C
C  create Zebra bank ISAF  (end record)
      CALL MZFORM('ISAF','2I-F',IOISAF)
      CALL MZBOOK(IXMAIN,LISAE,LISAE,1,
     $            'ISAF',0,0,4,IOISAF,0)
      LISAF=LISAE
      IQ(LISAF+1)=IEVT
      IQ(LISAF+2)=NEVENT
      Q(LISAF+3)=SIGF*1000.
      IF(IEVT.GT.0) Q(LISAF+4)=SIGF/IEVT*1000.
  999 RETURN
      END
