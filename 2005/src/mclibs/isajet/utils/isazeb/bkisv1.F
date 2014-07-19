*
* $Id: bkisv1.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkisv1.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISV1(NVERTX,LISV1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book ISV1 bank (ISAJET vertices)
C-   Input:
C-     NVERTX = vertex number
C-   Outputs :
C-     LISV1 = pointer to LISV1
C-
C-   Created  11-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "izisv1.inc"
#include "zebcom.inc"
#include "isalnk.inc"
C
      INTEGER NVERTX,LISV1
      INTEGER IOISV1
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        CALL MZFORM('ISV1','1I-F',IOISV1)   ! format for ISV1
        FIRST=.FALSE.
      ENDIF
      IF(NVERTX.EQ.1) THEN
        CALL MZBOOK(IXMAIN,LVD,LISAE,-IZISV1,
     $              'ISV1',2,1,9,IOISV1,-1)
        LISV1=LVD(1)
      ELSE
        CALL MZBOOK(IXMAIN,LVD(NVERTX),LVD(NVERTX-1),0,
     $                  'ISV1',2,1,9,IOISV1,-1)
        LISV1=LVD(NVERTX)
      ENDIF
  999 RETURN
      END
