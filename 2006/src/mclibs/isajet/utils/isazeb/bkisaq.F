*
* $Id: bkisaq.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkisaq.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISAQ(LISAQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book ISAQ banks
C-   Outputs :
C-     LISAQ = pointer to created bank
C-
C-   Created  11-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER LISAQ
#include "izisaq.inc"
#include "zebcom.inc"
      INTEGER NLINKS,NDATA
      PARAMETER( NLINKS = 2 )
      PARAMETER( NDATA = 9 )
      INTEGER GZISAQ,IOISAQ,LNEXT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('ISAQ','1I-F',IOISAQ)
        FIRST=.FALSE.
      ENDIF
C
      LISAQ=GZISAQ()
C
      IF(LISAQ.EQ.0) THEN   ! first ISAQ bank
        CALL MZBOOK(IXMAIN,LISAQ,LISAE,-IZISAQ,
     $                    'ISAQ',NLINKS,0,NDATA,IOISAQ,-1)
C
      ELSE                  ! find last ISAQ bank
    1   LNEXT=LQ(LISAQ)
        IF(LNEXT.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISAQ,LISAQ,0,
     $                    'ISAQ',NLINKS,0,NDATA,IOISAQ,-1)
        ELSE
          LISAQ=LNEXT
          GOTO 1
        ENDIF
      ENDIF
C
  999 RETURN
      END
