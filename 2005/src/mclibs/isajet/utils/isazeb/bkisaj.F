*
* $Id: bkisaj.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkisaj.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISAJ(LISAJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book ISAJ banks
C-   Outputs :
C-     LISAJ = pointer to created bank
C-
C-   Created  11-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER LISAJ
#include "izisaj.inc"
#include "zebcom.inc"
      INTEGER NLINKS,NDATA
      PARAMETER( NLINKS = 1 )
      PARAMETER( NDATA = 9 )
      INTEGER GZISAJ,IOISAJ,LNEXT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('ISAJ','1I-F',IOISAJ)
        FIRST=.FALSE.
      ENDIF
C
      LISAJ=GZISAJ()
C
      IF(LISAJ.EQ.0) THEN   ! first ISAJ bank
        CALL MZBOOK(IXMAIN,LISAJ,LISAE,-IZISAJ,
     $                    'ISAJ',NLINKS,0,NDATA,IOISAJ,-1)
C
      ELSE                  ! find last ISAJ bank
    1   LNEXT=LQ(LISAJ)
        IF(LNEXT.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISAJ,LISAJ,0,
     $                    'ISAJ',NLINKS,0,NDATA,IOISAJ,-1)
        ELSE
          LISAJ=LNEXT
          GOTO 1
        ENDIF
      ENDIF
C
  999 RETURN
      END
