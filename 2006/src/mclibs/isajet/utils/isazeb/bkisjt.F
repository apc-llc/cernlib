*
* $Id: bkisjt.F,v 1.11 2001/10/08 14:03:15 mclareni Exp $
*
* $Log: bkisjt.F,v $
* Revision 1.11  2001/10/08 14:03:15  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BKISJT(LISJT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book ISJT banks (jets found with toy calorimeter)
C-   Output:
C-      LISJT = pointer to booked bank
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER LISJT,LISAC,GZISAC,LNEXT,GZISJT
#include "izisjt.inc"
#include "zebcom.inc"
      INTEGER NLINKS,NDATA
      PARAMETER( NLINKS = 1 )
      PARAMETER( NDATA  = 9 )
C----------------------------------------------------------------------
C
      LISJT=GZISJT()
C
      IF(LISJT.EQ.0) THEN   ! first ISJT bank
        LISAC=GZISAC()
        CALL MZBOOK(IXMAIN,LISJT,LISAC,-IZISJT,
     $                    'ISJT',NLINKS,0,NDATA,3,-1)
C
      ELSE                  ! find last ISJT bank
    1   LNEXT=LQ(LISJT)
        IF(LNEXT.EQ.0) THEN
          CALL MZBOOK(IXMAIN,LISJT,LISJT,0,
     $                    'ISJT',NLINKS,0,NDATA,3,-1)
        ELSE
          LISJT=LNEXT
          GOTO 1
        ENDIF
      ENDIF
  999 RETURN
      END
