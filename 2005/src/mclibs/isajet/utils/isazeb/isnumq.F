*
* $Id: isnumq.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isnumq.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISNUMQ(NJT,NQS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Find number of primary (ISAJ) and secondary (ISAQ) parton banks
C-   Outputs :
C-      NJT= number of ISAJ banks
C-      NQS= number of ISAQ banks
C-
C-   Created  13-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
      INTEGER NJT,NQS
      INTEGER LISAJ,LISAQ,GZISAJ,GZISAQ
C----------------------------------------------------------------------
      NJT=0
      NQS=0
C
C  find number of primary parton banks
      LISAJ=GZISAJ()
  11  IF(LISAJ.NE.0) THEN
        NJT=NJT+1
        LISAJ=LQ(LISAJ)
        GOTO 11
      ENDIF
C  find number of stable parton banks
      LISAQ=GZISAQ()
  12  IF(LISAQ.NE.0) THEN
        NQS=NQS+1
        LISAQ=LQ(LISAQ)
        GOTO 12
      ENDIF
  999 RETURN
      END
