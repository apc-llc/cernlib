*
* $Id: golclo.F,v 1.1.1.1 1996/02/14 13:10:47 mclareni Exp $
*
* $Log: golclo.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:47  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE GOLCLO
#include "higz/golden.inc"
#include "higz/himeta.inc"
      DIMENSION PAR(2)
      INTEGER UNLINKF
      DATA PAR /0.0, 0.0/
*
      IF (FILOPN) THEN
         CALL GOLWRI(MA,PAR,' ')
         CALL GOLWRI(SP,0,' ')
*
#if (!defined(CERNLIB_XIOPAK))&&(!defined(CERNLIB_CIPACK))
         IF (BIMODE) THEN
            LAST = LBUF - IPOINT + 1
            IF (LAST .LT. 13) LAST = LAST + LBUF
            L1 = LAST
            L2 = 0
            IF (LAST .GT. 256) THEN
               L1 = LAST/2
               L2 = LAST - L1
            END IF
            CALL MFOTIB(SS)
            CALL MFOTIB(L1-2)
            CALL MPUTBF('DEFAULT.SYM',11)
            IF (IPOINT .LE. LBUF) CHMBUF(IPOINT:LBUF) = ' '
            IPOINT = IPOINT + L1 - 11 - 2
            IF (L2 .GT. 0) THEN
               CALL MFOTIB(SS)
               CALL MFOTIB(L2-2)
               CALL MPUTBF('DEFAULT.SYM',11)
            END IF
            CALL MOUTBF
         END IF
#endif
#if defined(CERNLIB_XIOPAK)||defined(CERNLIB_CIPACK)
         IF (BIMODE) THEN
            CALL MOUTBF
#endif
#if defined(CERNLIB_XIOPACK)
            CALL XCLOSE(LUNG)
#endif
#if defined(CERNLIB_CIPACK)
            CALL CICLOS(LUNG)
            IF (FRADEL) then
               ISTAT = UNLINKF(chmeta)
            END IF
         ELSE
#endif
            IF (FRADEL) THEN
               CLOSE(LUNG(1),STATUS='DELETE')
            ELSE
               CLOSE(LUNG(1),STATUS='KEEP')
            END IF
#if defined(CERNLIB_XIOPAK)||defined(CERNLIB_CIPACK)
         END IF
#endif
         FILOPN = .FALSE.
      END IF
      END
#endif
