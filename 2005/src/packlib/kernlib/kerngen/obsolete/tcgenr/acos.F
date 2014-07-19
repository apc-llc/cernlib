*
* $Id: acos.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: acos.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ACOS (A)
C                                       LAST MODIF.  16/07/80
      COMMON /SLATE/QXQ(7),ARG(2),C,XQX(30)
      DATA  ZERO  /1.0E-15/
C
C-       ENTRY ACOS
C
      ARG(2) = A
      ARG(1) = ABS(SQRT(1.-ARG(2)**2))
      ACOS   = ATAN2(ARG(1),ARG(2))
      RETURN
C
         ENTRY ASIN                                                      CDC
C        ENTRY ASIN    (A)                                              -CDC
C
      ARG(1) = A
      ARG(2) = ABS(SQRT(1.-ARG(1)**2))
#if defined(CERNLIB_ENTRET)
      ASIN   = ATAN2(ARG(1),ARG(2))
#endif
#if !defined(CERNLIB_ENTRET)
      ACOS   = ATAN2(ARG(1),ARG(2))
#endif
      RETURN
C
         ENTRY TAN                                                       CDC
C        ENTRY TAN     (A)                                              -CDC
C
      C = COS(A)
      IF (ABS(C).GT.ZERO)  GO TO 63
#if defined(CERNLIB_ENTRET)
      TAN  = 10.E20
#endif
#if !defined(CERNLIB_ENTRET)
      ACOS = 10.E20
#endif
      RETURN
C
#if defined(CERNLIB_ENTRET)
   63 TAN  = SIN(A)/C
#endif
#if !defined(CERNLIB_ENTRET)
   63 ACOS = SIN(A)/C
#endif
      RETURN
      END
