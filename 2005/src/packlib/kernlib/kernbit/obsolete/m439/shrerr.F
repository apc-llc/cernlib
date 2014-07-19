*
* $Id: shrerr.F,v 1.1.1.1 1996/02/15 17:47:44 mclareni Exp $
*
* $Log: shrerr.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:44  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if (defined(CERNLIB_IBM)||defined(CERNLIB_CDC))&&(!defined(CERNLIB_FORTRAN))
      SUBROUTINE SHRERR(NBEG,LBIT)
      DIMENSION IC(2)
      DATA IC/1HG,1HS/
      DATA NOM/20/,INOM/0/
#if defined(CERNLIB_IBM)
      DATA NBPW/32/
#endif
#if defined(CERNLIB_CDC)
      DATA NBPW/60/
#endif
      I=1
      GO TO 201
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_F4))
      ENTRY SHSERR
#endif
#if defined(CERNLIB_IBM)||!defined(CERNLIB_F4)
      ENTRY SHSERR(NBEG,LBIT)
#endif
      I=2
  201 WRITE(6,200)IC(I)
  200 FORMAT(1X,6('ERROR IN ',A1,'ETBYT'))
      IF (NBEG.LE.0) WRITE(6,280)
  280 FORMAT(' INCORRECT VALUE OF BEGIN BIT NUMBER')
      IF (LBIT.LE.0 .OR. LBIT.GT.NBPW) WRITE(6,300)
  300 FORMAT(' INCORRECT BYTE LENGTH')
      WRITE(6,240)NBEG,LBIT
  240 FORMAT(' NBEG=',I20,'  LBIT=',I20)
      INOM=INOM+1
      IF (INOM.GT.NOM) STOP
      RETURN
      END
#endif
