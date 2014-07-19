*
* $Id: c341ch.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: c341ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C341CH(OK)
          LOGICAL          OK, OKD
          CHARACTER*4      ERRCNT
#include "kernnumt/sysdat.inc"
          EXTERNAL         ALOGAM
          REAL             ALOGAM, RF
#if defined(CERNLIB_NUMLOPRE)
          EXTERNAL         DLOGAM
          DOUBLE PRECISION DLOGAM, DF
#endif
          PARAMETER(KOUNTX  =  5)
          DOUBLE PRECISION X(KOUNTX), Y(KOUNTX)
          DATA
     +    X(1) / 0.25D0/,  Y(1) / 0.12880 22524 6981D1 /,
     +    X(2) / 1.25D0/,  Y(2) /-0.98271 83642 1813D-1/,
     +    X(3) / 3.0D0 /,  Y(3) / 0.69314 71805 5994D0 /,
     +    X(4) / 5.0D0 /,  Y(4) / 0.31780 53830 3479D1 /,
     +    X(5) /50.0D0 /,  Y(5) / 0.14456 57439 4634D3 /
          TRUNC   =  5E-13
#if defined(CERNLIB_NUMHIPRE)
          RROUND  =  RELPRT(1)*10.
          CALL RFEQDY(KOUNTX,X,ALOGAM,Y,TRUNC,RROUND,OK)
#endif
#if defined(CERNLIB_NUMLOPRE)
          RROUND  =  RELPRT(1)
          DROUND  =  RELPRT(2)*10.
          CALL RFEQDF(KOUNTX,X,ALOGAM,DLOGAM,TRUNC,RROUND,OK)
          CALL DFEQDY(KOUNTX,X,DLOGAM,Y,TRUNC,DROUND,OKD)
          OK  =  OK .AND. OKD
#endif
#if defined(CERNLIB_NUMHIPRE)
          ERRCNT  =  ' TWO'
#endif
#if defined(CERNLIB_NUMLOPRE)
          ERRCNT  =  'FOUR'
#endif
          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101)ERRCNT
          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102)ERRCNT
          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103)ERRCNT
          RF  =  ALOGAM(0.)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'ALOGAM', 'ALOGAM', RF, 'C341.1'
          ENDIF
          RF  =  ALOGAM(-1.)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'ALOGAM', 'ALOGAM', RF, 'C341.1'
          ENDIF
#if defined(CERNLIB_NUMLOPRE)
          DF  =  DLOGAM(0.D0)
          IF(DF .NE. 0.D0)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'DLOGAM', 'DLOGAM', DF, 'C341.1'
          ENDIF
          DF  =  DLOGAM(-1.D0)
          IF(DF .NE. 0.D0)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'DLOGAM', 'DLOGAM', DF, 'C341.1'
          ENDIF
#endif
          RETURN
  101 FORMAT(/2X,A4,' ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
  102 FORMAT(/2X,A4,' ERROR MESSAGES SHOULD NOW FOLLOW ...')
  103 FORMAT(/2X,A4,' ABEND MESSAGES SHOULD NOW FOLLOW ...')
  104 FORMAT(/' ????? TEST OF ',A6,' ... ',A6,' =', E20.10,
     +       ' ERROR CONDITION ', A6, ' NOT DETECTED.')
      END
#endif
