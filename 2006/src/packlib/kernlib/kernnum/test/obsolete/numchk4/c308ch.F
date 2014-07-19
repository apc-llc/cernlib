*
* $Id: c308ch.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: c308ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C308CH(OK)
          LOGICAL          OK, OKK, OKE
          CHARACTER*4      ERRCNT
#include "kernnumt/sysdat.inc"
          EXTERNAL         ELLICK, ELLICE
          REAL             ELLICK, ELLICE, RF
#if defined(CERNLIB_NUMLOPRE)
          EXTERNAL         DELLIK, DELLIE
          DOUBLE PRECISION DELLIK, DELLIE, DF
#endif
          PARAMETER(KOUNTX  =  1)
          DOUBLE PRECISION X(KOUNTX), EK(KOUNTX), EE(KOUNTX)
          DATA X  / 0.75D0 /
          DATA EK / 0.19109 89780 7518D1 /
          DATA EE / 0.13184 72107 9946D1 /
          TRUNC   =  5E-13
#if defined(CERNLIB_NUMHIPRE)
          RROUND  =  RELPRT(1)*10.
          CALL RFEQDY(KOUNTX,X,ELLICK,EK,TRUNC,RROUND,OKK)
          CALL RFEQDY(KOUNTX,X,ELLICE,EE,TRUNC,RROUND,OKE)
          OK  =  OKK .AND. OKE
#endif
#if defined(CERNLIB_NUMLOPRE)
          RROUND  =  RELPRT(1)
          DROUND  =  RELPRT(2)*10.
          CALL RFEQDF(KOUNTX,X,ELLICK,DELLIK,TRUNC,RROUND,OKK)
          CALL RFEQDF(KOUNTX,X,ELLICE,DELLIE,TRUNC,RROUND,OKE)
          OK  =  OKK .AND. OKE
          CALL DFEQDY(KOUNTX,X,DELLIK,EK,TRUNC,DROUND,OKK)
          CALL DFEQDY(KOUNTX,X,DELLIE,EE,TRUNC,DROUND,OKE)
          OK  =  OK .AND. OKK .AND. OKE
#endif
#if defined(CERNLIB_NUMHIPRE)
          ERRCNT  =  ' TWO'
#endif
#if defined(CERNLIB_NUMLOPRE)
          ERRCNT  =  'FOUR'
#endif
          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101) ERRCNT
          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102) ERRCNT
          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103) ERRCNT
          RF  =  ELLICK(1.)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'ELLICK','ELLICK',RF, 'C308.1'
          ENDIF
          RF  =  ELLICE(-1.001)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'ELLICE','ELLICE',RF, 'C308.1'
          ENDIF
#if defined(CERNLIB_NUMLOPRE)
          DF  =  DELLIK(1.D0)
          IF(DF .NE. 0.D0)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'DELLIK','DELLIK',DF, 'C308.1'
      ENDIF
      DF  =  DELLIE(-1.001D0)
      IF(DF .NE. 0.D0)  THEN
         OK  =  .FALSE.
         WRITE(*,104) 'DELLIE','DELLIE',DF, 'C308.1'
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
