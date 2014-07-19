*
* $Id: c337ch.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: c337ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C337CH(OK)
          LOGICAL OK, OKD
          CHARACTER*3      ERRCNT
#include "kernnumt/sysdat.inc"
          EXTERNAL         EXPINT
          REAL             EXPINT, RF
#if defined(CERNLIB_NUMLOPRE)
          EXTERNAL         DEXPIN
          DOUBLE PRECISION DEXPIN, DF
#endif
          PARAMETER(KOUNTX  =  7)
          DOUBLE PRECISION X(KOUNTX), Y(KOUNTX)
          DATA
     +    X(1) / -25.D0 /,  Y(1) / -0.30059 50906 5253D10 /,
     +    X(2) / -15.D0 /,  Y(2) / -0.23495 58524 9076D6  /,
     +    X(3) / -10.D0 /,  Y(3) / -0.24922 28976 2418D4  /,
     +    X(4) /  -5.D0 /,  Y(4) / -0.40185 27535 5802D2  /,
     +    X(5) /   0.5D0/,  Y(5) /  0.55977 35947 7615D0  /,
     +    X(6) /   2.D0 /,  Y(6) /  0.48900 51070 8060D-1 /,
     +    X(7) /   5.D0 /,  Y(7) /  0.11482 95591 2753D-2 /
          TRUNC   =  5E-13
#if defined(CERNLIB_NUMHIPRE)
          RROUND  =  RELPRT(1)*10.
          CALL RFEQDY(KOUNTX,X,EXPINT,Y,TRUNC,RROUND,OK)
#endif
#if defined(CERNLIB_NUMLOPRE)
          RROUND  =  RELPRT(1)
          DROUND  =  RELPRT(2)*10.
          CALL RFEQDF(KOUNTX,X,EXPINT,DEXPIN,TRUNC,RROUND,OK)
          CALL DFEQDY(KOUNTX,X,DEXPIN,Y,TRUNC,DROUND,OKD)
          OK  =  OK .AND. OKD
#endif
#if defined(CERNLIB_NUMHIPRE)
          ERRCNT  =  'ONE'
#endif
#if defined(CERNLIB_NUMLOPRE)
          ERRCNT  =  'TWO'
#endif
          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101) ERRCNT
          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102) ERRCNT
          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103) ERRCNT
          RF  =  EXPINT(0.)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'EXPINT', 'EXPINT', RF, 'C337.1'
          ENDIF
#if defined(CERNLIB_NUMLOPRE)
          DF  =  DEXPIN(0.D0)
          IF(DF .NE. 0.D0)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'DEXPIN', 'DEXPIN', DF, 'C337.1'
          ENDIF
#endif
          RETURN
  101 FORMAT(/2X,A3,' ERROR AND ABEND MESSAGES SHOULD NOW FOLLOW ...')
  102 FORMAT(/2X,A3,' ERROR MESSAGES SHOULD NOW FOLLOW ...')
  103 FORMAT(/2X,A3,' ABEND MESSAGES SHOULD NOW FOLLOW ...')
  104 FORMAT(/' ????? TEST OF ',A6,' ... ',A6,' =', E20.10,
     +       ' ERROR CONDITION ', A6, ' NOT DETECTED.')
      END
#endif
