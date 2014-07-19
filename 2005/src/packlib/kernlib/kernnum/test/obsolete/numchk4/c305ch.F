*
* $Id: c305ch.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: c305ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C305CH(OK)
          LOGICAL OK, OKR, OKD
          CHARACTER*3 ERRCNT
#include "kernnumt/sysdat.inc"
#if (!defined(CERNLIB_NUMIB)||defined(CERNLIB_NUMOWNCODE))&&(!defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMOWNCODE))&&(!defined(CERNLIB_IBMRT)||defined(CERNLIB_NUMOWNCODE))
          REAL             GAMMA,  RF
          DOUBLE PRECISION DGAMMA, DF
          EXTERNAL         GAMMA,  DGAMMA
#endif
#if (defined(CERNLIB_NUMIB)||defined(CERNLIB_NUMUC)||defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_NUMOWNCODE))
          INTRINSIC        GAMMA,  DGAMMA
#endif
#if (defined(CERNLIB_NUMIB)||defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_NUMOWNCODE))
          PARAMETER(KNTSKP  =  2)
#endif
#if (!defined(CERNLIB_NUMIB)||defined(CERNLIB_NUMOWNCODE))&&(!defined(CERNLIB_IBMRT)||defined(CERNLIB_NUMOWNCODE))
          PARAMETER(KNTSKP  =  0)
#endif
          PARAMETER(KOUNTX  =  4)
          DOUBLE PRECISION X(KOUNTX), Y(KOUNTX)
          DATA
     +    X(1)/-3.5D0/, Y(1)/+0.27008 82058 52269 10892 16255 213D+00/,
     +    X(2)/-0.5D0/, Y(2)/-0.35449 07701 81103 20545 96334 967D+01/,
     +    X(3)/+1.5D0/, Y(3)/+0.88622 69254 52758 01364 90837 417D+00/,
     +    X(4)/+5.5D0/, Y(4)/+0.52342 77778 45535 20181 14900 849D+02/
#if defined(CERNLIB_NUMHIPRE)
          RROUND  =  RELPRT(1)*10.
          DTRUNC  =  1E-26
#endif
#if (defined(CERNLIB_NUMLOPRE))&&(defined(CERNLIB_NUMOWNCODE))
          RROUND  =  RELPRT(1)
          DTRUNC  =  5E-17
#endif
#if (defined(CERNLIB_NUMLOPRE))&&(!defined(CERNLIB_NUMOWNCODE))&&(!defined(CERNLIB_NUMIB))&&(!defined(CERNLIB_NUMUC))&&(!defined(CERNLIB_IBMRT))
          RROUND  =  RELPRT(1)
          DTRUNC  =  5E-17
#endif
#if (defined(CERNLIB_NUMLOPRE))&&(!defined(CERNLIB_NUMOWNCODE))&&(defined(CERNLIB_NUMIB)||defined(CERNLIB_NUMUC)||defined(CERNLIB_IBMRT))
          RROUND  =  RELPRT(1)*10.
          DTRUNC  =  5E-17
#endif
          DROUND  =  RELPRT(2)*10.
          RTRUNC  =  5E-17
          M  =  1 + KNTSKP
          N  =  KOUNTX - KNTSKP
          CALL DFEQDY(N,X(M),DGAMMA,Y(M),DTRUNC,DROUND,OKD)
          CALL RFEQDY(N,X(M), GAMMA,Y(M),RTRUNC,RROUND,OKR)
          OK  =  OKD .AND. OKR
#if (!defined(CERNLIB_NUMIB)||defined(CERNLIB_NUMOWNCODE))&&(!defined(CERNLIB_NUMUC)||defined(CERNLIB_NUMOWNCODE))&&(!defined(CERNLIB_IBMRT)||defined(CERNLIB_NUMOWNCODE))
          ERRCNT  =  'TWO'
          IF(      ERPRNT .AND.       ERSTOP) WRITE(*,101) ERRCNT
          IF(      ERPRNT .AND. .NOT. ERSTOP) WRITE(*,102) ERRCNT
          IF(.NOT. ERPRNT .AND.       ERSTOP) WRITE(*,103) ERRCNT
          RF  =  GAMMA(0.)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'GAMMA ', 'GAMMA ', RF, 'C305.1'
          ENDIF
          DF  =  DGAMMA(0.D0)
          IF(DF .NE. 0.D0)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'DGAMMA', 'DGAMMA', DF, 'C305.2'
          ENDIF
#endif
          RETURN
 101     FORMAT(/2X,A3,' ERROR AND ABEND MESSAGE SHOULD NOW FOLLOW ...')
 102      FORMAT(/2X,A3,' ERROR MESSAGE SHOULD NOW FOLLOW ...')
 103      FORMAT(/2X,A3,' ABEND MESSAGE SHOULD NOW FOLLOW ...')
 104      FORMAT(/' ????? TEST OF ',A6,' ... ',A6,' = ', E20.10,
     +       ' ERROR CONDITION ', A6, ' NOT DETECTED.')
 105      FORMAT(1P,' TEST VALUE =', D35.27,' DIFFERS FROM'/
     +              ' REF. VALUE =', D35.27)
      END
#endif
