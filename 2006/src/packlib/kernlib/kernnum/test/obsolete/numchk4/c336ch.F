*
* $Id: c336ch.F,v 1.1.1.1 1996/02/15 17:48:40 mclareni Exp $
*
* $Log: c336ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:40  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C336CH(OK)
          LOGICAL          OK, OKS, OKC
          CHARACTER*3      ERRCNT
#include "kernnumt/sysdat.inc"
          EXTERNAL         SININT, COSINT
          REAL             SININT, COSINT, RF
#if defined(CERNLIB_NUMLOPRE)
          EXTERNAL         DSININ, DCOSIN
          DOUBLE PRECISION DSININ, DCOSIN, DF
#endif
          PARAMETER(KOUNTX  =  2)
          DOUBLE PRECISION X(KOUNTX), S(KOUNTX), C(KOUNTX)
          DATA X / 2.D0,                10.D0                 /
          DATA S / 0.16054 12976 8027D1, 0.16583 47594 2189D1 /
          DATA C / 0.42298 08287 7486D0,-0.04545 64330 0445D0 /
          TRUNC   =  5E-13
#if defined(CERNLIB_NUMHIPRE)
          RROUND  =  RELPRT(1)*10.
          CALL RFEQDY(KOUNTX,X,SININT,S,TRUNC,RROUND,OKS)
          CALL RFEQDY(KOUNTX,X,COSINT,C,TRUNC,RROUND,OKC)
          OK  =  OKS .AND. OKC
#endif
#if defined(CERNLIB_NUMLOPRE)
          RROUND  =  RELPRT(1)
          DROUND  =  RELPRT(2)*10.
          CALL RFEQDF(KOUNTX,X,SININT,DSININ,TRUNC,RROUND,OKS)
          CALL RFEQDF(KOUNTX,X,COSINT,DCOSIN,TRUNC,RROUND,OKC)
          OK  =  OKS .AND. OKC
          CALL DFEQDY(KOUNTX,X,DSININ,S,TRUNC,DROUND,OKS)
          CALL DFEQDY(KOUNTX,X,DCOSIN,C,TRUNC,DROUND,OKC)
          OK  =  OK .AND. OKS .AND. OKC
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
          RF  =  COSINT(0.)
          IF(RF .NE. 0.)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'COSINT', 'COSINT', RF, 'C336.1'
          ENDIF
#if defined(CERNLIB_NUMLOPRE)
          DF  =  DCOSIN(0.D0)
          IF(DF .NE. 0.D0)  THEN
             OK  =  .FALSE.
             WRITE(*,104) 'DCOSIN', 'DCOSIN', DF, 'C336.1'
          ENDIF
#endif
      RETURN
  101 FORMAT(/2X,A3,' ERROR AND ABEND MESSAGE SHOULD NOW FOLLOW ...')
  102 FORMAT(/2X,A3,' ERROR MESSAGE SHOULD NOW FOLLOW ...')
  103 FORMAT(/2X,A3,' ABEND MESSAGE SHOULD NOW FOLLOW ...')
  104 FORMAT(/' ????? TEST OF ',A6,' ... ',A6,' =', E20.10,
     +       ' ERROR CONDITION ', A6, ' NOT DETECTED.')
      END
#endif
