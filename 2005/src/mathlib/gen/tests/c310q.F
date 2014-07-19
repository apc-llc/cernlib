*
* $Id: c310q.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c310q.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_QUAD)
      SUBROUTINE C310Q
#include "gen/imp128.inc"


#include "gen/def128.inc"
     + Z0, Z1, HF, X
      PARAMETER (Z0 = 0, Z1 = 1, HF = Z1/2)
      LOGICAL LTEST2
C
      CHARACTER*6 TFUNC
C
#include "iorc.inc"
      DATA LTEST2/.TRUE./
      DIMENSION  Y(7),T(7)
C     Set maximum error allowed for test to be considered successful
#if defined(CERNLIB_DOUBLE)
      DATA TOL/1Q-31/
      DATA TFUNC/'QLGAMA'/
      DATA (Y(I), I=1,7)
     + / 0.10Q-02,0.35Q0, 1.00Q0, 2.50Q0, 8.00Q0, 100.00Q0,
     +       0.99990000000000Q0/


      DATA (T(I), I=1,7)
     +/      6.90717888538385368251234466807698Q+00,
     +       9.34581227146232556570346655611102Q-01,
     +      -1.29037306273944802154750522153190Q-32,
     +       2.84682870472919159632494669682699Q-01,
     +       8.52516136106541430016553103634711Q+00,
     +       3.59134205369575398776044010460285Q+02,
     +       5.77297915612002217342328598585586Q-05 /

#endif
#if !defined(CERNLIB_DOUBLE)
      DATA TOL/1D-31/
      DATA TFUNC/'DLGAMA'/
      DATA (Y(I), I=1,7)
     + / 0.10D-02,0.35D0, 1.00D0, 2.50D0, 8.00D0, 100.00D0,
     +       0.99990000000000D0/

      DATA (T(I), I=1,7)
     +/      6.90717888538385368251234466807698D+00,
     +       9.34581227146232556570346655611102D-01,
     +      -1.29037306273944802154750522153190D-32,
     +       2.84682870472919159632494669682699D-01,
     +       8.52516136106541430016553103634711D+00,
     +       3.59134205369575398776044010460285D+02,
     +       5.77297915612002217342328598585586D-05 /
#endif

c      CALL HEADER('C310',0)

#if defined(CERNLIB_DOUBLE)
      ERRMAX=0Q0
      WRITE(LOUT,'(/10X,''TEST FOR QLGAMA'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      ERRMAX=0D0
      WRITE(LOUT,'(/10X,''TEST FOR DLGAMA'')')
#endif
        WRITE(LOUT,'(/8X,''X'',14X,'' '',A,''(X)'',7X,''Rel Error'')')
     +      TFUNC
      DO 1 I = 1,7
      X=Y(I)
#if !defined(CERNLIB_DOUBLE)
      DR=DLGAMA(X)
      IF(ABS(DR) .GT. 1D-31) ER=ABS((DR-T(I))/DR)
#endif
#if defined(CERNLIB_DOUBLE)
      DR=QLGAMA(X)
      IF(ABS(DR) .GT. 1Q-31)  ER=ABS((DR-T(I))/DR)
#endif
      ERRMAX =MAX(ERRMAX,ER)
         WRITE(LOUT,'(1X,F10.3,1P,E40.32,E10.1)') X,DR,ER
    1 CONTINUE

      WRITE(LOUT,'(/''LARGEST RELATIVE ERROR WAS'',1P,E10.1)')
     +ERRMAX
      LTEST2=LTEST2.AND.(ERRMAX.LE.TOL)
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')
#if !defined(CERNLIB_DOUBLE)
      DR=DLGAMA(Z0)
      DR=DLGAMA(-Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      DR=QLGAMA(Z0)
      DR=QLGAMA(-Z1)
#endif

      RETURN
      END
#endif
