*
* $Id: c310m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c310m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C310M
C     This program tests the operation of MATHLIB subprograms
C     ALGAMA, DLGAMA and QLGAMA(C310)

      LOGICAL LTEST, LTEST1,LTEST2
      COMMON /C310LT1/LTEST1
#if defined(CERNLIB_QUAD)
      COMMON /C310LT2/LTEST2
#endif
#include "iorc.inc"
      CALL HEADER('C310',0)
      LTEST=.TRUE.
      LTEST1=.TRUE.
      LTEST2=.TRUE.
      CALL C310D
        LTEST=LTEST .AND. LTEST1
#if defined(CERNLIB_QUAD)
      CALL C310Q
        LTEST=LTEST .AND. LTEST2
#endif
      IRC=ITEST('C310',LTEST)
      CALL PAGEND('C310')
      RETURN
      END

      SUBROUTINE C310D

#include "imp64r.inc"
      REAL  ALGAMA
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2),TOLIBM(2)

#include "gen/def64.inc"
     + Z0, Z1, HF, X
      PARAMETER (Z0 = 0, Z1 = 1, HF = Z1/2)
      LOGICAL LTEST1
C
      CHARACTER*6 TFUNC(2)
C
#include "iorc.inc"
#if defined(CERNLIB_DOUBLE)
      DIMENSION  Y(7),T(7)
      REAL            RT(7)
#endif
#if !defined(CERNLIB_DOUBLE)
      REAL       Y(7),T(7)
#endif
      DATA LTEST1/.TRUE./

      DATA TOL/1D-8, 5D-11/
      DATA TOLIBM/1D-6, 1D-11/
      DATA TFUNC/'ALGAMA','DLGAMA'/
      DATA (Y(I), I=1,7)
     + / 0.10D-02,0.35D0, 1.00D0, 2.50D0, 8.00D0, 100.00D0,
     +       0.99990000000000D0/

      DATA (T(I), I=1,7)
     + /6.90717888538385361D0,0.934581227146233540D0,
     +  0.000000000000000000D0,0.284682870472918514D0,
     +  8.52516136106527989D0,359.134205369575341D0,
     +  0.577297915612021598D-4 /

      ERRMAX=0D0
      RERRMAX=0E0
#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR ALGAMA'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR ALGAMA'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DLGAMA'')')
#endif
        WRITE(LOUT,'(/8X,''X'',14X,'' '',A,''(X)'',7X,''Rel Error'')')
     +      TFUNC(JF)
      DO 1 I = 1,7
      X=Y(I)
#if !defined(CERNLIB_DOUBLE)
      DR=ALGAMA(X)
      IF(ABS(DR) .GT. 1D-14) ER=ABS((DR-T(I))/DR)
      ERRMAX =MAX(ERRMAX,ER)
         WRITE(LOUT,'(1X,F10.3,1P,D27.18,D10.1)') X,DR,ER
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX=X
                 RT(I)=T(I)
                 RDR=ALGAMA(RX)
      IF(ABS(RDR) .GT. 1E-4)RER=ABS((RDR-RT(I))/RDR)
      IF(ABS(RDR) .LE. 1E-4)RER=ABS((RDR-RT(I)))
                 X=RX
      ERRMAX =MAX(RERRMAX,RER)
         WRITE(LOUT,'(1X,F10.3,1P,E27.9,E10.1)')
     +             RX ,RDR,RER
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DLGAMA(X)
      IF(ABS(DR) .GT. 1D-14)  ER=ABS((DR-T(I))/DR)
      ERRMAX =MAX(ERRMAX,ER)
         WRITE(LOUT,'(1X,F10.3,1P,E27.18,E10.1)') X,DR,ER
      ENDIF
#endif
    1 CONTINUE

#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)||defined(CERNLIB_IBM)||defined(CERNLIB_IBMRT)||defined(CERNLIB_IBMAIX)
      ETOL=TOLIBM(JF)
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_IBMAIX))
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''LARGEST RELATIVE ERROR WAS'',1P,E10.1)')
     +ERRMAX
      LTEST1=LTEST1.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:'')')
#if !defined(CERNLIB_DOUBLE)
      DR=ALGAMA(Z0)
      DR=ALGAMA(-Z1)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ0=Z0
                 RZ1=Z1
      DR=ALGAMA(RZ0)
      DR=ALGAMA(-RZ1)
      ENDIF
      IF(JF.EQ.2)THEN
      DR=DLGAMA(Z0)
      DR=DLGAMA(-Z1)
      ENDIF
#endif
1000  CONTINUE

      RETURN
      END

