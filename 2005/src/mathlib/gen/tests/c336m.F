*
* $Id: c336m.F,v 1.1.1.1 1996/04/01 15:01:18 mclareni Exp $
*
* $Log: c336m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:18  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C336M
C     This program tests the operation of MATHLIB subprograms
C     SININT, COSINT and DSININ, DCOSIN (C336)
#include "imp64r.inc"
      REAL SININT,COSINT
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)
#include "iorc.inc"

      PARAMETER (Z0 = 0)
      LOGICAL LTEST
      DIMENSION Y(6),SIEX(6),CIEX(6)
      REAL     RY(6),RSIEX(6),RCIEX(6)
      DATA TOL/1D-6, 1D-13/
      DATA C1 /0.57721 56649 01532 86D0/

      DATA (Y(I),I=1,6)/
     +  -10.00D0 , -5.00D0 ,-0.10D-09 ,0.10D-11 , 1.00D0 ,300.00D0 /


      DATA (SIEX(I),CIEX(I),I=1,6)/
     +     -1.65834759421887390D0   ,  -0.454564330044553728D-01,
     +     -1.54993124494467405D0   ,  -0.190029749656643931D0  ,
     +     -0.999999999999999778D-10, -22.4486352650389200D0    ,
     +      0.999999999999999778D-12, -27.0538054510270101D0    ,
     +      0.946083070367182893D0  ,   0.337403922900967324D0  ,
     +      1.57088108821374939D0   ,  -0.333219991859211068D-02/



      DATA LTEST/.TRUE./
      CALL HEADER('C336',0)

#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR SININT and COSINT'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)
     +WRITE(LOUT,'(/10X,''TEST FOR SININT and COSINT'')')
      IF(JF.EQ.2)
     +WRITE(LOUT,'(/10X,''TEST FOR DSININT and DCOSINT'')')
#endif

      DEPS=1D-14
      REPS=1D-7
      WRITE(LOUT,100)
      DO 1 I = 1, 6
      X=Y(I)
#if !defined(CERNLIB_DOUBLE)
      S= SININT(X)
      C= COSINT(X)
      ERS= ABS((S-SIEX(I))/SIEX(I))
      ERC= ABS((C-CIEX(I))/CIEX(I))
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX=X
                 RZ0=Z0
                 RC1=C1
                 RSIEX(I)=SIEX(I)
                 RCIEX(I)=CIEX(I)
      RS= SININT(RX)
      RC= COSINT(RX)
      IF(ABS(RSIEX(I)).GE. 1E-8)ERS= ABS((RS-RSIEX(I))/RSIEX(I))
      IF(ABS(RCIEX(I)).GE. 1E-8)ERC= ABS((RC-RCIEX(I))/RCIEX(I))
      IF(ABS(RSIEX(I)).LT. 1E-8)ERS= ABS (RS-RSIEX(I))
      IF(ABS(RCIEX(I)).LT. 1E-8)ERC= ABS (RC-RCIEX(I))
            S=RS
            C=RC
      ENDIF
      IF(JF.EQ.2)THEN
      S=DSININ(X)
      C=DCOSIN(X)
      ERS= ABS(( S-SIEX(I))/SIEX(I))
      ERC= ABS(( C-CIEX(I))/CIEX(I))
      ENDIF
#endif
         ERRMAX=MAX( ERRMAX,ERC,ERS )
      IF(I.LT.4)
     +WRITE(LOUT,'(1X,F10.2,2(F10.2,D10.1))') X,S,ERS,C,ERC
      IF(I.GE.4 .AND. I .LT. 6)
     +WRITE(LOUT,'(1X,D10.2,2(D10.2,D10.1))') X,S,ERS,C,ERC
      IF(I.GE.6)
     +WRITE(LOUT,'(1X,F10.2,2(F10.2,D10.1))') X,S,ERS,C,ERC
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/''LARGEST RELATIVE ERROR WAS'',D10.1)')
     +ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0
      WRITE(LOUT,'(/''TESTING ERROR MESSAGES:''/)')
#if !defined(CERNLIB_DOUBLE)
      C= COSINT(Z0)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RZ0=Z0
      RC= COSINT(RZ0)
      ENDIF
      IF(JF.EQ.2)C=DCOSIN(Z0)
#endif

1000  CONTINUE
  100 FORMAT('1'/1X,6X,'X',8X,'SI(X)',2X,'SI ERROR',5X,'CI(X)',
     1       2X,'CI ERROR'/)
C     Check if the test was successful
      IRC=ITEST('C336',LTEST)
      CALL PAGEND('C336')
      RETURN
      END
