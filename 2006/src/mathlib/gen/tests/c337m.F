*
* $Id: c337m.F,v 1.1.1.1 1996/04/01 15:01:18 mclareni Exp $
*
* $Log: c337m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:18  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C337M
C     This program tests the operation of MATHLIB subprograms
C     REXPIN  and DEXPIN (C337)
#include "imp64r.inc"
c     REAL EXPINT
      REAL REXPIN
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2)
#include "iorc.inc"

      DIMENSION Y(9),YEX(9)
c     REAL     RY(9),RYEX(9)
      REAL           RYEX(9)
      PARAMETER (Z0 = 0)
      LOGICAL LTEST
      DATA TOL/1D-5, 1D-13/
      DATA DEPS /1D-12/, EPS /1E-8/,EPSS /1E-5/,DEL /1D-10/, XE /100/
      DATA (Y(I), I=1,9)/
     + 0.10D-12,0.70D0,2.80D0,15.00D0,-0.10D-05,-2.00D0,
     + -10.00D0,-20.00D0,-50.00D0/
      DATA (YEX(I), I=1,9)/
     +     0.2935639054402169D+02,
     +     0.3737688432335093D+00,
     +     0.1685529244521608D-01,
     +     0.1918627892147866D-07,
     +     0.1323829389306249D+02,
     +    -0.4954234356001892D+01,
     +    -0.2492228976241876D+04,
     +    -0.2561565266405658D+08,
     +    -0.1058563689713169D+21/

      DATA LTEST/.TRUE./
      CALL HEADER('C337',0)

#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR REXPIN'')')
      WRITE(LOUT,100)
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)
     +WRITE(LOUT,'(/10X,''TEST FOR REXPIN'')')
      IF(JF.EQ.1)WRITE(LOUT,101)
      IF(JF.EQ.2)
     +WRITE(LOUT,'(/10X,''TEST FOR DEXPINT'')')
      IF(JF.EQ.2)WRITE(LOUT,100)
#endif

      DO 1 I = 1,9
      X=Y(I)
#if !defined(CERNLIB_DOUBLE)
C     DRA= EXPINT(X)
      DRA= REXPIN(X)
      DRT=YEX(I)
      IF (DRT .NE. 0D0) ER= ABS((DRA-DRT)/DRT)
      WRITE(LOUT,'(1X,D12.2,D25.16,D12.1)') X,DRA,ER
#endif
#if defined(CERNLIB_DOUBLE)
      IF (JF.EQ.1) THEN
                   RX=X
                   RYEX(I)=YEX(I)
C     RDRA= EXPINT(RX)
      RDRA= REXPIN(RX)
      RDRT=RYEX(I)
      IF (RDRT .NE. 0D0) ER= ABS((RDRA-RDRT)/RDRT)
                    X=RX
                  DRA=RDRA
      WRITE(LOUT,'(1X,D12.2,D25.9,D12.1)') X,RDRA,ER
      ENDIF
      IF (JF.EQ.2) THEN
      DRA=DEXPIN(X)
      DRT=YEX(I)
      IF (DRT .NE. 0D0) ER= ABS((DRA-DRT)/DRT)
      WRITE(LOUT,'(1X,D12.2,D25.16,D12.1)') X,DRA,ER
      ENDIF
#endif
      ERRMAX = MAX (ERRMAX,ER)
   1  CONTINUE
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
C     RDRA=EXPINT(Z0)
      RDRA=REXPIN(Z0)
#endif
#if defined(CERNLIB_DOUBLE)
      IF (JF.EQ.1) THEN
                   RZ0=Z0
C                  RDRA=EXPINT(RZ0)
                   RDRA=REXPIN(RZ0)
      ENDIF
      IF (JF.EQ.2)DRA=DEXPIN(Z0)
#endif

1000  CONTINUE

  100 FORMAT('1'/1X,11X,'X',15X,'DEXPIN',8X,'REL.ERR.'/)
c 101 FORMAT('1'/1X,11X,'X',15X,'EXPINT',8X,'REL.ERR.'/)
  101 FORMAT('1'/1X,11X,'X',15X,'REXPIN',8X,'REL.ERR.'/)
C     Check if the test was successful
      IRC=ITEST('C337',LTEST)
      CALL PAGEND('C337')
      RETURN
      END
