*
* $Id: c300m.F,v 1.2 1999/09/24 14:55:52 mclareni Exp $
*
* $Log: c300m.F,v $
* Revision 1.2  1999/09/24 14:55:52  mclareni
* Add external erf,erfc,derf,derfc to test Cernlib versions, but it may not be enough on all systems
*
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C300M

C     Routine to test MATHLIB routines ERF, ERFC, DERF, and DERFC (C300)

#include "imp64r.inc"
      REAL ERF,ERFC,SFERF,GAUSS
C     Set maximum error allowed for test to be considered successful
      DIMENSION TOL(2),TOLIBM(2)

#include "gen/def64.inc"
     + X,Z0,Z1

      LOGICAL LTEST
      EXTERNAL FERF,SFERF,GAUSS
      EXTERNAL ERF, ERFC, DERF, DERFC
      PARAMETER (Z0 = 0, Z1 = 1)
#include "iorc.inc"
      DATA LTEST/.TRUE./
      DATA TOLIBM/5D-6, 1D-13/
      DATA TOL/5D-6, 5D-14/

C     PI=4*ATAN(Z1)
      PI = 3.14159 26535 89793D0
      EPS=1D-15
      REPS=1D-7

      CALL HEADER('C300',0)

#if defined(CERNLIB_DOUBLE)
      NF=2
#endif
#if !defined(CERNLIB_DOUBLE)
      NF=1
#endif
      DO 1000 JF=1,NF
         ERRMAX= 0.0D0
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/10X,''TEST FOR ERF and ERFC'')')
      WRITE(LOUT,'(/9X,''X'',14x,''ERF'',22X,''ERFC'',19X,
     +''ERF+ERFC'',9X,''Error'')')
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)WRITE(LOUT,'(/10X,''TEST FOR ERF and ERFC'')')
      IF(JF.EQ.1)
     +WRITE(LOUT,'(/9X,''X'',14x,''ERF'',22X,''ERFC'',19X,
     +''ERF+ERFC'',9X,''Error'')')
      IF(JF.EQ.2)WRITE(LOUT,'(/10X,''TEST FOR DERF and DERFC'')')
      IF(JF.EQ.2)
     +WRITE(LOUT,'(/9X,''X'',13x,''DERF'',21X,''DERFC'',19X,
     +''DERF+DERFC'',9X,''Error'')')
#endif

      DO 1 I = -80,80
      X=I/10D0
#if !defined(CERNLIB_DOUBLE)
      F=ERF(X)
      FC=ERFC(X)
      T=(2/SQRT(PI))*GAUSS(SFERF,Z0,X,EPS*ABS(F))
      DR=0
      IF(F .NE. 0) DR=ABS((F-T)/F)
         ERRMAX= MAX( ERRMAX,DR )
#endif
#if defined(CERNLIB_DOUBLE)
      IF(JF.EQ.1)THEN
                 RX=X
                 RZ0=Z0
                 RF=ERF(RX)
                 RFC=ERFC(RX)
      RT=(2/SQRT(PI))*GAUSS(SFERF,RZ0,RX,REPS*ABS(RF))
      RDR=0
      IF(RF .NE. 0) RDR=ABS((RF-RT)/RT)
        RERRMAX= ERRMAX
        ERRMAX= MAX(RERRMAX,RDR )
                 RX=X
                 F=RF
                 FC=RFC
                 DR=RDR
      ENDIF
      IF(JF.EQ.2)THEN
      F=DERF(X)
      FC=DERFC(X)
      T=(2/SQRT(PI))*DGAUSS(FERF,Z0,X,EPS*ABS(F))
      DR=0
      IF(F .NE. 0) DR=DABS((F-T)/F)
         ERRMAX= MAX( ERRMAX,DR )
      ENDIF
#endif

    1 WRITE(LOUT,'(1X,F10.1,2D25.15,F25.15,1P,D10.1)') X,F,FC,F+FC,DR

#if !defined(CERNLIB_DOUBLE)
      ETOL=TOL(JF+1)
#endif
#if defined(CERNLIB_DOUBLE)||defined(CERNLIB_IBM)||defined(CERNLIB_IBMRT)||defined(CERNLIB_IBMAIX)
      ETOL=TOLIBM(JF)
#endif
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_IBMAIX))
      ETOL=TOL(JF)
#endif
      WRITE(LOUT,'(/'': Largest Absolute Error was'',
     + 1P,D10.1)')ERRMAX
      LTEST=LTEST.AND.(ERRMAX.LE.ETOL)
         ERRMAX= 0D0

1000  CONTINUE
      IRC=ITEST('C300',LTEST)
      CALL PAGEND('C300')
      END
      FUNCTION FERF(T)
#include "gen/imp64.inc"

      FERF=EXP(-T**2)
      RETURN
      END
      FUNCTION SFERF(T)

      SFERF=EXP(-T**2)
      RETURN
      END
