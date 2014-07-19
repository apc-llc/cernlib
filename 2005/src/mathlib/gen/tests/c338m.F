*
* $Id: c338m.F,v 1.1.1.1 1996/04/01 15:01:19 mclareni Exp $
*
* $Log: c338m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:19  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C338M
C     This program tests the operation of MATHLIB routines
c     CEXPIN and WEXPIN (C338)

#include "gen/impc64.inc"
      INTEGER IRC, ITEST
      COMPLEX   SI,SZ,SR,CEXPIN,SST
C     Set maximum error allowed for test to be considered successful
C     PARAMETER (NZ = 1000)
      PARAMETER (NZ = 10)
#include "gen/def64.inc"
     +       ZR(NZ),ZI(NZ),ERRMAX,TOL(2)
      REAL            SZR(NZ),SZI(NZ),SERRMAX
      EXTERNAL FC338

#if defined(CERNLIB_DOUBLE)
      PARAMETER (I = (0D0,1D0))
#endif
      PARAMETER (SI= (0E0,1E0))
      LOGICAL LTEST
#include "iorc.inc"

      DATA TOL/1D-6, 5D-11/
      DATA LTEST/.TRUE./
      CALL HEADER('C338',0)

#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/9X,''Z'',27X,''WEXPIN/EXPIN'',22X,''Error'')')
#endif
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/9X,''Z'',30X,''EXPIN'',25X,''Error'')')
#endif
      ERRMAX=0D0
      SERRMAX=0E0
      EPS=1D-13
      Z0=0
#if defined(CERNLIB_DOUBLE)
      CALL DVRAN(NZ,-50D0,50D0,ZR(1),ZR(2))
      CALL DVRAN(NZ,0D0,50D0,ZI(1),ZI(2))
#endif
      CALL RVRAN(NZ,-50E0,50E0,SZR(1),SZR(2))
      CALL RVRAN(NZ,0E0,50E0,SZI(1),SZI(2))
      DO 1 J = 1,NZ
#if defined(CERNLIB_DOUBLE)
      Z=ZR(J)+I*ZI(J)
#endif
      SZ=SZR(J)+SI*SZI(J)
      SSZ=SZ
#if defined(CERNLIB_DOUBLE)
      R=WEXPIN(Z)
#endif
      SR=CEXPIN(SZ)
#if defined(CERNLIB_DOUBLE)
      T=WGAUSS(FC338,Z0,Z,EPS)
      IF(ABS(R) .NE. 0)ERRMAX=MAX(ERRMAX,ABS((T-R)/R))
      IF(ABS(R) .EQ. 0)ERRMAX=MAX(ERRMAX,ABS(T-R))
      LTEST=LTEST .AND. ERRMAX .LE. TOL(2)
      ST=WGAUSS(FC338,Z0,SSZ,EPS)
      SST=ST
      IF(ABS(SR) .EQ. 0)SERRMAX=MAX(SERRMAX,ABS(SST-SR))
      LTEST=LTEST .AND. SERRMAX .LE. TOL(1)
      WRITE(LOUT,'(1X,''('',F7.3,'','',F7.3,'')'',2X,
     +''('',E22.15,'','',E22.15,'')'',1P,D10.1)')
     +  Z,R,ERRMAX
      WRITE(LOUT,'(1X,''('',F7.3,'','',F7.3,'')'',8X,
     +''('',E16.8,'','',E16.8,'')'',6X,1P,D10.1)')
     +  SZ,SR,SERRMAX
#endif
#if !defined(CERNLIB_DOUBLE)
      ST=CGAUSS(FC338,Z0,SZ,EPS)
      IF(ABS(SR) .NE. 0)SERRMAX=MAX(SERRMAX,ABS((ST-SR)/SR))
      IF(ABS(SR) .EQ. 0)SERRMAX=MAX(SERRMAX,ABS(ST-SR))
      LTEST=LTEST .AND. SERRMAX .LE. TOL(2)
      WRITE(LOUT,'(1X,''('',F7.3,'','',F7.3,'')'',2X,
     +''('',E22.15,'','',E22.15,'')'',1P,D10.1)')
     + SZ,SR,SERRMAX
#endif
    1 CONTINUE
#if !defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''Largest Error was'',1P,D10.1/)')
     +SERRMAX
#endif
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,'(/''SINGLE PREC. Largest Error'',1P,D10.1)')
     +SERRMAX
      WRITE(LOUT,'(/''DOUBLE PREC. Largest Error'',1P,D10.1/)')
     +ERRMAX
#endif
C     Check if the test was successful
      IRC=ITEST('C338',LTEST)
      CALL PAGEND('C338')
      RETURN
      END
      FUNCTION FC338(T)
#include "gen/impc64.inc"

      FC338=(1-EXP(-T))/T
      RETURN
      END
