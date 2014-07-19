*
* $Id: d105m.F,v 1.1.1.1 1996/04/01 15:01:22 mclareni Exp $
*
* $Log: d105m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:22  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D105M
#include "iorc.inc"
C     Test program for the GENLIB routines TRIINT,DTRINT (D105)
C     The program compares numerical solution with analytical solutions
C     for simple-to-evaluate integrals.  T Hepworth 11.5.90
C     Set the number of tests
      PARAMETER ( NT=3 )
#include "gen/def64.inc"
     +    X(NT,3),Y(NT,3),EPS,DTRINT,
     +    EXACT(NT),SOL,ERROR,ERRMAX,TSTERR,
     +    X1,X2,X3,Y1,Y2,Y3,
     +    D105S1,D105S2,D105S3
      INTEGER          NPT(3)
C     Set the largest absolute error allowed for a successful test
      PARAMETER ( TSTERR=1D-11 )
      EXTERNAL D105S1,D105S2,D105S3
      DATA NPT / 7,25,64 /
      DATA (X(1,J),Y(1,J),J=1,3) /0D0,0D0, 6D0,0D0, 3D0,4D0 /
      DATA EXACT(1) /158D0/
      DATA (X(2,J),Y(2,J),J=1,3) /-6D0,2D0, 6D0,7D0, 2D0,10D0/
      DATA EXACT(2) /28D0/
      DATA (X(3,J),Y(3,J),J=1,3) /-1D0,2D0, 0D0,1D0, 2D0,3D0/

      EXACT(3)= 1.6666666666666667D0
      CALL HEADER('D105',0)
      EPS=1D-6
C     Initialise largest error
      ERRMAX=0D0
      DO 100 I=1,NT
         X1=X(I,1)
         Y1=Y(I,1)
         X2=X(I,2)
         Y2=Y(I,2)
         X3=X(I,3)
         Y3=Y(I,3)
         WRITE(LOUT,'(/'' Test Number'',I3)') I
C        Perform test for NPT is 7,25,64
         DO 50 J=1,3
C           Perform test for NSD is 0 and 1
            DO 40 NSD=0,1
               IF (I .EQ. 1) THEN
#if defined(CERNLIB_DOUBLE)
                  SOL=DTRINT(D105S1,NSD,NPT(J),EPS,X1,Y1,X2,Y2,X3,Y3)
#endif
#if !defined(CERNLIB_DOUBLE)
                  SOL=TRIINT(D105S1,NSD,NPT(J),EPS,X1,Y1,X2,Y2,X3,Y3)
#endif
               ELSEIF (I .EQ. 2) THEN
#if defined(CERNLIB_DOUBLE)
                  SOL=DTRINT(D105S2,NSD,NPT(J),EPS,X1,Y1,X2,Y2,X3,Y3)
#endif
#if !defined(CERNLIB_DOUBLE)
                  SOL=TRIINT(D105S2,NSD,NPT(J),EPS,X1,Y1,X2,Y2,X3,Y3)
#endif
               ELSE
#if defined(CERNLIB_DOUBLE)
                  SOL=DTRINT(D105S3,NSD,NPT(J),EPS,X1,Y1,X2,Y2,X3,Y3)
#endif
#if !defined(CERNLIB_DOUBLE)
                  SOL=TRIINT(D105S3,NSD,NPT(J),EPS,X1,Y1,X2,Y2,X3,Y3)
#endif
               ENDIF
               ERROR= ABS( EXACT(I)-SOL )
               WRITE(LOUT,'('' NPT='',I3,6X,''NSD='',I3)') NPT(J),NSD
               WRITE(LOUT,'('' Calculated    '',F25.16)') SOL
               WRITE(LOUT,'('' Exact         '',F25.16)') EXACT(I)
               WRITE(LOUT,'('' Absolute Error'',F25.16)') ERROR
               ERRMAX=MAX( ERRMAX,ERROR )
 40         CONTINUE
 50      CONTINUE
100   CONTINUE
      WRITE(LOUT,'('' Largest Absolute Error was'',1P,D10.1)') ERRMAX
C     Check if the test ws successful
      IRC=ITEST('D105',ERRMAX .LE. TSTERR)
      CALL PAGEND('D105')
      RETURN
      END
C
