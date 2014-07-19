*
* $Id: c201m.F,v 1.1.1.1 1996/04/01 15:01:13 mclareni Exp $
*
* $Log: c201m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:13  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C201M
C     Test of MATHLIB routines SNLEQ and DSNLEQ (C201)
#include "gen/imp64.inc"
#include "iorc.inc"
      LOGICAL  RETOK,ERROK
      EXTERNAL C201S
      COMMON /FLABEL/ L
      DIMENSION X(10),F(10),W(130)
      INTEGER   MESS(4)
C     Set maximum error for test still to be considered successful
      PARAMETER ( TSTERR=1D-11,Z1=1D0 )

      CALL HEADER('C201',0)
      L=0
      CALL C201S(1,X,F,1)

      TOL=1D-12
C     Initialse maximum error for all tests
      RESMAX=0D0
C     Set intermediate stage printing on/off
      IF (LOUT .EQ. 6) THEN
         IPRNT=1
      ELSE
         IPRNT=0
      ENDIF
      DO 1 L = 1,4
         WRITE(LOUT,'(//'' Test number'',I3)') L
         IF (L .EQ. 1) THEN
           N=10
           H=Z1/(N+1)
           DO 10 I = 1,N
   10      X(I)=I*H*(I*H-1)
         ELSEIF(L .EQ. 2) THEN
           N=2
           X(1)=-0.9
           X(2)=1.7
         ELSEIF(L .EQ. 3) THEN
           N=2
           X(1)=1
           X(2)=4
         ELSE
           N=2
           X(1)=45
           X(2)=600
         ENDIF
        MAXF=50*(N+3)

C*RC    FNORM1=0
        DO 11 I = 1,N
          CALL C201S(N,X,F,I)
C*RC11   FNORM1=MAX(FNORM1,ABS(F(I)))
11      CONTINUE

#if defined(CERNLIB_DOUBLE)
        CALL DSNLEQ(N,X,F,TOL,TOL,MAXF,IPRNT,INFO,C201S,W)
#endif
#if !defined(CERNLIB_DOUBLE)
        CALL SNLEQ(N,X,F,TOL,TOL,MAXF,IPRNT,INFO,C201S,W)
#endif
        FNORM2=0
        DO 12 I = 1,N
          CALL C201S(N,X,F,I)
   12   FNORM2=MAX(FNORM2,ABS(F(I)))
        WRITE(LOUT,100) FNORM2,INFO,(X(I),I=1,N)
C       Calculate the maximum of the max-norms of residuals
        RESMAX=MAX( RESMAX,FNORM2 )
        MESS(L)=INFO
    1 CONTINUE

C     Check return codes
      IF (MESS(1) .GE. 1 .AND. MESS(1) .LE. 3 .AND.
     +    MESS(2) .GE. 1 .AND. MESS(2) .LE. 3 .AND.
     +    MESS(3) .GE. 1 .AND. MESS(3) .LE. 3 .AND.
     +    MESS(4) .GE. 1 .AND. MESS(4) .LE. 3) THEN
         RETOK=.TRUE.
      ELSE
         RETOK= .FALSE.
      ENDIF
C     Check if the desired accuracy was acheieved
      IF (RESMAX .LE. TSTERR) THEN
         ERROK=.TRUE.
      ELSE
         ERROK=.FALSE.
      ENDIF
      IRC=ITEST('C201',RETOK .AND. ERROK)
      CALL PAGEND('C201')

  100 FORMAT(//' FINAL MAX-NORM OF THE RESIDUALS',D15.7/
     +         ' EXIT PARAMETER  (INFO)           ',I10/
     +         ' FINAL APPROXIMATE SOLUTION:'//34(4X,3F25.15/))
      RETURN
      END

