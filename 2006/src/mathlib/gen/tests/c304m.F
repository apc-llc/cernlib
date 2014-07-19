*
* $Id: c304m.F,v 1.1.1.1 1996/04/01 15:01:14 mclareni Exp $
*
* $Log: c304m.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:14  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE C304M
#include "iorc.inc"
C     Program to test the MATHLIB routines DILOG and DDILOG (C304)

      REAL DILOG
#include "gen/def64.inc"
     +    DDILOG
#include "gen/def64.inc"
     +    DX(8),DTST(8),DD,ERR
      LOGICAL LER,LET
C     Error tolerances for successful tests
      DATA D,DD /1.0E-5,2.0D-15/

      DATA DX /-5.0D0,-1.9D0,-1.0D0,-0.8D0,-0.4D0,0.9D0,1.0D0,10.0D0/

      DATA DTST(1) /-0.27492 79126 06080 8D+1/
      DATA DTST(2) /-0.13812 66904 63337 9D+1/
      DATA DTST(3) /-0.82246 70334 24113 2D+0/
      DATA DTST(4) /-0.67978 15878 34681 1D+0/
      DATA DTST(5) /-0.36583 25775 12449 6D+0/
      DATA DTST(6) / 0.12997 14723 00495 9D+1/
      DATA DTST(7) / 0.16449 34066 84822 6D+1/
      DATA DTST(8) / 0.53630 12873 57862 7D+0/

      CALL HEADER('C304',0)
      LET=.FALSE.
      WRITE(LOUT,99)
      DO 1 IX = 1,7
#if defined(CERNLIB_DOUBLE)
         ERR=ABS( DILOG(SNGL(DX(IX)))-SNGL(DTST(IX)) )
         LER= ERR .GT. D
         WRITE(LOUT,200) DX(IX),DILOG(SNGL(DX(IX))),SNGL(DTST(IX)),ERR
#endif
#if !defined(CERNLIB_DOUBLE)
         ERR=ABS( DILOG(DX(IX))-DTST(IX) )
         LER=ERR .GT. D
         WRITE(LOUT,200) DX(IX),DILOG(DX(IX)),DTST(IX),ERR
#endif
         LET=LET .OR. LER
         IF (LER) WRITE(LOUT,100) DX(IX)
    1 CONTINUE

      IF(.NOT.LET) WRITE(LOUT,101)

C     Don't test DDILOG if double precision isn't being used
#if defined(CERNLIB_DOUBLE)
      WRITE(LOUT,*) 'TESTING DDILOG'
      WRITE(LOUT,99)
      LET=.FALSE.
      DO 2 IX = 1,7
         ERR=ABS ( DDILOG(DX(IX))-DTST(IX) )
         LER=ERR .GT. DD
         LET=LET .OR. LER
         WRITE(LOUT,200) DX(IX),DDILOG(DX(IX)),DTST(IX),ERR
         IF (LER) WRITE (6,102) DX(IX)
    2 CONTINUE
      IF(.NOT.LET) WRITE(LOUT,103)
#endif
C     Check if the test was successful
      IRC=ITEST('C304',.NOT. LET)
      CALL PAGEND('C304')

   99 FORMAT(8X,'X',15X,'Calculated',18X,'Exact',9X,'Absolute Error')
  100 FORMAT(1X,'C304 DILOG TEST FAILED FOR X = ',F6.1)
  101 FORMAT(1X,'C304 DILOG TEST SUCCESSFUL')
  102 FORMAT(1X,'C304 DDILOG TEST FAILED FOR X = ',F6.1)
  103 FORMAT(1X,'C304 DDILOG TEST SUCCESSFUL')
  200 FORMAT(1X,F8.2,2X,3F23.16)

      END
