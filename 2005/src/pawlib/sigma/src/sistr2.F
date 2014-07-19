*
* $Id: sistr2.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sistr2.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.20  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SISTR2(N)
C
C
C    ....................................................
C
C
C         PURPOSE
C            PERFORM UNSTACKING IF REQUIRED
C
C         USAGE
C            CALL SISTR2(N)
C
C         DESCRIPTION  OF PARAMETERS
C             N: SIUSTK N TIMES
C
C         COMM. BLOCKS USED
C            COM1
C
C         REMARKS
C            N = 0 FOR STRING
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SILSTK
C            SIMSTK
C            SILSKK
C            SISTAK
C            STORE
C            TRACE
C            SIUSTK  AND ENTRY SIUSTC
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C
      DIMENSION LL(6)
      CHARACTER CL*8
      CALL SITRAC (' SISTR2  ')
C
C     ITEM IS IN DYNA,STORE STACK ENTRY
C
      CALL SILSKK(0,LL,CL)
      CALL SISTAK(0,MP,MN)
C...  DO ONLY DECREMENT ISTAPO BY ONE
      CALL SIUSTC
C
C
C     SIUSTK N TIMES
C
C     STRING GENERATION REQUIRES N=0 CALL UNSTACKING NOTHING
      IF(N.EQ.0) GOTO 15
      DO 10 I=1,N
      CALL SIUSTK
  10  CONTINUE
C
  15  CONTINUE
C
C     RESTORE SAVED STACK ITEM
C
      CALL SILSTK(LL,CL)
      CALL SIMSTK(MP,MN)
C
C
 999  END
