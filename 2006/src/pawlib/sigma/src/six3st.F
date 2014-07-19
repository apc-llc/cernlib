*
* $Id: six3st.F,v 1.1.1.1 1995/12/12 14:36:21 mclareni Exp $
*
* $Log: six3st.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:21  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.06/00 14/08/89  15.16.08  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIX3ST
C    ..................................................
C
C
C         PURPOSE
C            TO SAVE TOP ITEM IN STACK, AND ELIMINATE NEXT 1,2,3...
C
C         USAGE
C            CALL SIX3ST
C            CALL SIX4ST
C
C         COMM. BLOCKS USED
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            TRACE
C            SILSKK
C            SISTAK
C            SIUSTC
C            SIUSTK
C            SILSTK
C            SIMSTK
C
C         AUTHOR.   R.ALLARIA  DATE 06/11/73
C
C...  PAW VERSION  ...  MAY 1988
C
C    ..................................................
C
      DIMENSION LL(6)
      CHARACTER CL*8
      J=2
      GO TO 10
      ENTRY SIX4ST
      J=3
10    CONTINUE
      CALL SITRAC(' SIX3ST  ')
      CALL SILSKK(0,LL,CL)
      CALL SISTAK(0,MP,MN)
      CALL SIUSTC
      DO 20 I=1,J
20    CALL SIUSTK
      CALL SILSTK(LL,CL)
      CALL SIMSTK(MP,MN)
C
 999  END
