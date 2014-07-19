*
* $Id: hlsort.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlsort.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 13/01/93  21.09.21  by  Rene Brun
*-- Author :    P.Aubert   18/11/92
      SUBROUTINE HLSORT(ARRAY,IINDEX,LGRARRAY,LGRHOLL)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C HLSORT   : HEAP SORT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ARRAY    : ARRAY TO BE SORTED
C IINDEX   : ARRAY OF RESULT
C LGRARRAY : NUMBER OF WORD ( LENGTH OF ARRAY = 4*LGRARRAY )
C LGRHOLL  : NUMBER OF CHARACTER IN EACH WORD
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C EXEMPLE  :
C IN         ARRAY = 8 6 4 2 1
C OUTPUT     ARRAY = 1 2 4 6 8 IINDEX = 5 4 3 2 1
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER ARRAY(*),IINDEX(*),LGRARRAY,LGRHOLL
C
      INTEGER LEFT,RIGHT
      INTEGER X(4),XX
      INTEGER I
C
      LEFT = ( LGRARRAY / 2 ) + 1
      RIGHT = LGRARRAY
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C -- init index
      DO 1 I=1,LGRARRAY
         IINDEX(I)=I
 1    CONTINUE
C -- while
 10   IF(LEFT.GT.1)THEN
         LEFT = LEFT-1
         CALL HLSHFT(ARRAY,IINDEX,LEFT,RIGHT,LGRHOLL)
         GO TO 10
      ENDIF
C -- end while
C -- while
 20   IF(RIGHT.GT.1)THEN
C        -- swap ARRAY(1) and ARRAY(RIGHT)
         CALL HLSWAP(X,ARRAY,1,1)
         CALL HLSWAP(ARRAY,ARRAY,1,RIGHT)
         CALL HLSWAP(ARRAY,X,RIGHT,1)
C        -- index
         XX           = IINDEX(1)
         IINDEX(1)    = IINDEX(RIGHT)
         IINDEX(RIGHT)= XX
C        -- sort
         RIGHT        = RIGHT-1
         CALL HLSHFT(ARRAY,IINDEX,LEFT,RIGHT,LGRHOLL)
         GO TO 20
      ENDIF
C -- end while
C
      END
