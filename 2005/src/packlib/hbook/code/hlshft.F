*
* $Id: hlshft.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hlshft.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 13/01/93  21.09.21  by  Rene Brun
*-- Author :
      SUBROUTINE HLSHFT(ARRAY,IINDEX,LEFT,RIGHT,LGRHOLL)
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PART OF HSORT
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      INTEGER HLHCMP
C
      INTEGER ARRAY(*),IINDEX(*),LEFT,RIGHT,LGRHOLL
C
      INTEGER I,J
      INTEGER X(4),XX
C
C -- sort
      I  = LEFT
      J  = 2*LEFT
      CALL HLSWAP(X,ARRAY,1,LEFT)
C -- index
      XX = IINDEX(LEFT)
C
C -- sort
      IF((J.LT.RIGHT).AND.
     $     (HLHCMP(ARRAY(4*(J-1)+1),ARRAY(4*J+1),LGRHOLL).LT.0))THEN
         J = J+1
      ENDIF
C -- while
 10   IF((J.LE.RIGHT).AND.(HLHCMP(X,ARRAY(4*(J-1)+1),LGRHOLL).LT.0))THEN
         CALL HLSWAP(ARRAY,ARRAY,I,J)
         IINDEX(I) = IINDEX(J)
         I         = J
         J         = 2*J
         IF((J.LT.RIGHT).AND.
     $       (HLHCMP(ARRAY(4*(J-1)+1),ARRAY(4*J+1),LGRHOLL).LT.0))THEN
            J = J+1
         ENDIF
         GO TO 10
      ENDIF
C
      CALL HLSWAP(ARRAY,X,I,1)
      IINDEX(I) = XX
C
      END
C
