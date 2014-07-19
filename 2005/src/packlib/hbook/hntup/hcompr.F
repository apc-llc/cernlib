*
* $Id: hcompr.F,v 1.1.1.1 1996/01/16 17:07:57 mclareni Exp $
*
* $Log: hcompr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 17/12/91  17.49.06  by  Fons Rademakers
*-- Author :    Fons Rademakers
      SUBROUTINE HCOMPR(STR,I2)
************************************************************************
*                                                                      *
* HCOMPR                                                               *
*                                                                      *
* Removes all blanks from a string.                                    *
*                                                                      *
* INPUT:                                                               *
*     STR    (C)    Character string with possible blanks              *
*     I2     (I)    Length of STR                                      *
*                                                                      *
* OUTPUT:                                                              *
*     STR    (C)    Same character string without any blanks           *
*     I2     (I)    Length of modified STR                             *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)  STR
*
      INTEGER I1, I2, I
*
      I1 = 1
*
 10   IF (STR(I1:I1) .EQ. ' ') THEN
         DO 20 I = I1+1, I2
            STR(I-1:I-1) = STR(I:I)
 20      CONTINUE
         STR(I2:I2) = ' '
         I2 = I2 - 1
         IF (I1 .LT. I2) GOTO 10
      ELSE
         I1 = I1 + 1
         IF (I1 .LT. I2) GOTO 10
      ENDIF
*
      END
 
