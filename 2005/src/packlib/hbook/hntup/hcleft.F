*
* $Id: hcleft.F,v 1.3 1997/09/05 09:02:59 couet Exp $
*
* $Log: hcleft.F,v $
* Revision 1.3  1997/09/05 09:02:59  couet
* - Previous fixe where not enough: the returned string could be equal to '.'
*   which is not a valid number.
*
* Revision 1.2  1997/08/20 15:54:47  couet
* This routine now remove the useless zeros a the end of the floating point
* numbers. This causes problems in the hntdup routine because the first token of
* a ntuple variable declaration may be too long and the user may received the
* following error message:
*
*  ***** ERROR in HBNAME : Error in token  : ID= 4
*
* Revision 1.1.1.1  1996/01/16 17:07:57  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/00 28/01/92  17.49.21  by  Fons Rademakers
*-- Author :    Fons Rademakers   28/01/92
      SUBROUTINE HCLEFT(STR, IBEG, IEND)
************************************************************************
*                                                                      *
* CLEFT (vsn 1.00)                                                     *
*                                                                      *
* 1) Left adjust a character string between IBEG and IEND.             *
* 2) Supress the useless 0                                             *
*                                                                      *
************************************************************************
*
      CHARACTER*(*) STR
      CHARACTER*80  ST
      INTEGER       IBEG, IEND, I
*
      ST = STR(IBEG:IEND)
*
* Left adjust 
*
      DO 10 I = 1, IEND-IBEG+1
         IF (ST(I:I) .NE. ' ') THEN
            STR(IBEG:IEND) = ST(I:)
            GOTO 20
         ENDIF
   10 CONTINUE
*
* Supress the useless 0
*
   20 ILAST = LENOCC(STR)
   30 IF (STR(ILAST:ILAST).EQ.'0') THEN
         STR(ILAST:ILAST) = ' '
         ILAST = ILAST-1
         GOTO 30
      ENDIF
      IF (STR(1:1).EQ.'.'.AND.LENOCC(STR).EQ.1) THEN
         STR='0.'
      ENDIF
*
      END
