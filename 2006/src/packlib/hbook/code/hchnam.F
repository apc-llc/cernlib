*
* $Id: hchnam.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hchnam.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 07/04/93  12.23.53  by  Fons Rademakers
*-- Author :    Fons Rademakers   02/04/93
      SUBROUTINE HCHNAM(CHPATH, IUNIT, PATH)
*.==========>
*.
*.     The pathname of a chain referencing an already open unit is
*.     converted to a pathname containing the open unit.
*.     E.g.: //CHAIN/BLABLA becomes //LUN2/BLABLA when IUNIT=202
*.
*..=========> ( A.A. Rademakers )
*
      CHARACTER*(*) CHPATH, PATH
      CHARACTER*2   CLUN
*
      IF (CHPATH .EQ. ' ') THEN
         PATH = ' '
         RETURN
      ENDIF
*
      CALL HITOC(IUNIT-200, CLUN, LC, IER)
      PATH = '//LUN'//CLUN
      LP = LENOCC(PATH)
*
*-- string must start with //, redundant IF statement
*
      IF (CHPATH(1:2) .EQ. '//') THEN
         I = INDEX(CHPATH(3:), '/')
         IF (I .NE. 0) THEN
            PATH = PATH(1:LP)//CHPATH(I+2:)
         ENDIF
      ENDIF
*
      END
