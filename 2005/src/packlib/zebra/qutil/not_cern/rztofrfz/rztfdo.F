*
* $Id: rztfdo.F,v 1.1.1.1 1996/03/06 10:47:20 mclareni Exp $
*
* $Log: rztfdo.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:20  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE RZTFDO(CH)
#include "rzpawc.inc"
      CHARACTER*1 CH
C
      CALL MZEBRA(-3)
      CALL MZPAW(MEMOR,' ')
      CALL FZFILE(2,900,'O'//CH)
      CALL RZFILE(1,'RZ',' ')
      CALL RZSTAT('//RZ',99,' ')
      CALL RZTOFZ(2,'C')
      CALL FZENDO(2,'TE')
      CALL RZEND('RZ')
C
      END
