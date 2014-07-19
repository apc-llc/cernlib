*
* $Id: tjmufl.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tjmufl.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TJMUFL (CHNAME,MUSTFL)

C-    Check the FL parameters are what they should be

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "tejqdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "zebra/jzuc.inc"
C--------------    End CDE                             --------------
      CHARACTER    CHNAME*4
      DIMENSION    MUSTFL(10)


      DO  24  JL=1,10
      IF (JQFLAG(JL).NE.MUSTFL(JL))  GO TO 92
   24 CONTINUE
      RETURN

C----              Faults

   92 WRITE (IQLOG,9092) JL, JQFLAG(JL), MUSTFL(JL)
 9092 FORMAT (/' !!! ',5X,'Faulty flag',I3,' value have/must =',2I4)

   97 WRITE (IQLOG,9097) CHNAME
 9097 FORMAT (10X,'Fault in processor "',A,'   !!!')
      CALL ZFATAL
      END
*      ==================================================
#include "zebra/qcardl.inc"
