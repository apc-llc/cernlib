*
* $Id: tjmuan.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tjmuan.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TJMUAN (CHNAME,MUSTAN)

C-    Check the AN parameters are what they should be

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
      DIMENSION    MUSTAN(20)


      NAN = MUSTAN(1)
      IF (IQ(LQAN).NE.NAN)              GO TO 91

      DO  24  JL=1,NAN
      IF (IQ(LQAN+JL).NE.MUSTAN(JL+1))  GO TO 92
   24 CONTINUE
      RETURN

C----              Faults

   91 WRITE (IQLOG,9091) IQ(LQAN),NAN
 9091 FORMAT (/' !!! ',5X,'# of parameters have/must =',2I4)
      GO TO 97

   92 WRITE (IQLOG,9092) JL, IQ(LQAN+JL), MUSTAN(JL+1)
 9092 FORMAT (/' !!! ',5X,'Faulty parameter',I3,
     F'  value have/must =',2I4)

   97 WRITE (IQLOG,9097) CHNAME
 9097 FORMAT (10X,'Fault in processor "',A,'   !!!')
      CALL ZFATAL
      END
*      ==================================================
#include "zebra/qcardl.inc"
