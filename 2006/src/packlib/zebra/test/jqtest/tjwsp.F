*
* $Id: tjwsp.F,v 1.1.1.1 1996/03/06 10:47:02 mclareni Exp $
*
* $Log: tjwsp.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE TJWSP (CHNAME,MDATF,MDATL,MULI,MUDA)

C-    Check the working space parameters are what they should be

#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "tejqdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "zebra/jzuc.inc"
C--------------    End CDE                             --------------
      CHARACTER    CHNAME*4
      DIMENSION    MDATF(48),MDATL(48),MULI(24),MUDA(24)
      DIMENSION    LINKWS(4)
      EQUIVALENCE (LINKWS(1), LX1)


      NLINK = LOCF (MDATF) - LQSTOR - 1
      NDATA = LOCF (MDATL) - LQSTOR + 1
      NDATX = LQSTA(KQT+1)

      IF (NLINK.NE.NQLINK)         GO TO 91
      IF (NDATA.NE.NDATX)          GO TO 92

      DO  24  JL=1,4
      IF (LINKWS(JL).NE.MULI(JL))  GO TO 93
      IF ( MDATF(JL).NE.MUDA(JL))  GO TO 94
   24 CONTINUE
      RETURN

C----              Faults

   91 WRITE (IQLOG,9091) NQLINK,NLINK
 9091 FORMAT (/' !!! ',5X,
     F'Faulty size of wsp links, value have/must =',2I4)
      GO TO 97

   92 WRITE (IQLOG,9092) NDATX,NDATA
 9092 FORMAT (/' !!! ',5X,
     F'Faulty size of wsp data, value have/must =',2I4)
      GO TO 97

   93 WRITE (IQLOG,9093) JL, LINKWS(JL), MULI(JL)
 9093 FORMAT (/' !!! ',5X,'Faulty wsp link',I2,' value have/must =',
     F2I7)
      GO TO 97

   94 WRITE (IQLOG,9094) JL, MDATF(JL), MUDA(JL)
 9094 FORMAT (/' !!! ',5X,'Faulty wsp data',I2,' value have/must =',
     F2I7)

   97 WRITE (IQLOG,9097) CHNAME
 9097 FORMAT (10X,'Fault in processor "',A,'   !!!')
      CALL ZFATAL
      END
*      ==================================================
#include "zebra/qcardl.inc"
