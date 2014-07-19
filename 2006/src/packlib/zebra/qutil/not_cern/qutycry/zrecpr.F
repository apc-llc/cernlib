      SUBROUTINE ZRECPR

C-    PRINT  DUMP + TRACE-BACK

#include "zebra/zbcd.inc"
#include "zebra/zunit.inc"
#include "zebra/quest.inc"
#include "zebra/qexchcry.inc"

      POINTER      (LCODE,CODE(99))

#include "zebra/q_jbyt.inc"


      LPADR = JBYT (IXP(1),27,22)
      JPPAR = JBYT (IXP(1),25, 2)
      JPLET = IQLETT(JPPAR+1)

      WRITE (IQLOG,9000) ISEC,LPADR,JPLET,LPADR,JPLET
 9000 FORMAT (10H0ZRECOV.  ,10(1H-),
     F 10H  JCONDT =,I4,6H  P = ,O8,A1,' OCT = HEX ',Z6,1X,A1/1X)

      IF (IQUEST(5).LT.0)          GO TO 32

      WRITE (IQLOG,9011)
 9011 FORMAT (20X,6H-----A,25X,1HS/1X)

      WRITE (IQLOG,9012) (IXP(J),IXP(J+8), J=1,8)
 9012 FORMAT (10X,Z16,10X,Z16)

      WRITE (IQLOG,9013) IVMR,IESW,ISEC
 9013 FORMAT (1H0,4X,5HVMR= ,Z16,5X,5HESW= ,Z16,5X,5HSEC= ,Z16/1X)
      IF (IQUEST(5).EQ.0)          GO TO 32

      LGO  = LPADR - IQUEST(5)
      LEND = LPADR + IQUEST(5)

      DO 27  LL=LGO,LEND,4
      LCODE = LL
      WRITE (IQLOG,9024) LL,(CODE(J),J=1,4)
 9024 FORMAT (4X,Z6,4Z20)
   27 CONTINUE

      WRITE (IQLOG,9027)
 9027 FORMAT (1X)

   32 CALL TRBK (IQLOG)
      RETURN
      END
