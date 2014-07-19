*
* $Id: zetece.F,v 1.1.1.1 1996/03/06 10:47:05 mclareni Exp $
*
* $Log: zetece.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE ZETECE

C-    Check conversions of Hollerith to internal CETA code

#include "zebra/zbcd.inc"
#include "zebra/zceta.inc"
#include "zebra/zunit.inc"
#include "test_include/cqc.inc"
#include "zebra/quest.inc"
#include "zebra/zkrakcc.inc"
C--------------    End CDE                             --------------
      DIMENSION    ILETTV(64), IHOLL(80)
      EQUIVALENCE (ILETTV, IQLETT)
      DIMENSION    MPAK(2)
#if defined(CERNLIB_QMVDS)
      SAVE         MPAK
#endif
      DATA  MPAK   / 6, 4 /


      NFAIL = 0

C----              Test IZBCD

      CALL UCOPY (IQLETT,IQHOLK,63)

      DO 23  J=1,63
      JV = IZBCD (IQHOLK(J))
      IQCETK(J) = JV
   23 IHOLL(J)  = IQLETT(JV)

      JFAIL = 0
      DO 24  JF=1,63
      IF (IHOLL(JF).EQ.IQHOLK(JF))  GO TO 24
      JFAIL = JF
      WRITE (IQPRNT,9024) JFAIL
 9024 FORMAT (1X/' ZETECE.  ****  IZBCD FAILS  at',I4/1X)
   24 CONTINUE

      IF (JFAIL.EQ.0)              GO TO 31
      NFAIL = JFAIL
      NQCF  = NQCF + 1
      WRITE (IQPRNT,9028) (IQHOLK(J),IHOLL(J),IQCETK(J),J,J=1,63)
 9028 FORMAT (10X,A4,1X,A4,2I4)

C----              Test IZBCDV

   31 CALL VZERO (IQCETK,64)
      CALL IZBCDV (63)

      DO 33  J=1,63
      JV = IQCETK(J)
   33 IHOLL(J) = IQLETT(JV)

      JFAIL = 0
      DO 34  JF=1,63
      IF (IHOLL(JF).EQ.IQHOLK(JF))  GO TO 34
      JFAIL = JF
      WRITE (IQPRNT,9034) JFAIL
 9034 FORMAT (1X/' ZETECE.  ****  IZBCDV FAILS  at',I4/1X)
   34 CONTINUE

      IF (JFAIL.EQ.0)              GO TO 41
      NFAIL = JFAIL
      NQCF  = NQCF + 1
      WRITE (IQPRNT,9028) (IQHOLK(J),IHOLL(J),IQCETK(J),J,J=1,63)

C----              Test ZHTOI / ZITOH

   41 CALL UTRANS (IQLETT,IQHOLK,63,1,4)
      CALL ZHTOI  (IQHOLK,IQCETK,16)
      CALL ZITOH  (IQCETK,IHOLL,16)

      JFAIL = 0
      DO 44  JF=1,16
      IF (IHOLL(JF).EQ.IQHOLK(JF))  GO TO 44
      JFAIL = JF
      WRITE (IQPRNT,9044) JFAIL
 9044 FORMAT (1X/' ZETECE.  ****  ZHTOI/ZITOH FAIL  at',I4/1X)
   44 CONTINUE

      IF (JFAIL.EQ.0)              GO TO 51
      NFAIL = JFAIL
      NQCF  = NQCF + 1

      DO 49  JL=1,16
      CALL UPKBYT (IQCETK(JL),1,IQUEST,4,MPAK)
      WRITE (IQPRNT,9048) JL,IQHOLK(JL),IHOLL(JL)
     +,                   (IQUEST(J),J=1,4)
 9048 FORMAT (10X,I3,2X,A4,1X,A4,4I4)
   49 CONTINUE

C----              Test IZCHAV

   51 CALL VFILL  (IQCETK,64,57)
      CALL VFILL  (IQHOLK,64,IQQUES)
      CALL UH1TOC (IQLETT,CQHOLK,63)
      CALL IZCHAV (63)

      DO 53  J=1,63
      JV = IQCETK(J)
   53 IHOLL(J) = IQLETT(JV)

      JFAIL = 0
      DO 54  JF=1,63
      IF (IHOLL(JF).EQ.ILETTV(JF))  GO TO 54
      JFAIL = JF
      WRITE (IQPRNT,9055) JFAIL
 9055 FORMAT (1X/' ZETECE.  ****  IZCHAV FAILS  at',I4/1X)
   54 CONTINUE

      IF (JFAIL.EQ.0)              GO TO 61
      NFAIL = JFAIL
      NQCF  = NQCF + 1
      WRITE (IQPRNT,9056) (CQHOLK(J:J),IHOLL(J),IQCETK(J),J,J=1,63)
 9056 FORMAT (10X,A1,3X,A4,2I4)

C----              Dump translation table if errors

   61 IF (NFAIL.EQ.0)        RETURN

      WRITE (IQPRNT,9092)
 9092 FORMAT (1X/' Dump translation table :')

      JLE = 0
   93 JLA = JLE + 1
      IF (JLA.GT.NQTCET)     RETURN
      JLE = MIN (NQTCET,JLE+10)
      N   = JLE+1 - JLA

      CALL UCOPY (IQCETA(JLA),IQCETK,N)

      DO 94  J=1,N
      JV = IQCETK(J)
   94 IQHOLK(J) = IQLETT(JV)

      WRITE (IQPRNT,9094)  JLA,(IQCETK(J),J=1,N)
      WRITE (IQPRNT,9095)      (IQHOLK(J),J=1,N)
      GO TO 93

 9094 FORMAT (1X/8X,I5,5I3,2X,5I3)
 9095 FORMAT (15X,5A3,2X,5A3)
      END
*      ==================================================
#include "zebra/qcardl.inc"
