*
* $Id: cqpos.F,v 1.1.1.1 1996/03/06 10:47:00 mclareni Exp $
*
* $Log: cqpos.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:00  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQPOS

C-    Called from CQHEAD to position check-card file for a new lot

#include "zebra/zunit.inc"
#include "test_include/cqc.inc"
#include "test_include/cqc1.inc"
C--------------    End CDE                             --------------
      DIMENSION    MPACK(2)
#if defined(CERNLIB_QMVDS)
      SAVE         MPACK
#endif
      DATA  MPACK  /9,3/


      IF (NTAB.EQ.0)               GO TO 61
      IFLEOF= 0
      JTAB = LTAB
      IF (NAME.NE.NAML)            GO TO 21
      IF (NUM.LT.NUML)             GO TO 21
      IF (NUM.LE.NUME)             GO TO 41

C----              Locate table-entry for  name/num

   21 CONTINUE
   22 JTAB= IUFIND (NAME,NAMTAB,JTAB+1,NTAB)
      IF (JTAB.LE.NTAB)            GO TO 24
      IF (IFLEOF.NE.0)             GO TO 61
      IFLEOF= 7
      JTAB = 0
      GO TO 22

   24 CALL UPKBYT (NUMTAB(JTAB),1,NUMA,2,MPACK)
      IF (NUM.LT.NUMA)             GO TO 22
      IF (NUM.GT.NUME)             GO TO 22
      IF (JTAB.LE.LTAB)            GO TO 48

C----              Position to header card

   41 LTAB = JTAB
   42 CONTINUE
      READ (IQCIN,8999,ERR=59,END=47) (M(J),J=1,10)
      CALL UBUNCH (M,NAML,4)
      NUML= IUFORW (M,5,7)
      JCRD= IUFORW (M,8,10)
      IF (NUML.EQ.0)               GO TO 42
      IF (JCRD.NE.0)               GO TO 42
      IF (NAML.NE.NAME)            GO TO 42
      IF (NUML.NE.NUM)             GO TO 42
      RETURN

   47 IF (IFLEOF.NE.0)             GO TO 61
   48 REWIND IQCIN
      WRITE (IQPRNT,9048)
      IFLEOF= 7
      GO TO 41

C---               No check-cards
   59 WRITE (IQPRNT,9059)
   61 IQCHEK= 0
      NQCF  = NQCF + 1
      WRITE (IQPRNT,9061)
      RETURN

 8999 FORMAT (10A1)
 9048 FORMAT (' CQPOS.   ****  Rewind when searching check-cards')
 9059 FORMAT (' CQPOS.   ****  Read error on check-cards')
 9061 FORMAT (' CQPOS.   ****  Checking cancelled.')
      END
*      ==================================================
#include "zebra/qcardl.inc"
