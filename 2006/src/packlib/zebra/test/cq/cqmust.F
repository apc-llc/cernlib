*
* $Id: cqmust.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: cqmust.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQMUST (NW)

C-    Called from CQDATA to read the next check-card  (must-vector)


#include "zebra/zbcd.inc"
#include "zebra/zunit.inc"
#include "test_include/cqc.inc"
#include "test_include/cqc1.inc"
#include "zebra/quest.inc"
#include "zebra/zkrakc.inc"
C--------------    End CDE                             --------------


      IF (JCRD.EQ.1)  JCROFF=0

      READ (IQCIN,8001,ERR=76,END=77) NAMR,NUMR,JCRI,(IQHOLK(J),J=1,70)
      CALL IZBCDV (70)

      IF (NAMR.NE.NAME)            GO TO 71
      IF (NUMR.NE.NUM)             GO TO 71
      IF (JCRI.EQ.JCRD)            GO TO 19
      IF (JCROFF.EQ.999)           GO TO 19
      IF   (JCRI.NE.999)           GO TO 71
      JCROFF= 999

C--                Krack into  MUST(1/NW)

   19 MFLAGK = 0
      JEND   = 0
      JMU    = 0

   20 IF (JMU.GE.NW)         RETURN
      JMU  = JMU  + 1
      JGO  = JEND + 1
      JEND = JEND + 5
      KOFM = KODEFM(JMU)

      IF   (KOFM-5)          22, 33, 23
   22 IF (KOFM.EQ.1)               GO TO 31
      JEND = JEND + 5
   23 IF (KOFM.EQ.6)               GO TO 37
      IF (KOFM.EQ.7)               GO TO 37
      CALL ZKRAKN (MFLAGK,JGO,JEND,MUST(JMU))
      IF (IQUEST(1).EQ.2)          GO TO 20
      MUST(JMU) = 0
      GO TO 20

   31 JGO  = JGO + 6
      GO TO 36

   33 JGO  = JGO + 2
      CALL UBUNCH (IQHOLK(JGO),MUST(JMU),4)
      JMU  = JMU + 1
      JGO  = JGO + 4
   36 JEND = JEND + 5
   37 CALL UBUNCH (IQHOLK(JGO),MUST(JMU),4)
      GO TO 20

C----              Troubles

   71 WRITE (IQPRNT,9071)  NAMR,NUMR,JCRI
      GO TO 79
   76 WRITE (IQPRNT,9076)
      GO TO 79
   77 WRITE (IQPRNT,9077)
   79 WRITE (IQPRNT,9079)
      IQCHEK= 0
      NQCF  = NQCF + 1
      RETURN

 8001 FORMAT (A4,2I3,70A1)
 9071 FORMAT (1X/' CQMUST.  ****  Card out of sequence, card-id is '
     F,A4,2I3)
 9076 FORMAT (1X/' CQMUST.  ****  Read error on check cards.')
 9077 FORMAT (1X/' CQMUST.  ****  EOF out of sequence.')
 9079 FORMAT (   ' CQMUST.  ****  Checking cancelled.')
      END
*      ==================================================
#include "zebra/qcardl.inc"
