*
* $Id: cqinit.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: cqinit.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQINIT (IUNIT)

C-    Initialise CQ-package

#include "zebra/zbcd.inc"
#include "zebra/zunit.inc"
#include "test_include/cqc.inc"
#include "test_include/cqc1.inc"
C--------------    End CDE                             --------------


      PARAMETER    (MAXTAB = 100)


      IQCIN= IUNIT
      NQCF = 0
      NTAB = 0
      IQCOPT(8) = 0
      WRITE (IQPRNT,9000) IQCIN
      IF (IQCIN.EQ.0)        RETURN

C----              Tabulate check-card file

      NAML= 0
      REWIND IQCIN
      WRITE (IQPRNT,9020)
   21 CONTINUE
      READ (IQCIN,8999,ERR=44,END=29) (M(J),J=1,10)
      JCRD= IUFORW (M,8,10)
      IF (JCRD.NE.0)               GO TO 21
      NUM = IUFORW (M,5,7)
      IF  (NUM.EQ.0)               GO TO 21
      CALL UBUNCH (M,NAME,4)
      IF (NAME.EQ.IQBLAN)          GO TO 21
      IF (NAME.NE.NAML)            GO TO 31
      IF (NUM.NE.NUML+1)           GO TO 31
      NUML= NUM
      GO TO 21

   29 NAME= -1

C----              New series,  complete last entry

   31 IF (NTAB.EQ.0)               GO TO 41
      CALL SBYT (NUML,NUMTAB(NTAB),10,9)
      WRITE (IQPRNT,9031) NAML,NUMA,NUML

C----              New series,  open next entry

   41 IF (NAME.EQ.-1)              GO TO 46
      IF (NTAB.EQ.MAXTAB)          GO TO 45
      NTAB= NTAB + 1
      NAMTAB(NTAB)= NAME
      NUMTAB(NTAB)= NUM
      NAML= NAME
      NUMA= NUM
      NUML= NUM
      GO TO 21

C----              End reached

   44 WRITE (IQPRNT,9044)
      GO TO 46
   45 WRITE (IQPRNT,9045)
   46 WRITE (IQPRNT,9046) NTAB
      REWIND IQCIN
      LTAB= 0
      NAML= 0
      RETURN

 9000 FORMAT (1X/' CQINIT.  Check-package starting, data on unit',I4)
 8999 FORMAT (10A1)
 9020 FORMAT (10X,'Check-card table building')
 9031 FORMAT (10X,A4,I3,3H TO,I4)
 9044 FORMAT (10X,'****  Read error on check-cards.')
 9045 FORMAT (10X,30H****  TABLE-CAPACITY EXCEEDED.)
 9046 FORMAT (10X,'Table built for',I5,' series.')
      END
*     ==================================================
#include "zebra/qcardl.inc"
