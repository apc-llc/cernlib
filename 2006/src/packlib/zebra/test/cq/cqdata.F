*
* $Id: cqdata.F,v 1.1.1.1 1996/03/06 10:46:59 mclareni Exp $
*
* $Log: cqdata.F,v $
* Revision 1.1.1.1  1996/03/06 10:46:59  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE CQDATA (IDATA,IDATAL)

C-    Check, punch, list a partial lot of check-data,  user callled

#include "zebra/zunit.inc"
#include "test_include/cqc.inc"
#include "test_include/cqc1.inc"
C--------------    End CDE                             --------------
      DIMENSION    IDATA(9),IDATAL(9)
      DIMENSION    AM(100)
      EQUIVALENCE (AM(1),M(1))


      IF (IQCOPT(1).EQ.0)    RETURN


      NDT = LOCF(IDATAL(2)) - LOCF(IDATA)
      IF (NDT.LT.1)                GO TO 11
      IF (NDT.LT.1001)             GO TO 13
   11 WRITE (IQPRNT,9011) NDT
      CALL ZFATAL

   13 IF (IQCOPT(8).EQ.0)          GO TO 16
      ICHKSV = IQCHEK
      IQCHEK = 0


   16 JE = 0
   17 IF (JE.LT.NDT)               GO TO 18
      IF (IQCOPT(8).NE.0)  IQCHEK=ICHKSV
      RETURN

   18 NW = MIN (NDT-JE,NENTR)
      JD = JE + 1
      JE = JE + NW

C-------           Comparing

      IOPTPR = IQCOPT(2)
      IF (IQCHEK.EQ.0)             GO TO 41
      CALL CQMUST (NW)
      IF (IQCHEK.EQ.0)             GO TO 41
      CALL CQCOMP (IFAIL,NW,IDATA(JD),IDATA(JD))
      IF (IFAIL.EQ.0)              GO TO 41
      NQCF  = NQCF + 1
      IOPTPR=  7
      IQCHEK= -7

C-------           Listing

   41 IF (IOPTPR.EQ.0)             GO TO 51
      IF (NAMEPR.NE.0)             GO TO 42
      NAMEPR = 7
      WRITE (IQPRNT,9041) NAMEVC
   42 CALL CQOCT (IDATA,JD,JE,NPRI)
      WRITE (IQPRNT,MFLP,ERR=44) JCRD, (M(J),J=1,NPRI)
   44 CONTINUE

C-------           Punching

   51 IF (IQCOPT(8).NE.0)          GO TO 17
      IF (IQCOPT(3).EQ.0)          GO TO 61
      WRITE (IQPNCH,MFPU,ERR=54) NAME,NUM,JCRD,(IDATA(J),J=JD,JE)
   54 CONTINUE

C-------         Comparison  Dump

   61 IF (IQCHEK.GE.0)             GO TO 69
      IQCHEK= 7
      CALL CQOCT (MUST,1,NW,NPRI)
      WRITE (IQPRNT,MFLP,ERR=64) JCRD, (M(J),J=1,NPRI)
   64 CONTINUE
      WRITE (IQPRNT,9061)
      JF = JD + IFAIL-1
      WRITE (IQPRNT,9062) IFAIL,(IDATA(J), J=JF,JE)
      IF (IQCOPT(5).NE.0)    CALL ZFATAL
      WRITE (IQPRNT,9071)

   69 JCRD = JCRD + 1
      GO TO 17

 9011 FORMAT (1X/' CQDATA.  ****  Illegal number of data-words =',I7)
 9041 FORMAT (1X/' CQDATA. ',110A1)
 9061 FORMAT (1X,' CQDATA.  ****  CHECK FAILURE above,',
     F' Have-vector printed before Must-vector.')
#if !defined(CERNLIB_HEX)
 9062 FORMAT (8H CQDATA.,8X,'Octal dump follows, starting at word',
     FI4/(10X,5O23))
#endif
#if defined(CERNLIB_HEX)
 9062 FORMAT (8H CQDATA.,8X,'Hex dump follows, starting at word',
     FI4/(10X,5Z17))
#endif
 9071 FORMAT (1X)
      END
*      ==================================================
#include "zebra/qcardl.inc"
