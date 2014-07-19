*
* $Id: fqread.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqread.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQREAD

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#if defined(CERNLIB_QFIMDANY)
#include "fqmbuf.inc"
#endif
#include "test_include/readiq.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------
      PARAMETER   (NHEA=40)
      DIMENSION    MHEA(NHEA)


      CHARACTER    CHFILE*8, CHCL*8, CHST*4, CHFM*(*)
#if defined(CERNLIB_QFIASC)
      PARAMETER    (CHFM='FORMATTED')
#endif
#if !defined(CERNLIB_QFIASC)
      PARAMETER    (CHFM='UNFORMATTED')
#endif
      CHARACTER    FQNAME(1)*24
#if defined(CERNLIB_QFICDANY)
      EXTERNAL     FQCPUT, FQCGET
#endif
#include "fqtest/fqnamrda.inc"


C     NQDEVZ = 7
      CALL VZERO (IFLXQ,10)

C----              Create the store and its divisions

      CALL MZSTOR (IXSTOR,'//', '.', FENCE
     +,            LQ,LA1,LX1,LQ(NQLIM2),IQ(NQN))

C     CALL MZLOGL (IXSTOR,1)
      CALL MZWORK (IXSTOR,M,DLAST,0)
      CALL VZERO  (LQUSER,150)
      CALL MZDIV (IXSTOR,IXHOLD,'HOLD',1000,2000, 'L')

C----              Ready FZ test operation
#if defined(CERNLIB_QFIASC)
      CHFILE = 'AI'
#endif
#if defined(CERNLIB_QFINDAN)
      CHFILE = 'I'
#endif
#if defined(CERNLIB_QFIXDAN)
      CHFILE = 'XNI'
#endif
#if defined(CERNLIB_QFIXDAX)
      CHFILE = 'XI'
#endif
#if defined(CERNLIB_QFIDDAN)
      CHFILE = 'DNI'
#endif
#if defined(CERNLIB_QFIDDAX)
      CHFILE = 'DI'
#endif
#if defined(CERNLIB_QFICDAN)
      CHFILE = 'CNI'
#endif
#if defined(CERNLIB_QFICDAX)
      CHFILE = 'CI'
#endif
#if defined(CERNLIB_QFILDAN)
      CHFILE = 'LNI'
#endif
#if defined(CERNLIB_QFILDAX)
      CHFILE = 'LI'
#endif
#if defined(CERNLIB_QFIRDAN)
      CHFILE = 'LDNI'
#endif
#if defined(CERNLIB_QFIRDAX)
      CHFILE = 'LDI'
#endif
#if defined(CERNLIB_QFIMDAN)
      CHFILE = 'MNI'
#endif
#if defined(CERNLIB_QFIMDAX)
      CHFILE = 'MI'
#endif

      LUNT1  = 11
      LUNT2  = 12
      LUNT3  = 13

      NWREC  = 0
      LEVOUT = 0
      LEVIN  = 0
      MINPR  = 19
      MAXPR  = 1
      MAXMAK = 99

#include "test_include/maxmak.inc"
* Ignoring t=pass

      WRITE (IQLOG,9011) CHFILE, MAXMAK
 9011 FORMAT (1X/' *!!!!!!  FZ simple read-back with option ',A/
     F' *!!!!',I9,' used for MAXMAK')

C----------        Read back             --------------

      LOGLEV = LEVIN

      CHCL  = 'r  '
      CHST  = 'OLD'
      LUNFZ = LUNT1
      JF    = 1
#include "fqtopmk.inc"
* Ignoring t=pass
#include "fqtest/fqtopen.inc"
#if defined(CERNLIB_QFILDANY)||defined(CERNLIB_QFIRDANY)
      IQUEST(1) = LUNPTR(JF)
#endif
      CALL FZFILE (LUNFZ, NWREC, CHFILE)
#if defined(CERNLIB_QFICDANY)
      CALL FZHOOK (LUNFZ, FQCGET, NIL)
#endif
#if defined(CERNLIB_QFIMDANY)
      CALL FZMEMO (LUNFZ,MEMBUF,NMEMOR)
#endif
      CALL FZLOGL (LUNFZ,LOGLEV)

C----              Read back

      JGOOD = 0

   41 NHEAD = NHEA
      CALL FZIN (LUNFZ,IXSTOR, LQMAIN,1, '.',NHEAD,MHEA)
      IF (IQUEST(1).NE.0)          GO TO 91
      JGOOD = JGOOD + 1

      WRITE (IQLOG,9044)  JGOOD
 9044 FORMAT (' Read d/s number',I5)

   69 CALL MZWIPE (IXSTOR+21)
      IF (JGOOD.GE.MAXMAK)         GO TO 98
      GO TO 41

C----              END OR BAD DATA

   91 IF (IQUEST(1).LT.0)    CALL ZFATAM ('FQREAD - Trouble.')
      IF (IQUEST(1).GE.5)          GO TO 98
      IF (IQUEST(1).LT.4)          GO TO 41
      CALL FZENDI (LUNFZ,'C')
      GO TO 41

   98 CALL FZENDI (LUNFZ,'TX')

      WRITE (IQLOG,9098) JGOOD
 9098 FORMAT (1X/' *!!!!--',I7,' d/s read successfully.')
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
