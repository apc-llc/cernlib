*
* $Id: fqtest.F,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqtest.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
#include "test_include/pilot.h"
      SUBROUTINE FQTEST

#include "zebra/zstate.inc"
#include "zebra/zunit.inc"
#include "zebra/mzbits.inc"
#include "zebra/quest.inc"
#include "test_include/cqc.inc"
#include "zebra/cqbkc.inc"
#if defined(CERNLIB_QFIMDANY)
#include "fqmbuf.inc"
#endif
#include "test_include/testla.inc"
#include "test_include/testll.inc"
#include "test_include/testdd.inc"
#include "test_include/testiq.inc"
#include "test_include/testee.inc"
#include "test_include/fqtlun.inc"
C--------------    END CDE                             --------------


      CHARACTER    CHFILE*8, CHCL*8, CHST*4, CHFM*(*)
      LOGICAL      THERE
#if defined(CERNLIB_QFIASC)
      PARAMETER    (CHFM='FORMATTED')
#endif
#if !defined(CERNLIB_QFIASC)
      PARAMETER    (CHFM='UNFORMATTED')
#endif
      CHARACTER    FQNAME(3)*(12)
#if defined(CERNLIB_QFICDANY)
      EXTERNAL     FQCPUT, FQCGET
#endif
#include "fqtest/fqnameda.inc"


C     NQDEVZ = 7
      CALL VZERO (IFLXQ,10)

C----              Create the store and its divisions

      CALL MZSTOR (IXSTOR,'//', '.', FENCE
     +,            LQ,LA1,LX1,LQ(NQLIM2),IQ(NQN))

      CALL MZLOGL (IXSTOR,1)
      CALL MZWORK (IXSTOR,M,DLAST,0)
      CALL VZERO  (LQUSER,150)
      CALL MZDIV (IXSTOR,IXHOLD,'HOLD',1000,2000, 'L')
      CALL MZDIV (IXSTOR,IXDV3, 'DIV3',4000,6000, '.')
      CALL MZDIV (IXSTOR,IXDV4, 'DIV4',4000,6000, 'R')

C--                Initialize the d/s creation

      CALL CQBKIN
#if !defined(CERNLIB_QFINDAN)
      MMBKD(5) =  9
      CALL MZFORM ('D1',      '1I 2H 12I 35F 10D 10F / *S' , JJ)
#endif
      CALL MZLOGL (IXSTOR,0)

C----              Ready FZ test operation

*            file alfa
#if defined(CERNLIB_QFIASC)
      CHFILE = 'AIO'
*           file native
#endif
#if defined(CERNLIB_QFINDAN)
      CHFILE = 'IO'
*           file exchange, data native
#endif
#if defined(CERNLIB_QFIXDAN)
      CHFILE = 'XNIO'
#endif
#if (defined(CERNLIB_QFIXDAN))&&(defined(CERNLIB_QMIBM)||defined(CERNLIB_QMIBMD))
      CHFILE = 'FXNIO'
*           file exchange, data exchange
#endif
#if defined(CERNLIB_QFIXDAX)
      CHFILE = 'XIO'
#endif
#if (defined(CERNLIB_QFIXDAX))&&(defined(CERNLIB_QMIBM)||defined(CERNLIB_QMIBMD))
      CHFILE = 'FXIO'
*           file direct, data native
#endif
#if defined(CERNLIB_QFIDDAN)
      CHFILE = 'DNIO'
*           file direct, data exchange
#endif
#if defined(CERNLIB_QFIDDAX)
      CHFILE = 'DIO'
*           file channel, data native
#endif
#if defined(CERNLIB_QFICDAN)
      CHFILE = 'CNIO'
*           file channel, data exchange
#endif
#if defined(CERNLIB_QFICDAX)
      CHFILE = 'CIO'
*           file library C, data native
#endif
#if defined(CERNLIB_QFILDAN)
      CHFILE = 'LNIO'
*           file library C, data exchange
#endif
#if defined(CERNLIB_QFILDAX)
      CHFILE = 'LIO'
*           file library C random, data native
#endif
#if defined(CERNLIB_QFIRDAN)
      CHFILE = 'LDNIO'
*           file library C random, data exchange
#endif
#if defined(CERNLIB_QFIRDAX)
      CHFILE = 'LDIO'
*           file memory, data native
#endif
#if defined(CERNLIB_QFIMDAN)
      CHFILE = 'MNIO'
*           file memory, data exchange
#endif
#if defined(CERNLIB_QFIMDAX)
      CHFILE = 'MIO'
#endif

      LUNT1  = 11
      LUNT2  = 12
      LUNT3  = 13

      INTAPE = 0
      NWREC  = 0
      LEVOUT = 0
      LEVIN  = 0
      MINPR  = 19
      MAXPR  = 1
C-    MAXMAK = 255     is the maximum
      MAXMAK = 24
      MKBREA = 71

C--       Execution options :
C-           IFLXQ(1)  :  print FZINFO results
C-           IFLXQ(2)  :  FQMAKE : rewind the file,
C-                        skip to end-of-run and continue writing
C-           IFLXQ(3)  :  write place-holder for the Direct Access Table
C-           IFLXQ(4)  :  save the Direct Access Table
C-           IFLXQ(5)  :  reset DAT forward ref. record to null
C-           IFLXQ(6)  :  running in 'memory' mode

#if defined(CERNLIB_QFIXDANY)
      IFLXQ(2) = 7
      IFLXQ(3) = 7
      IFLXQ(4) = 7
#endif
#if defined(CERNLIB_QFIDDANY)||defined(CERNLIB_QFIRDANY)
      IFLXQ(3) = 7
      IFLXQ(4) = 7
#endif
#if defined(CERNLIB_QFIMDANY)
      IFLXQ(6) = 7
#endif

#include "test_include/maxmak.inc"
* Ignoring t=pass

      WRITE (IQLOG,9011) CHFILE, MAXMAK
 9011 FORMAT (1X/' *!!!!!!  Test of FZ with option ',A/
     F' *!!!!',I9,' used for MAXMAK')

C----------        Create the FZ file    --------------

      IF (INTAPE.NE.0)             GO TO 41

#if defined(CERNLIB_QFINDAN)
C--       Test first closing of unused files
      CALL FQUNUS
#endif

C----              Delete the file if it exists

      LUNFZ  = LUNT1
      INQUIRE (FILE=FQNAME(1), EXIST=THERE)
      IF (.NOT.THERE)              GO TO 24
      OPEN (LUNFZ,FILE=FQNAME(1),STATUS='OLD',ERR=23,FORM=CHFM)
   23 CLOSE (LUNFZ,STATUS='DELETE',ERR=24)
   24 CONTINUE

C----              Open the file

      CHCL = 'w+ '
      CHST = 'NEW'
      JF   = 1
#include "fqtopmk.inc"
* Ignoring t=pass
#include "fqtest/fqtopen.inc"
#if defined(CERNLIB_QFILDANY)||defined(CERNLIB_QFIRDANY)
      IQUEST(1) = LUNPTR(JF)
#endif
      CALL FZFILE (LUNFZ, NWREC, CHFILE)
#if defined(CERNLIB_QFICDANY)
      CALL FZHOOK (LUNFZ, FQCPUT, NIL)
#endif
#if defined(CERNLIB_QFIMDANY)
      CALL FZMEMO (LUNFZ,MEMBUF,NMEMOR)
#endif

C----              Write the file

      CALL FQMAKE
#if defined(CERNLIB_QFIXDANY)
      CALL FZRUN  (LUNFZ,-1,0,0)
#endif

C----              Close the file

      CALL FQRSET (-1,0)
#if defined(CERNLIB_QFIDDANY)||defined(CERNLIB_QFILDANY)
      CALL FZENDO (LUNFZ,'TX')
#endif
      CALL FZENDO (0,'TX')
      CALL FQTINF (0)

#if defined(CERNLIB_QFICDANY)||defined(CERNLIB_QFIMDANY)
      CLOSE (LUNFZ)
#endif

C----------        Read back             --------------

   41 LOGLEV = LEVIN
#if (!defined(CERNLIB_QFANOUT))&&(!defined(CERNLIB_QFANCOP))
      NLUNT = 1
#endif
#if defined(CERNLIB_QFANOUT)||defined(CERNLIB_QFANCOP)
      NLUNT = 3
#endif

C----              Open the file(s) for read-back

      DO 49  JF=1,NLUNT
      LUNFZ = LUNTV(JF)
      IF (JF.NE.1)                 GO TO 43
      CHCL  = 'r  '
      CHST  = 'OLD'
#include "fqtopbk.inc"
* Ignoring t=pass
      GO TO 47

   43 CHCL  = 'w+ '
      CHST  = 'NEW'
      INQUIRE (FILE=FQNAME(JF), EXIST=THERE)
      IF (.NOT.THERE)              GO TO 46
      OPEN (LUNFZ,FILE=FQNAME(JF),STATUS='OLD',ERR=45,FORM=CHFM)
   45 CLOSE (LUNFZ,STATUS='DELETE',ERR=46)
   46 CONTINUE
#include "fqtopmk.inc"
* Ignoring t=pass

   47 CONTINUE
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
   49 CONTINUE

C----              Read back

#if (!defined(CERNLIB_QFANOUT))&&(!defined(CERNLIB_QFANCOP))&&(!defined(CERNLIB_QRANDOM))
      CALL FQBACK
#endif
#if defined(CERNLIB_QFANOUT)
      CALL FQBKFA
#endif
#if defined(CERNLIB_QFANCOP)
      CALL FQBKCO
#endif
#if defined(CERNLIB_QRANDOM)
      CALL FQBKRA
#endif
      RETURN
      END
*      ==================================================
#include "zebra/qcardl.inc"
