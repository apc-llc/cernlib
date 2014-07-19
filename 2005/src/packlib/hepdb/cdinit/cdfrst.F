*
* $Id: cdfrst.F,v 1.1.1.1 1996/02/28 16:24:14 mclareni Exp $
*
* $Log: cdfrst.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:14  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDFRST (IDIV)
*     ========================
*
************************************************************************
*                                                                      *
*        SUBR. CDFRST (IDIV)                                           *
*                                                                      *
*   Initializes of the Data Base Package (for each file)               *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDIV     User Division                                           *
*                                                                      *
*   Called by CDINIT                                                   *
*                                                                      *
************************************************************************
*
      PARAMETER       (MNSYS = 10000)
*
*             ZEBRA system common blocks
*
#include "zebra/zunit.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
      COMMON /ZEBQ/  IQFENC(4), LQZEB(100)
*
#include "hepdb/caopti.inc"
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/cinitl.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
#include "hepdb/csavbk.inc"
#include "hepdb/cuserf.inc"
#if defined(CERNLIB__P3CHILD)
#include "hepdb/p3dbl3.inc"
#endif
      CHARACTER       CHFOR*11, CHTAG(TENCK)*8, CHFRM*20
      CHARACTER       CHNAM*62
      LOGICAL         DONE
      SAVE            DONE
*
      DATA            CHFOR /'IIIIIIIIIII'/, DONE /.FALSE./
*
*     Reserved fields in lower case
*
      DATA            CHTAG /'SERIAL_#', 'POINTER ', 'FLAGS   ',
     +                       'INS_TIME', 'key 5   ', 'SOURCEID',
     +                       'SW_REF_#', 'key 8   ', 'key 9   ',
     +                       'key 10  '/
 
*     DATA            CHTAG /'SERIAL_#', 'POINTER ', 'STR_VALI',
*    +                       'END_VALI', 'PRG_VERS', 'FLAGS   ',
*    +                       'INS_TIME', 'KEY 8   ', 'KEY 9   '/
      DATA            CHNAM
     +/'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'/
*
*     ------------------------------------------------------------------
*
* *** DB System initialization
*
      IF (DONE) GO TO 999
      DONE   = .TRUE.
      LPRTCD = IQPRNT
      IDIVCD = IDIV
      IDEBCD = 0
      LQST   = LOCF(LQ(1)) - 1
      LQZB   = LQST + 1 - LOCF(LQZEB(1))
      CALL MZSDIV (0, -1)
      KOFSCD = KQS - LQZB
      CALL MZSDIV (IDIVCD, -1)
      KOFUCD = KQS - LQZB
      MXSYS  = 4*NQDMAX(KQT+1)/5
      CALL MZDIV  (IDIVCD, IDISCD, 'DB-SYSTM', MNSYS, MXSYS, 'LCM')
      CALL MZLINK (IDIVCD, '/CLINKS/', LSTRCL(1), LSTRCL(1),
     +             LREFCL(10))
      CALL MZLINK (IDIVCD, '/CDUSCM/', LTOPCD, LBDACD, LBUPCD)
      CALL MZLINK (IDISCD, '/CSAVBK/', LOBJCS(1), LOBJCS(1),
     +             LOBJCS(NSVMCS))
      CALL MZLINK (0, '/CDCBLK/', LCDRCD, LCDRCD, LJNKCD)
*
* *** Initialize the IO descriptors
*
      ICHR0  = ICHAR ('0')
      CHFRM  = '4I '//CHAR(NWNOCD/10+ICHR0)//
     +                CHAR(MOD(NWNOCD,10)+ICHR0)//'B -H'
      CALL MZFORM ('NOCD', CHFRM, IONOCD)
      CALL MZFORM ('UPCD', '10I 4H -I', IOUPCD)
      CALL MZFORM ('DICD', '1I / 5I 1F 22H', IODICD)
      CALL UCTOH  ('KYCD', IHKYCD, 4, 4)
      CHFRM  = CHAR(MFZDIR+ICHR0-1)//'H / 1I 20H'
      CALL MZFORM ('FZDB', CHFRM, IOFZCD)
      CALL MZFORM ('FDDB', '/ 1I 20H', IOFDCD)
*
      INSRCD = 0
      LTOPCD = 0
      NTOPCD = 0
      CALL CDPKTS (991231, 235959, IBIGCD, IRC)
*
      NUFZCF = 0
      KEY7CK = 0
      CHFTCK = CHFOR
      DO 10 I = 1, TENCK
   10 CHTGCK(I) = CHTAG(I)
*
* *** Character string
*
      DO 15 K = 1, 62
   15 CALFCA(K) = CHNAM(K:K)
      CALL VZERO (IOPACA, MXKYCA+26)
      MPAKCA(1) = 8
      MPAKCA(2) = 4
#if !defined(CERNLIB_MIP)
      CSTRCA( 32: 47) = ' !"#$%&''()*+,-./'
#endif
#if defined(CERNLIB_MIP)
      CSTRCA( 32: 47) = '  "#$%&''()*+,-./'
      CSTRCA( 33: 33) = '21'X
#endif
      CSTRCA( 48: 57) = '0123456789'
      CSTRCA( 58: 64) = ':;<=>?@'
      CSTRCA( 65: 90) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
#if defined(CERNLIB_IBM)
      CSTRCA( 91: 96) = '[\]^_`'
#endif
#if defined(CERNLIB_BSLASH)||defined(CERNLIB_IBMRT)||defined(CERNLIB_MIP)
      CSTRCA( 91: 96) = '[\\]^_`'
#endif
#if (!defined(CERNLIB_BSLASH))&&(!defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_MIP))
      CSTRCA( 91: 96) = '[ ]^_`'
      CSTRCA( 92: 92) = CHAR(92)
#endif
      CSTRCA( 97:122) = 'abcdefghijklmnopqrstuvwxyz'
      CSTRCA(123:126) = '{|}~'
#if !defined(CERNLIB_IBM)
      CSTRCA(127:127) = CHAR(186)
#endif
#if defined(CERNLIB_IBM)
      CSTRCA(127:127) = CHAR(106)
#endif
*
      TOPLCI = ' '
#if defined(CERNLIB__P3CHILD)
*
      NWDBP3 = 0
      LIDBP3 = 0
      LODBP3 = 0
      IXDBP3 = IDIVCD
      CALL MZLINK (IXDBP3, '/P3DBL3/', LNK1P3, LNK1P3, LNK9P3)
      IPASP3 = 0
      NDIRP3 = 0
      NBKDP3 = 0
      NBKYP3 = 0
      INDXP3 = 1
      CALL MZBOOK (IDISCD, LNK1P3, 0, 2, 'DBP3',   0, 0, 122, 2, 0)
      CALL MZBOOK (IXDBP3, LNK2P3, 0, 2, 'P3LK', 100, 0, 100, 2, 0)
#endif
*
* *** User common block reset only if IFLGCU .ne. -1
*
      IF (IFLGCU.NE.-1) THEN
        IDCMCU = 0
        IPRECU = 0
        DELTCU = 0.
      ENDIF
*                                                             END CDFRST
  999 END
