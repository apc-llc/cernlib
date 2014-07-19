*
* $Id: dbhost.F,v 1.1.1.1 1996/02/28 16:24:07 mclareni Exp $
*
* $Log: dbhost.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:07  mclareni
* Hepdb, cdlib, etc
*
*
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_IBM)
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
#if !defined(CERNLIB__P3CHILD)
      SUBROUTINE DBHOST (IDTASK, NWUSER, IWUSER)
*     ==========================================
*
************************************************************************
*                                                                      *
*        SUBR. DBHOST (IDTASK,NWUSER,IWUSER)                           *
*                                                                      *
*   Service routine for HEPDB requests from child tasks                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDTASK   Child identifier                                        *
*     NWUSER   Number of words in the message vector                   *
*     IWUSER   Message vector                                          *
*                                                                      *
*   Called by <user>                                                   *
*                                                                      *
************************************************************************
*
#include "zunit.inc"
#include "mqsys.inc"
#include "caopti.inc"
#include "cduscm.inc"
#include "p3dbl3.inc"
      COMMON /P3ZEBR/ ZEBRP3(35), IDTZP3
*
      DIMENSION       IWUSER(9)
      DIMENSION       INKEYS(3), IARG(6), MLDBP3(8)
      CHARACTER       CHNAME*8 , OLDDIR*255
      SAVE            MLDBP3, IXSYP3, NBDBP3, KQSP3
*
      DATA            INITDB, INITZ, INITRZ, KQSP3  /0,0,0,0/
      DATA            INKEYS        /4HEBRA, 4HINIT, 4HDONE/
      DATA            CHNAME        /'ABCDEFGH'/
*
*     ------------------------------------------------------------------
*
      CALL APTBEG (INDXT)
*
*  ** Check that ZEBRA system has been initialized
*
      ICODE  = 0
      IF (INITZ.EQ.0) THEN
        DO 2 K=1,3
          IF (MQKEYS(K).NE.INKEYS(K))   GO TO 900
    2   CONTINUE
        IF (NQSTOR.LT.0)                GO TO 902
        LIDBP3 = 0
        LODBP3 = 0
        INITZ  = 1
        CALL P3UNIT (LPRTAP)
      ENDIF
*
*  ** Check that HEPDB system has been initialized
*
      IF (INITDB.EQ.0) THEN
        DO 4 K=1,8
          IF (CALFCA(K).NE.CHNAME(K:K)) GO TO 10
    4   CONTINUE
        INITDB = IDIVCD
      ENDIF
*
*  ** Load the message vector
*
   10 CALL APHTOC (IWUSER(NWUSER-1), RNDBP3, 8)
      IDDBP3 = IDTASK
      NWDBP3 = NWUSER - 2
      CALL UCOPY (IWUSER, IWDBP3, NWDBP3)
      MSDBP3 = ' '
      CALL APHTOC (IWUSER, MSDBP3, 8)
      IQDBP3 = -1
*
      CALL P3LOGQ (LOGLAP)
      IF (LOGLAP.GT.1) CALL CDPRNT (LPRTAP, '(/,'' DBHOST : Invoked '//
     +   'by Task'',I3,'', Routine '//RNDBP3//''')', IDTASK, 1)
*
*  ** Select action to take
*
      IF (RNDBP3.EQ.'CDRZIN') THEN
        ICODE = 7
      ELSE IF (RNDBP3.EQ.'CDAFRI') THEN
        ICODE = 3
      ELSE IF (RNDBP3.EQ.'CDATOI') THEN
        ICODE = 4
      ELSE IF (RNDBP3.EQ.'CDEND ') THEN
        ICODE = 6
      ELSE IF (RNDBP3.EQ.'CDINIT') THEN
        ICODE = 1
      ELSE IF (RNDBP3.EQ.'CDSNAM') THEN
        IF (MSDBP3.EQ.'JOURNAL') THEN
          ICODE = 2
        ELSE
          ICODE = 5
        ENDIF
      ELSE IF (MSDBP3.EQ.'JOURNAL') THEN
        ICODE = 2
      ELSE
        GO TO 910
      ENDIF
*
      GO TO (100,200,300,400,500,600,100), ICODE
*
*  ** CDINIT : open Database file in update mode
*
  100 IF (INITDB.NE.0)                  GO TO 150
*
*  ** Database not initialized: look for largest store
*
      IBIG   = 0
      MYSTOR = -1
      DO 120 JQ=0,NQSTOR
        KQ    = NQOFFT(JQ+1)
        ISIZE = LQSTA(KQ+21) - LQSTA(KQ+1)
        IF (ISIZE.GT.IBIG) THEN
          IBIG   = ISIZE
          MYSTOR = JQ
        ENDIF
  120 CONTINUE
*
*  ** Create a division in the largest store
*
      MYSTOR = ISHFT (MYSTOR, 26)
      CALL MZDIV (MYSTOR, INITDB, 'APDBL3', 5000, 16384, 'PR')
*
*  ** O.K., we can go now
*
  150 IF (ICODE .EQ.7)                  GO TO 700
*
      IF (NWDBP3.LT.8)                  GO TO 920
      LUN    = IWDBP3(1)
      MSDBP3 = ' '
      CALL APHTOC (IWDBP3(2), MSDBP3, 16)
      NRECS  = IWDBP3(6)
      CALL APHTOC (IWDBP3(7), MSDBP3(17:24), 8)
      IF (LOGLAP.GT.-1) THEN
        IF (LOGLAP.LT.2)
     +   CALL CDPRNT (LPRTAP, '(/,'' DBHOST : Invoked '//
     +   'by Task'',I3,'', Routine '//RNDBP3//''')', IDTASK, 1)
        IARG(1) = LUN
        IARG(2) = NRECS
        CALL CDPRNT (LPRTAP, '(''            Initialize directory '//
     +       MSDBP3(1:16)//' on unit'',I3,'', options '//MSDBP3(17:24)//
     +       ', record length'',I6)', IARG, 2)
      ENDIF
      CALL DBINIT (INITDB, LUN, MSDBP3(1:16), LTOP, NRECS,MSDBP3(17:24))
      IQDBP3 = IQUEST(1)
      IF (IQDBP3.EQ.-2) IQDBP3 = 0
      IF (LIDBP3.EQ. 0) CALL APOPCH (LIDBP3, 0, IQDBP3)
      NWDBP3 = 0
      CALL DBLOGL (LUN, LOGLAP)
      GO TO 800
*
*  ** "Journal file": update database
*
  200 IF (INITDB.EQ.0)                  GO TO 930
      IF (LOGLAP.GT.1) CALL CDPRNT (LPRTAP, '(''            Update '//
     +   'Database'')', IARG, 0)
      IF (LIDBP3.EQ.0) CALL APOPCH (LIDBP3, 0, IQDBP3)
      IQDBP3 = 0
      NWDBP3 = 0
      GO TO 800
*
*  ** Update Database by reading journal file
*
  250 IDTZP3 = IDDBP3
      CALL DBFZUP (LIDBP3,'S')
      GO TO 999
*
*  ** CDAFRI : copy text onto external file from a bank
*
  300 LUN    = IWDBP3(1)
      NDATA  = IWDBP3(2)
      IQDBP3 = 0
      IF (LIDBP3.EQ.0) CALL APOPCH (LIDBP3, 0, IQDBP3)
      NWDBP3 = 0
      GO TO 800
*
  350 CALL APFZIN (IDDBP3,JQPDVL,LBANK,2,'A',0,0)
      IF (IQUEST(1).NE.0)               GO TO 999
      NDATA = IQ(KQS+LBANK-1)
      IF (NDATA.LE.0)                   GO TO 999
      NTOT  = 0
  360 IF (NTOT.LT.NDATA) THEN
        CALL CDLIND (IQ(KQS+LBANK+1), NTOT, MSDBP3, LENG)
        IF (LENG.GT.0) THEN
          WRITE (LUN,'(A)') MSDBP3(1:LENG)
        ELSE
          WRITE (LUN,*)
        ENDIF
        GO TO 360
      ENDIF
      CALL MZDROP (JQPDVL, LBANK, 'L')
      GO TO 999
*
*  ** CDATOI : copy text from external file into a bank
*
  400 LUN   = IWDBP3(1)
      NDATA = NDMXCB
      CALL UCTOH  ('APDB', ITEMP3, 4, 4)
      CALL MZBOOK (JQPDVL, LBANK , 0, 2, ITEMP3, 0, 0, NDATA, 1, -1)
      NDAT  = 0
  410 READ (LUN, '(A80)', ERR=420, END=420) MSDBP3
      LENG  = LENOCC (MSDBP3)
      CALL CDLINC (MSDBP3, LENG, IQ(KQS+LBANK+1), NDAT)
      IF (NDAT.GT.NDATA-20) THEN
        IQDBP3 = -1
        GO TO 800
      ENDIF
      GO TO 410
  420 NDP   = NDAT - NDATA
      IF (NDP.LT.0) CALL MZPUSH (JQPDVL, LBANK, 0, NDP, 'I')
      IQDBP3 = 0
      NWDBP3 = 1
      IWDBP3(1) = NDAT
      IF (LODBP3.EQ.0) CALL APOPCH (LODBP3, 1, IQDBP3)
      LBANK = LZFIDH (JQPDVL, ITEMP3, 0)
      GO TO 800
*
  450 CALL APFZUT (IDDBP3, JQPDVL, LBANK, 1, 'S', 1, 0, 0)
      CALL MZDROP (JQPDVL, LBANK, '    ')
      GO TO 999
*
*  ** CDSNAM : copy bank from external file
*
  500 LUN    = IWDBP3(1)
      IF (LODBP3.EQ.0) CALL APOPCH (LODBP3, 1, IQDBP3)
      CALL FZIN (LUN, JQPDVL, LBANK, 2, 'A', 0, 0)
      IQDBP3 = IQUEST(1)
      NWDBP3 = 0
      GO TO 800
*
*  ** DBEND : ready for future applications
*
  600 IQDBP3 = 0
      NWDBP3 = 0
      GO TO 800
  650 CONTINUE
      GO TO 999
*
*  ** DBRZIN: retrieve set of data objects
*
  700 IF (INITRZ.EQ.0) THEN
        INITRZ = 1
        IXSTP3 = ISHFT(INITDB,-26)
        IXDBP3 = ISHFT(IXSTP3, 26) +  2
        IXSYP3 = ISHFT(IXSTP3, 26) + 24
        CALL MZLINK (IXDBP3, '/P3DBL3/', LNK1P3, LNK1P3, LNK9P3)
        CALL UCTOH  ('DBRZ', MLDBP3(1), 4, 4)
        MLDBP3(2) = 0
        MLDBP3(3) = 0
        MLDBP3(4) = 5
        CALL MZIOBK (MLDBP3, 8, '2I 1B 2F')
        CALL MZBOOK (IXSYP3, LNK1P3, 0, 2, 'DBP3', 0, 0, 122, 2, 0)
        NBDBP3 = 122
        KQSP3  = KQS
      ENDIF
*       Receive list of requests
      INDXP3 = IWDBP3(3)
      NWDBP3 = 0
      CHNAME = MSDBP3
      IF (LOGLAP.GT.1) CALL CDPRNT (LPRTAP, '(/,''           Called '//
     +   'by '//CHNAME//' with'',I6,'' data words'')', INDXP3, 1)
      IF (INDXP3.GT.NBDBP3) THEN
        INC = INDXP3-NBDBP3
        CALL MZNEED (IXSYP3, INC, 'G...')
        IF (IQUEST(11).LT.0) THEN
          IQDBP3 = 11
          GO TO 790
        ENDIF
        CALL MZPUSH (IXSYP3, LNK1P3, 0, INC, 'I...')
        NBDBP3 = INDXP3
      ENDIF
*
      CALL P3RECV (IDDBP3, IQ(KQSP3+LNK1P3+1), NW, NBDBP3, IQDBP3)
*
      IF (IQDBP3.EQ.0.AND.NW.LT.INDXP3) THEN
        IQDBP3 = 12
        GO TO 790
      ENDIF
*       Nb. of directories
      NDIRP3 = IQ(KQSP3+LNK1P3+1)
*       Prepare banks
      IF (LNK2P3.NE.0) CALL MZDROP (IXDBP3, LNK2P3, 'L...')
      CALL MZBOOK (IXDBP3, LNK2P3, 0, 2, 'DBP3', 2, 2, 0, 2, 0)
*       Retrieve objects
      NBRETR = 0
      INDXP3 = 1
      CALL RZCDIR (OLDDIR, 'R...')
*
      DO 740 NDIR = 1, NDIRP3
* Set directory
        CALL ZITOH  (IQ(KQSP3+LNK1P3+INDXP3+2), IWDBP3, 20)
        CALL UHTOC  (IWDBP3, 4, MSDBP3, 80)
        CALL RZCDIR (MSDBP3,'....')
        IF (IQUEST(1).NE.0) THEN
          IQDBP3 = 13
          GO TO 790
        ENDIF
* Nb. of keys
        NBKDP3 = IQ(KQSP3+LNK1P3+INDXP3+1)
* Read data banks
        INDXP3 = INDXP3+21
        DO 730 NBK = 1, NBKDP3
          INDXP3 = INDXP3+1
          ICUR = IQ(KQSP3+LNK1P3+INDXP3)
          IF (NBRETR.EQ.0) THEN
            LNK9P3 = LNK2P3
            JBP3 = -1
          ELSE
            LNK9P3 = LNK3P3
            JBP3 =  0
          ENDIF
          LBANK = LNK9P3
          CALL RZIN (IXDBP3, LBANK, JBP3, ICUR, 999999, 'S...')
          IF (IQUEST(1).NE.0) THEN
            IQDBP3 = 14
            GO TO 790
          ENDIF
          LNK3P3 = LQ(KQSP3+LNK9P3+JBP3)
          LNK9P3 = 0
* Save first words in a special bank (unpacking,backwards compatib.)
          CALL MZNEED (IXDBP3, 20, 'G...')
          IF (IQUEST(11).LT.0) THEN
            IQDBP3 = 15
            GO TO 790
          ENDIF
          IF (NBRETR.EQ.0) THEN
            CALL MZLIFT (IXDBP3, LNK4P3, LNK2P3,-2, MLDBP3, 0)
          ELSE
            CALL MZLIFT (IXDBP3, LBANK,  LNK4P3, 0, MLDBP3, 0)
            LNK4P3 = LBANK
          ENDIF
          DO 725 I = 1, 2
            IQ(KQSP3+LNK4P3+I  ) = IQ(KQSP3+LNK3P3+I)
             Q(KQSP3+LNK4P3+I+3) =  Q(KQSP3+LNK3P3+I)
  725     CONTINUE
          IQ(KQSP3+LNK4P3+3) = IQ(KQSP3+LNK3P3+3)
*
          NBRETR = NBRETR+1
  730   CONTINUE
*
        IF (LOGLAP.GT.1) CALL CDPRNT (LPRTAP, '(I10,'' Data objects '//
     +   'retrieved from '//MSDBP3//' '')', NBKDP3, 1)
*
  740 CONTINUE
      IF (LOGLAP.GT.1) CALL CDPRNT (LPRTAP, '(I10,'' Data objects '//
     +   'retrieved'')', NBRETR, 1)
      IQDBP3=0
*       Reset directory
      CALL RZCDIR (OLDDIR, '....')
      GO TO 800
*
* ** Transfer objects to child
*
  750 CALL APFZUT (IDDBP3, IXDBP3, LNK2P3, 1, '....', 2, 0, IWDBP3)
      CALL MZDROP (IXDBP3, LNK2P3, '....')
      CALL MZGARB (IXDBP3, 0)
      CALL MZDRED (IXDBP3)
      GO TO 999
*
* ** Errors
*
  790 IF (LOGLAP.LT.1)                  GO TO 800
      IF (LOGLAP.LT.2) THEN
        CALL CDPRNT (LPRTAP, '(/,'' DBHOST : Invoked '//
     +   'by Task'',I3,'', Routine '//RNDBP3//''')', IDTASK, 1)
        CALL CDPRNT (LPRTAP, '(/,''           Called '//
     +   'by '//CHNAME//' with'',I6,'' data words'')', INDXP3, 1)
      ENDIF
*
      GO TO (791,792,793,794,795), IQDBP3-10
*
  791 CALL CDPRNT (LPRTAP,
     +  '('' ******   Error : No more space available in system '//
     +     'division to store list of requests'')', IARG, 0)
      GO TO 800
*
  792 IARG(1) = INDXP3
      IARG(2) = NW
      CALL CDPRNT (LPRTAP,
     +  '('' ******   Error in P3RECV:'',I6,'' Words expected,'',
     +     I6,'' Words received'')', IARG, 2)
      GO TO 800
*
  793 CALL CDPRNT (LPRTAP,
     +  '('' ******   Error : Unknow directory '//MSDBP3//' '')',
     +     IARG, 0)
      GO TO 800
*
  794 IARG(1) = IQUEST(1)
      IARG(2) = ICUR
      CALL CDPRNT (LPRTAP,
     +  '('' ******   RZIN Error'',I6,'' for Key'',I5,'' in'//
     +     MSDBP3//' '')', IARG, 2)
      GO TO 800
*
  795 CALL CDPRNT (LPRTAP,
     +  '('' ******   Error : No more space available in user '//
     +     'division to store auxiliary data'')', IARG, 0)
      GO TO 800
*
*  ** Restart child task
*
  800 NWDBP3 = NWDBP3+1
      IWDBP3(NWDBP3) = IQDBP3
      CALL APCONT (IDDBP3, IRET, NWDBP3, IWDBP3)
      IF (IQDBP3.NE.0.OR.IRET.NE.0)     GO TO 999
*
      GO TO (999,250,350,450,450,650,750), ICODE
*
*  ** Errors
*
  900 CALL CDPRNT (6, '(/,'' DBHOST : ZEBRA system not initialized'')',
     +     IARG, 0)
      GO TO 990
  902 CALL CDPRNT (IQPRNT, '(/,'' DBHOST : no ZEBRA store was initial'//
     +     'ized'')', IARG, 0)
      GO TO 990
  910 CALL CDPRNT (IQPRNT, '(/,'' DBHOST : Unknown request '//RNDBP3//
     +     ''')', IARG, 0)
      GO TO 990
  920 CALL CDPRNT (IQPRNT, '(/,''  DBHOST : not enough message words '//
     +     'received ('',I2,'') -- 8 expected'')', NWDBP3, 1)
      GO TO 990
  930 CALL CDPRNT (IQPRNT, '(/,'' DBHOST : DBL3 not initialized yet'')',
     +     IARG, 0)
*
  990 IQDBP3 = -1
      NWDBP3 = 0
      GO TO 800
*
  999 CALL APTEND (INDXT,'DBHOST',IDTASK)
*                                                             END DBHOST
      END
#endif
#endif
