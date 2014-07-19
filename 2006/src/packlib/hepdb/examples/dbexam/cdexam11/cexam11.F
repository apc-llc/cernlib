*
* $Id: cexam11.F,v 1.1.1.1 1996/02/28 16:23:54 mclareni Exp $
*
* $Log: cexam11.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:54  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM11
*
************************************************************************
*
*        PROG. CEXAM11
*
*   Provides facility to enter information into data base from an input
*   file. It operates with 2 FFKEY data cards 'IOPA' to specify the
*   input and output (data base) files and 'DIRE' to specify the name
*   of the directory and when needed the specification of user keys.
*
*   External routines : CDCRBK, CDLOKF, HEPDB routines
*   External libraries : PACKLIB, KERNLIB
*
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /GCBANK/ NZEBRA, GVERSN, ZVERSN, IXSTOR, IXDIV, IXCONS
     +              , FENDQ(16), LMAIN, LR1, WS(NLAST)
      DIMENSION       IQ(2), Q(2), LQ(8000)
      EQUIVALENCE     (Q(1),IQ(1),LQ(9)), (LQ(1),LMAIN,LBKS)
*
      COMMON /QUEST/  IQUEST(100)
*
      COMMON /CUSERB/ LKBKCU, LLBKCU
*
      PARAMETER       (NBUFF=50, NBDIR=20, NBKEY=20)
      DIMENSION       LBUFF(NBUFF), LBDIR(NBDIR), LBKEY(NBKEY)
      CHARACTER       CHINP*4, CHDBL*4, CHOPT*4, TOPN*16, CHDIR*80
      CHARACTER       CHNAM*80, CHFOR*100, CTAG(100)*8, CHVL*4
      PARAMETER       (NOFF1=10, NOFF2=12, IDHINS=4)
      INTEGER         KEYS(100)
*
*     ------------------------------------------------------------------
*
* *** Initialize ZEBRA
*
      CALL MZEBRA (-1)
      CALL MZSTOR (IXSTOR, '/GCBANK/', ' ', FENDQ(1), LQ(1), LQ(1)
     +           , LQ(1), LQ(NDIV1), LQ(NLAST))
      CALL MZDIV  (IXSTOR, IXCONS, 'Constant', 10000, NLAST-5000, 'L')
      IXDIV  = IXSTOR + 2
      CALL MZLINK (IXSTOR, '/CUSERB/', LKBKCU, LLBKCU, LLBKCU)
*
* *** Initialize FFREAD
*
      LPRNT  = 6
      CALL FFINIT (0)
      LUNIN  = 0
      LUNDB  = 0
      CALL VBLANK (LBUFF, NBUFF)
      CALL VBLANK (LBDIR, NBDIR)
      CALL VBLANK (LBKEY, NBKEY)
      CALL FFKEY  ('IOPA', LBUFF, NBUFF, 'MIXED')
      CALL FFKEY  ('DIRE', LBDIR, NBDIR, 'MIXED')
      CALL FFKEY  ('KYCH', LBKEY, NBKEY, 'MIXED')
      CALL FFGO
*
* *** Find the name of the directory and hence Top dir name
*
      CALL UHTOC (LBDIR(1), 4, CHNAM, 80)
      IF (CHNAM(1:2).NE.'//') THEN
        WRITE (LPRNT, *) ' CEXAM11 : No valid directory name given'
        GO TO 999
      ENDIF
      LNB     = INDEX (CHNAM, ' ')
      IF (LNB.GT.1) THEN
        CHDIR  = CHNAM(1:LNB-1)
      ELSE
        CHDIR  = CHNAM
      ENDIF
      NCHAR  = LENOCC (CHDIR)
      NCHRT  = INDEX  (CHDIR(3:NCHAR), '/') - 1
      IF (NCHRT.LE.0) NCHRT = NCHAR - 2
      TOPN   = CHDIR(3:NCHRT+2)
*
* *** Open the i/p and o/p files
*
      CALL CDLOKF ('INPU', LBUFF, NBUFF, I)
      IF (I.GT.0) THEN
        I      = I + 1
        LUNIN  = IABS (LBUFF(I))
        IF (LBUFF(I).LT.0) I = I + 2
        CALL UHTOC (LBUFF(I+1), 4, CHFOR, 80)
        LNB    = INDEX (CHFOR, ' ')
        CHNAM  =  CHFOR(2:LNB)
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
        CALL CUTOL (CHNAM)
#endif
        CALL CDOPFL (LUNIN,  CHNAM, 'OLD', IRC)
        IF (IRC.NE.0) LUNIN = 0
      ENDIF
*
      CALL CDLOKF ('DBL3', LBUFF, NBUFF, I)
      IF (I.GT.0) THEN
        I      = I + 1
        LUNDB  = IABS (LBUFF(I))
        IF (LBUFF(I).LT.0) THEN
          I      =  I + 1
          CALL UHTOC (LBUFF(I), 4, CHDBL, 4)
          I      =  I + 1
          LRCDB  = LBUFF(I)
          I      =  I + 1
          LXTDB  = LBUFF(I)
          I      =  I + 1
          CALL UHTOC (LBUFF(I), 4, CHOPT, 4)
        ELSE
          CHDBL  = 'RO  '
          LRCDB  = 4096
          LXTDB  = 4096
          CHOPT  = 'U   '
        ENDIF
        CALL UHTOC (LBUFF(I+1), 4, CHFOR, 80)
        LNB    = INDEX (CHFOR, ' ')
        CHNAM  =  CHFOR(2:LNB)
        CALL CDROPN (LUNDB, CHDBL, CHNAM, LRCDB, IRC)
        IF (IRC.NE.0) LUNDB = 0
      ENDIF
*
      IF (LUNIN.EQ.0.OR.LUNDB.EQ.0) THEN
        WRITE (LPRNT, *) ' CEXAM11 : Error in opening the files'
        GO TO 999
      ENDIF
*
* *** Initialize the Database
*
      NPAIR = 1
      LUFZ  = 0
      NTOP  = 0
      CALL CDINIT (IDIVCU, LUNDB, LUFZ, TOPN, NPAIR, LXTDB, NTOP,
     +             CHOPT, IRC)
      CALL CDLOGL (TOPN,  1, ' ', IRC)
      CALL UOPTC  (CHOPT, 'Z', II)
*
* *** See if directory creation is necessary
*
      IF (II.GT.0) THEN
        ICREA   = 1
      ELSE
        CALL RZCDIR (CHDIR, ' ')
        IF (IQUEST(1).EQ.0) THEN
          ICREA  = 0
          NWKEY  = IQUEST(8)
        ELSE
          ICREA  = 1
        ENDIF
      ENDIF
*
* *** Find the number of user keys and create directory
*
      IF (ICREA.GT.0) THEN
        NUKYS   = 0
        CHFOR   = ' '
        CTAG(1) = ' '
        CALL CDLOKF ('KEYS', LBDIR, NBDIR, I)
   10   IF (I.GT.0 .AND. I.LE.NBDIR-3) THEN
          I      = I + 1
          CALL UHTOC (LBDIR(I), 4, CTAG(NUKYS+1), 8)
          IF (CTAG(NUKYS+1).NE.'        ') THEN
            I      = I + 2
            CALL UHTOC (LBDIR(I), 4, CHVL, 4)
            IF (CHVL(1:1).NE.'I'.AND.CHVL(1:1).NE.'B'.AND.
     +          CHVL(1:1).NE.'H'.AND.CHVL(1:1).NE.'A') CHVL(1:1) = 'I'
            IF (NUKYS.GT.0) THEN
              CHFOR  = CHFOR(1:NUKYS)//CHVL(1:1)
            ELSE
              CHFOR  = CHVL(1:1)
            ENDIF
            NUKYS  = NUKYS + 1
            GO TO 10
          ENDIF
        ENDIF
        IPREC  = 0
        DELTA  = 0.
        CALL CDMDIR (CHDIR, NUKYS, CHFOR, CTAG, 0, IPREC,DELTA, 'C',IRC)
        NWKEY  = NUKYS + NOFF2
        IF (IQUEST(1).NE.0) THEN
          WRITE (LPRNT, *) ' CEXAM11 : Error ', IQUEST(1),
     +                      ' in creating directory'
          GO TO 50
        ENDIF
      ENDIF
*
* *** Now call user routine to prepare the data base object
*
      CALL VZERO  (KEYS, NWKEY)
      CALL CDCRBK (LUNIN, LPRNT, KEYS)
*
* *** Update some of the keys
*
      IK    = 1
   20 IF (IK.LT.NBKEY) THEN
        IF (LBKEY(IK).GT.0.AND.LBKEY(IK).LE.NWKEY) THEN
          IKEY   = LBKEY(IK)
          IF (IKEY.EQ.NOFF1+1.OR.IKEY.EQ.NOFF1+2) THEN
            CALL DBPKTS (LBKEY(IK+1), LBKEY(IK+2), KEYS(IKEY), IRC)
            IK     = IK + 3
          ELSE IF (IKEY.EQ.IDHINS) THEN
            CALL DBPKTM (LBKEY(IK+1), LBKEY(IK+2), KEYS(IKEY))
            IK     = IK + 3
          ELSE
            KEYS(IKEY) = LBKEY(IK+1)
            IK     = IK + 2
          ENDIF
          GO TO 20
        ENDIF
      ENDIF
*
* *** Enter it in Data base
*
      CALL CDSTOR (CHDIR, LKBKCU, LLBKCU, IXDIV, KEYS, 'DY', IRC)
      WRITE (LPRNT, *) ' CEXAM11 : Data entered in ', CHDIR(1:NCHAR)
      WRITE (LPRNT, *) '           Error code      ', IRC
*
* *** Close data base and exit
*
   50 CALL CDEND  ('*', 'A', IRC)
      CALL MZEND
*                                                            END CEXAM11
  999 END
