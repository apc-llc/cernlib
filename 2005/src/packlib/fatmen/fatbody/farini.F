*
* $Id: farini.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: farini.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FARINI (LUNRZ, PATH, NWKEY, CHFOR, CHTAG,
     +                   NRECS, CHOPT)
*
************************************************************************
*
*        SUBR. FARINI (LUNRZ, PATH, NWKEY, CHFOR, CHTAG,
*                      NRECS, CHOPT)
*
*   Initializes of the Random Access Event file (for each path)
*
*   Arguments :
*
*     LUNRZ    RZ file unit
*     PATH     Complete path name
*     NWKEY    Number of words associated to the keys at the lowest
*              level (If some directory in the pathname at a higher
*              level does not exist, it is created with 1 dummy key)
*     CHFOR    Character variable describing each element of the key
*              vector at the lowest level
*     CHTAG    Character array defined as CHARACTER*8 (NWKEY)
*     NRECS    Number of records for primary allocation (for RZMAKE)
*              If less than or equal to 0 use existing RZ file (RZFILE)
*     CHOPT    Character Option : Same as in RZFILE / RZMAKE
*          Z   Random access file does not exist (use RZMAKE)
*
*   Called by User    ( Not exactly )
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = -1 : Invalid Path name
*               = -2 : The file is already open with correct LUNRZ and
*                      PATHN
*               = -3 : The file is already open with wrong LUNRZ or
*                      PATHN
*
************************************************************************
*
      PARAMETER       (MNSYS = 10000, MXSYS = 100000)
*
*             ZEBRA system common blocks
*
#include "zebra/mzcb.inc"
      COMMON /ZEBQ/   IQFENC(4), LQZEB(100)
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
      PARAMETER       (NOPRZ=8)
      CHARACTER       CHOPT*(*), PATH*(*), CHFOR*(*), TOP*16, TOPN*16
      CHARACTER       COPRZ(NOPRZ)*1, CHOP*8, PATHX*255, CHOPE*4
      CHARACTER       FNAME*80
      CHARACTER*(*)   CHTAG(*)
      CHARACTER*6     CHFREE
#if defined(CERNLIB_VAXVMS)
      CHARACTER*16    RECTYP
#endif
#if defined(CERNLIB_CSPACK)
      EXTERNAL        FMXZIO
#endif
      INTEGER         IOPRZ(NOPRZ)
      DIMENSION       IBUF(1024)
      SAVE            NTOPM
*
      DATA            NTOPM /0/
      DATA            COPRZ /'1', 'D', 'L', 'M', 'S', 'U', 'X', 'C'/
*
*     ------------------------------------------------------------------
*
*     CALL UOPTC (CHOPT, 'Z', IOPTZ)
      IOPTZ  = INDEX(CHOPT,'Z')
      IOPTO  = 0
      IF (IOPTZ.NE.0.AND.NRECS.GT.0) THEN
         NREC = NRECS
         IOPTN = 1
      ELSE
         IOPTN = 0
         NREC = 0
      ENDIF
      DO 10 I = 1, NOPRZ
         IF (I.EQ.1) THEN
            CHOP = COPRZ(I)
         ELSE
            CHOP = CHOP(1:I-1)//COPRZ(I)
         ENDIF
   10 CONTINUE
      CALL UOPTC (CHOPT, CHOP(1:NOPRZ), IOPRZ)
*     IF (IOPRZ(1).NE.0.OR.IOPRZ(6).NE.0) IOPTO = 1
      IF (IOPRZ(6).NE.0) IOPTO = 1
*
* *** Find number of characters in the path name
*
      MAX    = LENOCC (PATH)
      IF (MAX.LT.3.OR.PATH(1:2).NE.'//') THEN
         IQUEST(1) = -1
         IF(IDEBFA.GE.-3) WRITE (LPRTFA, 9003) PATH
         GO TO 999
      ENDIF
      II     = INDEX (PATH(3:MAX), '/') - 1
      IF (II.LE.0) THEN
         II = MAX
      ELSE
         II = II + 2
      ENDIF
      IF (II.EQ.MAX) THEN
         ITOP = 1
      ELSE
         ITOP = 0
      ENDIF
      TOPN   = PATH(3:II)
*
* *** First pass initialization
*
      IF (NTOPM.EQ.0) THEN
      ENDIF
*
* *** Construct the character option
*
      NTOPM  = NTOPM + 1
      CHOP   = ' '
      II     = 0
      DO 20 I = 1, NOPRZ
         IF (IOPRZ(I).NE.0) THEN
            IF (II.EQ.0) THEN
               CHOP = COPRZ(I)
            ELSE
               CHOP = CHOP(1:II)//COPRZ(I)
            ENDIF
            II = II + 1
         ENDIF
   20 CONTINUE
      IFLG   = 0
      IF (IOPTO.NE.0.OR.IOPTN.NE.0) IFLG = 1
*
*  ** Check whether the RZ file has been already initialized
*
      IROPN  = 0
      IF (LTOPFA.NE.0) THEN
         LSAVFA = LTOPFA
   30    LUN = IQ(KOFUFA+LSAVFA+MLUNFA)
         NCH = IQ(KOFUFA+LSAVFA+MNCHFA)
         CALL UHTOC (IQ(KOFUFA+LSAVFA+MCHRFA), 4, PATHX, NCH)
         PATHX = PATHX(1:NCH)
         IF (PATHX.EQ.PATH.AND.LUN.EQ.LUNRZ) THEN
            IQUEST(1) = -2
            LSAVFA = 0
            IF(IDEBFA.GE.-3) WRITE (LPRTFA, 9004) PATH(1:MAX), LUNRZ
            GO TO 999
         ENDIF
*
         II = INDEX (PATHX(3:NCH), '/') - 1
         IF (II.LE.0) THEN
            II = NCH
         ELSE
            II = II + 2
         ENDIF
         TOP = PATHX(3:II)
         IF (TOPN.NE.TOP.AND.LUN.EQ.LUNRZ) THEN
            IQUEST(1) = -3
            LSAVFA = 0
            WRITE (LPRTFA, 9005) PATHX(1:NCH), LUN, PATH(1:MAX), LUNRZ
            GO TO 999
         ELSE IF (TOPN.EQ.TOP.AND.LUN.NE.LUNRZ) THEN
            IQUEST(1) = -3
            LSAVFA = 0
            WRITE (LPRTFA, 9005) PATHX(1:NCH), LUN, PATH(1:MAX), LUNRZ
            GO TO 999
         ENDIF
         IF (LUN.EQ.LUNRZ) THEN
            IROPN = 1
            IFLG = IQ(KOFUFA+LSAVFA+MFLGFA)
         ENDIF
*
         LSUP = LSAVFA
         LSAVFA = LQ(KOFUFA+LSUP)
         IF (LSAVFA.NE.0) GO TO 30
*
*  **   Create linear structure of the top-directories
*
         NS = NSLUFA + 10
         CALL MZBOOK (IDIVFA, LSAVFA, LSUP, 0, 'UPFA', NS, NS, NDUPFA,
     +   IOUPFA, 0)
      ELSE
         NS = NSLUFA + 10
         CALL MZBOOK (IDIVFA, LTOPFA, LTOPFA, 1, 'UPFA', NS, NS,
     +   NDUPFA, IOUPFA, 0)
         LSAVFA = LTOPFA
      ENDIF
*
**    Create the book-keeping bank
*
      CALL MZBOOK (IDIVFA, LBBKFA, LSAVFA, -KLBKFA, 'BKFA', 0, 0,
     +10*NWBKFA, IOBKFA, 0)
*
**    Create the "generic names bank"
*
      CALL MZBOOK (IDIVFA, LBGNFA, LSAVFA, -KLGNFA, 'GNFA', 0, 0,
     +800, 5, 0)
*
*  ** Fill up the top directory bank with user data
*
      NTOPFA = NTOPFA + 1
      IQ(KOFUFA+LSAVFA+MLUNFA) = LUNRZ
      IQ(KOFUFA+LSAVFA+MFLGFA) = IFLG
      IQ(KOFUFA+LSAVFA+MTOPFA) = NTOPFA
      IQ(KOFUFA+LSAVFA+MNCHFA) = MAX
      CALL UCTOH (PATH, IQ(KOFUFA+LSAVFA+MCHRFA), 4, MAX)
      IQ(KOFUFA+LBBKFA-5) = 0
      IQ(KOFUFA+LBGNFA-5) = 0
*
      IQUEST(1) = 0
#if !defined(CERNLIB_CZ)
      IF (IROPN.NE.0)    GO TO 70
*
*  **    Open the file
*
      FNAME = TOPN(1:LENOCC(TOPN))//'.FATRZ'
      LDEF  = LENOCC(DEFAULT)
#endif
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         LRECL = 0
         CHOP  = 'X'
         CALL FAOPEN('FARZ',LUNRZ,CHOPE,FNAME,LRECL,5000)
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9001) LUNRZ,TOPN,LRECL,CHOP
 9001 FORMAT(' FARINI. call RZHOOK for LUN ',I3,' TOPN ',A,
     +       ' LRECL ',I6,' CHOP ',A)
         CALL RZHOOK (LUNRZ, TOPN, FMXZIO, LRECL, CHOP)
         GOTO 999
      ENDIF
#endif
#if (defined(CERNLIB_FATSRV))&&(!defined(CERNLIB_CZ))
      FNAME = TOPN(1:LENOCC(TOPN))//'.FATRZ.A6'
#endif
#if (defined(CERNLIB_FMOTOZ))&&(!defined(CERNLIB_CZ))
      FNAME = TOPN(1:LENOCC(TOPN))//'.FATRZ.A6'
#endif
#if !defined(CERNLIB_CZ)
*
*     New CHOPT for use with RZOPEN
*
#endif
#if defined(CERNLIB_VAXVMS)
      CHOPE = 'S'
      IF (INDEX(CHOPT,'1').NE.0) CHOPE = 'SU'
*
*     Check if file is STREAM_LF
*     If so, assume exchange mode, C I/O, shared access
*
      INQUIRE(FILE=DEFAULT(1:LDEF)//FNAME,RECORDTYPE=RECTYP)
      IF(INDEX(RECTYP,'STREAM_LF').NE.0) THEN
         CHOPE = 'SXC'
         CHOP  = 'SXC'
         STRMLF = .TRUE.
      ELSE
         STRMLF = .FALSE.
      ENDIF
#endif
#if !defined(CERNLIB_VAXVMS)
      CHOPE = ' '
      IF (INDEX(CHOPT,'1').NE.0) CHOPE = 'U'
#endif
#if defined(CERNLIB_UNIX)
      CHOPE = 'X'
      CHOP  = 'X'
      IF (INDEX(CHOPT,'1').NE.0) THEN
         CHOPE = 'XU'
         CHOP  = 'XU'
      ENDIF
* Nasty hack for fatcat:/fatmen
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_DECS))&&(defined(CERNLIB_CERN))
*
*     Assume that files in /fatmen are exchange mode, C I/O
*
      IF(DEFAULT(1:7).EQ.'/fatmen') THEN
         CHOPE = 'XC'
         CHOP  = 'XC'
         IF (INDEX(CHOPT,'1').NE.0) THEN
            CHOPE = 'XUC'
            CHOP  = 'XUC'
         ENDIF
      ENDIF
#endif
*
*     New files...
*
      IF (INDEX(CHOPT,'Z').NE.0) THEN
         CHOPE = 'UN'
         IF(INDEX(CHOPT,'C').NE.0.AND.INDEX(CHOPT,'X').NE.0)
     +      CHOPE = 'CUNX'
         IF(INDEX(CHOPT,'C').EQ.0.AND.INDEX(CHOPT,'X').NE.0)
     +      CHOPE = 'UNX'
      ENDIF
 
      LRECL = 4096
      CALL FAOPEN('FARZ',LUNRZ,CHOPE,FNAME,LRECL,5000)
*
      IF (IOPTN.NE.0) THEN
*
*  **   Create file on mass storage and create the 1st pass directories
*
#if defined(CERNLIB_IBMVM)
*
*       Get number of free blocks on A disk
*
         NPRE = 2500
         CALL VMREXX('F','FREE_BLOCKS',CHFREE,IRC)
         IF (IRC .NE. 0) THEN
            PRINT *, 'FARINI. Error ',IRC,' obtaining number of free '
     +      //'blocks'
            PRINT *, '       Ensure that REXX variable FREE_BLOCKS is '
     +      //'set'
            PRINT *, '       in exec which calls this program'
            PRINT *, '       (FREE_BLOCKS=QDISK("191","BLKLEFT")'
         ELSE
            READ(CHFREE,*) NPRE
            NPRE = (NPRE*85)/100
         ENDIF
 
         PRINT *,'FARINI. Preformatting ',NPRE,' records on unit ',
     +   LUNRZ
 
         DO 40 IREC=1,NPRE
            WRITE(UNIT=LUNRZ,REC=IREC,ERR=50)IBUF
   40    CONTINUE
         GOTO 60
   50    PRINT *,'FARINI - Error at record ',IREC, ' LUN=',LUNRZ
   60    CONTINUE
#endif
         IF (NWKEY.GT.0.AND.ITOP.NE.0) THEN
*
*    New 'feature' of RZ calls ZFATAL for NREC > max
*
            NREC = MIN(65000,NREC)
            CALL RZMAKE (LUNRZ, TOPN, NWKEY, CHFOR, CHTAG, NREC, CHOP)
         ELSE
            CHTAG(1) = 'DUMMY   '
            CALL RZMAKE (LUNRZ, TOPN, 1, 'I', CHTAG, NREC, CHOP)
         ENDIF
*
      ELSE
*
*  **   Reopen the RZ-file on the mass storage for read and write
*
         IF(IFMODX.NE.0) THEN
            LCHOP = LENOCC(CHOP)
            IF(LCHOP.EQ.0) THEN
               LCHOP = 1
               CHOP  = 'X'
            ELSE
               IF(INDEX(CHOP(1:LCHOP),'X').EQ.0)
     +            CHOP(LCHOP+1:LCHOP+1) = 'X'
            ENDIF
         ENDIF
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9002) LUNRZ,TOPN,LRECL,CHOP
 9002 FORMAT(' FARINI. call RZFILE for LUN: ',I3,' TOPN: ',A,
     +       ' LRECL ',I6,' CHOP ',A)
         CALL RZFILE (LUNRZ, TOPN, CHOP)
         IF(IQUEST(1).NE.0) THEN
            PRINT *, 'FARINI. Error ',IQUEST(1),' from RZFILE, will '
     +      //'reinitialise'
            CALL RZEND(TOPN)
            CALL RZFILE(LUNRZ,TOPN,CHOP)
         ENDIF
*
      ENDIF
*
      IF (IQUEST(1).NE.0)  THEN
         IQUEST(1) = -2
         WRITE (LPRTFA, 9004) PATH, LUNRZ
         GO TO 999
      ENDIF
*
* *** Create the directory in up-date mode (if not yet created)
*
   70 IF (IFLG.NE.0.AND.NWKEY.GT.0) THEN
*       CALL FATMDI (PATH, NWKEY, CHFOR, CHTAG)
         IF (IQUEST(1).EQ.25) IQUEST(1) = 0
      ENDIF
*
 9003 FORMAT (/,' FARINI : Illegal Path Name ',A)
 9004 FORMAT (/,' FARINI : Path name ',A,' is already open on unit ',I4)
 9005 FORMAT (/,' FARINI : Path name ',A,' open on unit ',I4
     +,' is in conflict with'/,' Requested Path name ',A,' on unit ',I4)
*                                                             END FARINI
  999 END
