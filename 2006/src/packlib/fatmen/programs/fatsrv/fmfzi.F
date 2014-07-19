*
* $Id: fmfzi.F,v 1.1.1.1 1996/03/07 15:17:40 mclareni Exp $
*
* $Log: fmfzi.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:40  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFZI(FILEN,GENEN,ICOUNT)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fausto.inc"
#include "fatmen/fatstat.inc"
      CHARACTER*(*)  FILEN
      COMMON/FSRVCM/ HNAME,HTYPE,HSYS
      CHARACTER*8    HNAME,HTYPE,HSYS
      CHARACTER*80   FILEDEF,LABELDEF,SETUP
      CHARACTER*4    COMAND
      CHARACTER*240  GENEN
      CHARACTER*20   FNAME
      CHARACTER*255  CHDSN
      CHARACTER*8    CHHST
      CHARACTER*8    CHVSN,CHVID
      CHARACTER*1    CHOPT
      CHARACTER*8    DELTIM
      CHARACTER*8    CHUSER,CHNODE
      CHARACTER*80   CHLINE
      CHARACTER*20   CHFSYS,CHFGRP
      PARAMETER      (MHEAD=400)
      INTEGER*4      IUHEAD(MHEAD)
#include "fatmen/fmaxcop.inc"
#include "fatmen/fatout.inc"
      PARAMETER       (LKEYFA=10)
      CHARACTER*8  CHTAG(LKEYFA)
      CHARACTER*10 CHFOR
      DIMENSION KEYS(LKEYFA),KEYZ(LKEYFA)
      DIMENSION KEYSOU(LKEYFA,MAXCOP),KEYSIN(LKEYFA)
      DIMENSION IFORM(10)
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','medium','loc.code','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
      OUTPUT = 'TTY'
      LOUT   = 3
      LWRITE = LPRTFA
      LMONIT = LPRTFA
*
*     Count number of files
*
      NFMFIL = NFMFIL + 1
#if defined(CERNLIB_IBMVM)
*
*     Try to read FZ file just received in RDR
*
      WRITE(FILEDEF,9001) LUFZFA
 9001 FORMAT('FILEDEF ',I3,' DISK FATMEN RDRFILE')
      CALL VMCMS(FILEDEF,IC)
 
      IF (IC .NE. 0) THEN
         PRINT *,'FMFZI. return code ',IC,' from VMCMS'
         STOP
      ENDIF
*
*     Get user and node name of update
*
      CALL VMCMS('GLOBALV SELECT *EXEC STACK FATADDR',IC)
      CALL VMRTRM(CHLINE,IEND)
      ISTART = ICFNBL(CHLINE,1,IEND)
      CALL FMWORD(CHUSER,0,' ',CHLINE(ISTART:IEND),IC)
      CALL FMWORD(CHNODE,1,' ',CHLINE(ISTART:IEND),IC)
#endif
      CALL FZFILE(LUFZFA,0,'AI')
      CALL FZLOGL(LUFZFA,-2)
#if defined(CERNLIB_IBMVM)
      OPEN(LUFZFA,STATUS='OLD',ERR=180)
#endif
#if !defined(CERNLIB_IBM)
      LENF = LENOCC(FILEN)
      OPEN(LUFZFA,STATUS='OLD',ERR=180,FILE=FILEN(1:LENF), FORM=
     +'FORMATTED')
      IF(IDEBFA.GE.0) PRINT *,'FMFZI. processing ',FILEN(1:LENF)
#endif
#if defined(CERNLIB_UNIX)
      LSTA   = INDEXB(FILEN(1:LENF),'/') + 1
#endif
#if defined(CERNLIB_VAXVMS)
      LSTA   = INDEXB(FILEN(1:LENF),']') + 1
#endif
#if !defined(CERNLIB_IBM)
*
*     Old or new style journal file name?
*
      LDOT   = INDEX(FILEN(LSTA:LENF),'.')   + LSTA - 1
      LUSC   = INDEX(FILEN(LSTA:LDOT-1),'_')
      IF(LUSC.GT.0) THEN
         LUSC   = LUSC + LSTA - 1
         CHUSER = FILEN(LSTA:LUSC-1)
         CHNODE = FILEN(LUSC+1:LDOT-1)
         ISTYLE = 0
      ELSE
         LUSC   = INDEX(FILEN(LSTA:LENF),'_') + LSTA - 1
         CHUSER = FILEN(LDOT+1:LUSC-1)
         CHNODE = FILEN(LUSC+1:LENF)
         ISTYLE = 1
      ENDIF
#endif
#if defined(CERNLIB_IBMMVS)
      LENF = LENOCC(FILEN)
      OPEN(LUFZFA,STATUS='OLD',ERR=180,FILE='/'//FILEN(1:LENF), FORM=
     +'FORMATTED')
      LDOT   = INDEX(FILEN(1:LENF),'.TODO.') + 5
      JDOT   = INDEX(FILEN(LDOT+1:LENF),'.') + LDOT
      KDOT   = INDEX(FILEN(JDOT+1:LENF),'.') + JDOT
      CHUSER = FILEN(LDOT+1:JDOT-1)
      CHNODE = FILEN(JDOT+1:KDOT-1)
#endif
*
*     Loop on FZ file - may contain multiple updates
*
      IEOR   = 0
      ICOUNT = 0
   10 CONTINUE
      LHEAD  = MHEAD
      CHOPT  = ' '
      CALL FZIN(LUFZFA,IDIVFA,LNEWFA,2,CHOPT,LHEAD,IUHEAD)
*
*     Have we seen an end of run record?
*
      IF(IQUEST(1).EQ.2) IEOR = 1
      IF(IQUEST(1).GE.5) GOTO 160
      IF(IQUEST(1).NE.0) GOTO 10
*
*     Start timer
*
      CALL FMRTIM(DELTIM)
      CALL TIMED(T)
*
*     Count commands
*
      NFMCOM = NFMCOM + 1
      ICOUNT = ICOUNT + 1
 
      KEYOLD = 0
*
*     Decode IUHEAD
*
      CALL UHTOC(IUHEAD(1),4,COMAND,4)
 
      CALL DATIME(ID,IT)
 
      IF(IDEBFA.GE.-1) THEN
      WRITE (LPRTFA,9002) ID,IT
 9002 FORMAT(1X,'===============================================',
     +       1X,I6,1X,I6,1X,
     +          '===============================================')
      IF(COMAND.EQ.'LOG') THEN
         WRITE (LPRTFA,9003) COMAND,ICOUNT,NFMCOM
 9003 FORMAT(' FMFZI. ',A,' (update # ',I6,' total ',I6,')')
*
*     Unpack hollerith
*
         CALL UHTOC(IUHEAD(KFMSYS),4,CHFSYS,20)
         CALL UHTOC(IUHEAD(KFMGRP),4,CHFGRP,20)
         GENEN = '//'//CHFSYS(1:LENOCC(CHFSYS))//'/'//
     +                 CHFGRP(1:LENOCC(CHFGRP))//'/FATLOG'
 
      ELSE
         CALL UHTOC(IUHEAD(2),4,GENEN,240)
         WRITE (LPRTFA,9004) COMAND,GENEN(1:LENOCC(GENEN)),ICOUNT,NFMCOM
 9004 FORMAT(' FMFZI. ',A,1X,A,' (update # ',I6,' total ',I6,' )')
      ENDIF
      ENDIF
      IF(IDEBFA.GE.0) WRITE (LPRTFA,*) 'FMFZI. ',CHUSER,' @ ',CHNODE
 
      IF(COMAND.NE.'LOG') THEN
      CALL UCOPY(IUHEAD(71),KEYS(1),10)
*
*     Monitoring information
*
      IF(LHEAD.GT.80) THEN
         CHDSN = ' '
         WRITE(LMONIT,9005) IUHEAD(81),IUHEAD(82)
         CALL UHTOC(IUHEAD(91),4,CHDSN,255)
         WRITE(LMONIT,9006) CHDSN(1:LENOCC(CHDSN))
         NFMOPN = NFMOPN + 1
      ENDIF
 9005 FORMAT(' IHOWFA: ',Z8,' ITIMFA: ',I6)
 9006 FORMAT(' CHFNFA: ',A)
*
*     Ensure the keys correctly reflect dataset name
*     (Problems with FZ files spooled from VAX)
*
* ***  Find file name
*
      NCH = LENOCC(GENEN)
      ICH = INDEXB(GENEN(1:NCH),'/')
      FNAME = GENEN(ICH+1:NCH)
      CALL VBLANK(KEYS(2),5)
      CALL UCTOH(FNAME,KEYS(2),4,MIN(NCH-ICH,20))
*
*     Update keys vector from bank...
*
      IF(INDEX(COMAND(2:4),'DIR').EQ.0) THEN
         KEYS(MKCLFA) = IQ(LNEWFA+MCPLFA+KOFUFA)
         KEYS(MKLCFA) = IQ(LNEWFA+MLOCFA+KOFUFA)
         KEYS(MKMTFA) = IQ(LNEWFA+MMTPFA+KOFUFA)
         IF(IDEBFA.GE.-1) CALL FMPKEY(KEYS,LKEYFA)
*
*     Display dsn & host (disk files), vsn & vid (tape files)
*
         IF(LHEAD.GT.80.AND.IDEBFA.GE.0) THEN
            IF(KEYS(MKMTFA).EQ.1) THEN
               CHDSN = ' '
               CALL UHTOC(IQ(LNEWFA+MFQNFA+KOFUFA),4,CHDSN,NFQNFA)
               CALL UHTOC(IQ(LNEWFA+MHSNFA+KOFUFA),4,CHHST,NHSNFA)
               LDSN = LENOCC(CHDSN)
               LHST = LENOCC(CHHST)
               WRITE(LMONIT,9007) CHDSN(1:LDSN),CHHST(1:LHST)
 9007  FORMAT(' FMFZI. dsn = ',A,' host = ',A)
            ELSE
               CALL UHTOC(IQ(LNEWFA+MVSNFA+KOFUFA),4,CHVSN,NVSNFA)
               CALL UHTOC(IQ(LNEWFA+MVIDFA+KOFUFA),4,CHVID,NVIDFA)
               WRITE(LMONIT,9008) CHVSN,CHVID,IQ(LNEWFA+MFSQFA+KOFUFA)
 9008  FORMAT(' FMFZI. vsn = ',A,' vid = ',A,' fseq = ',I6)
            ENDIF
         ENDIF
      ENDIF
*
*     Display bank (for debug...)
*
      IF((IDEBFA.GE.3).AND.
     +   (INDEX(COMAND(2:4),'DIR').EQ.0)) THEN
         PRINT *,'FMFZI. new bank...'
         CALL FMSHOW(GENEN(1:NCH),LNEWFA,KEYS,'A',IC)
      ENDIF
 
      ENDIF
*
*        Get unit for RZ file
*
      LSAVFA = LTOPFA
      LUNRZ = IQ(LSAVFA+KOFUFA+MLUNFA)
*
*     Now process command
*
      IF (COMAND .EQ. 'MDIR') THEN
         CALL FATMDI(GENEN,LKEYFA,CHFOR,CHTAG)
         NFMMDR = NFMMDR + 1
 
      ELSEIF(COMAND(1:3) .EQ. 'DEL') THEN
         NFMDEL = NFMDEL + 1
         LEND = INDEXB(GENEN,'/') - 1
         CALL RZCDIR (GENEN(1:LEND), ' ')
         IF(IQUEST(1).NE.0) THEN
            WRITE(LPRTFA,*) 'FMFZI. cannot set directory to ',
     +         GENEN(1:LEND)
            GOTO 10
         ENDIF
*
*     Find KEYS(1) from RZ file by matching this bank against all
*     candidates in this directory.
*     A candidate matches when -
*        Disk files: hostname & file-id match.
*        Tape files: vsn, vid and file-seq match.
*
*
         IDEL    = 0
         KEYS(1) = 0
*
*     Update keys vector from bank...
*
         KEYS(MKCLFA) = IQ(LNEWFA+MCPLFA+KOFUFA)
         KEYS(MKLCFA) = IQ(LNEWFA+MLOCFA+KOFUFA)
         KEYS(MKMTFA) = IQ(LNEWFA+MMTPFA+KOFUFA)
         CALL UCOPY(KEYS,KEYSIN,10)
         CALL FMSELK(GENEN,KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
 
         IF(IDEBFA.GE.1) PRINT *,'FMFZI. found ',NMATCH,' matches'
*
*     For each candidate, compare bank contents
*
         DO 20  I=1,NMATCH
            LOLDFA = 0
            IF(IDEBFA.GE.0) THEN
               PRINT *,'FMFZI. candidate # ',I
               CALL FMPKEY(KEYSOU(1,I),LKEYFA)
            ENDIF
            CALL FMCOMP(GENEN,LNEWFA,KEYS,GENEN,LOLDFA,KEYSOU(1,I),IRC)
            CALL MZDROP(IDIVFA,LOLDFA,'L')
            LOLDFA = 0
            IF(IRC.EQ.0) THEN
               CALL UCOPY(KEYSOU(1,I),KEYS(1),10)
*
*     Stop search
*
               IDEL = 1
               GOTO 30
            ELSE
               IF(IDEBFA.GE.1) PRINT *,'FMFZI. comparison with ',
     +            'candidate # ',I,' failed with RC = ',IRC
            ENDIF
 
   20    CONTINUE
   30    CONTINUE
 
         IF(IDEL.EQ.0) THEN
*
*     No match found
*
            IF(IDEBFA.GE.-3) THEN
            PRINT *,'FMFZI. no match found for following entry - ',
     +         'last return code from FMCOMP was ',IRC
            CALL FMPKEY(KEYS,LKEYFA)
            NFMPDL = NFMPDL + 1
            ENDIF
         ELSE
 
            ICYCLE = 9999
            IF(IDEBFA.GE.1) PRINT *,'FMFZI. deleting following entry'
            CALL FMPKEY(KEYS,LKEYFA)
            CALL RZDELK(KEYS,ICYCLE,'C')
            IF(IQUEST(2).NE.0) THEN
               IF(IDEBFA.GE.-3)
     +         PRINT *,'FMFZI. warning from RZDELK - cycles '//
     +            'structure contains previously deleted objects'
               NFMPDK = NFMPDK + 1
               CALL RZFREE('RZFILE')
               LTOP = LENOCC(TOPDIR)
               CALL RZVERI(TOPDIR(1:LTOP),'B')
               CALL RZLOCK('RZFILE')
            ENDIF
 
#if defined(CERNLIB_SQLCOM)
            MODE =-1
            CALL FMZTOR(GENEN,LNEWFA+KOFUFA,MODE,IRC)
            PRINT *,'FMFZI. Return code ',IRC,' from ORACLE/SQL'
*
*     Attempt reconnect in case ORACLE has died
*
            IF((IRC.EQ.-6065).OR.(IRC.EQ.-3114)) THEN
               PRINT *,'FMFZI. attempting reconnect to ORACLE'
               CALL FMLOGI(IRC)
               CALL FMZTOR(GENEN,LNEWFA+KOFUFA,MODE,IRC)
               PRINT *,'FMFZI. Return code ',IRC,' from ORACLE/SQL'
            ENDIF
 
            IF (IRC .NE. 0) THEN
               CALL DZSHOW('FMFZI.',IDIVFA,LADDR,'L',0,0,0,0)
            ENDIF
*           ENDIF
#endif
 
         ENDIF
 
         CALL MZDROP(IDIVFA,LNEWFA,'L')
         LNEWFA = 0
 
      ELSEIF(COMAND(1:4) .EQ. 'DDIR') THEN
         NFMRMD = NFMRMD + 1
*
*        Delete this directory tree
*
         CALL RZCDIR(GENEN(1:ICH-1), ' ')
         IF(IQUEST(1).NE.0) THEN
            PRINT *,'FMFZI. cannot set directory to ',GENEN(1:ICH-1)
            GOTO 10
         ENDIF
         CALL RZDELT(FNAME(1:NCH-ICH))
 
      ELSEIF((COMAND(1:3) .EQ. 'PUT') .OR. (COMAND(1:3) .EQ. 'MOD')
     +  .OR. (COMAND(1:3) .EQ. 'REP')) THEN
*
*     Counters
*
         IF(COMAND(1:3).EQ.'PUT') NFMPUT = NFMPUT + 1
         IF(COMAND(1:3).EQ.'MOD') NFMMOD = NFMMOD + 1
         IF(COMAND(1:3).EQ.'REP') NFMMOD = NFMMOD + 1
*
*
*        Check if new directories have to be made
*
         LEND = INDEXB(GENEN,'/') - 1
         CALL RZCDIR (GENEN(1:LEND), ' ')
         IF (IQUEST(1).NE.0)THEN
            CALL FATMDI(GENEN(1:LEND),LKEYFA,CHFOR,CHTAG)
         ELSE
*
*     Check that file does not already exist. A match is true if
*        Disk files: hostname & file-id match.
*        Tape files: vsn, vid and file-seq match.
*
*
            CALL UCOPY(KEYS,KEYSIN,10)
*
*      Don't compare copy level or location code
*
            KEYSIN(MKCLFA) = -1
            KEYSIN(MKLCFA) = -1
            CALL FMSELK(GENEN,KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
 
            IF(IDEBFA.GE.1) PRINT *,'FMFZI. found ',NMATCH,' matches'
 
            DO 70 IKEY=1,NMATCH
               IF(IDEBFA.GE.3) THEN
                  PRINT *,'FMFZI. checking against following entry'
     +            ,' from catalogue'
                  CALL FMPKEY(KEYSOU(1,IKEY),LKEYFA)
               ENDIF
*
*     KEYSOU contains the key vectors of any files in the CWD
*     which match the new key vector KEYS
*
*     Read in next bank (must put in link area)
*
               ICYCLE = 9999
               JBIAS = 2
               CALL RZIN(IDIVFA,LOLDFA,JBIAS,KEYSOU(1,IKEY),ICYCLE,' ')
               IF(IQUEST(1).NE.0) PRINT *,'FMFZI. Error ',IQUEST(1),
     +         ' from RZIN'
 
               IF(IDEBFA.GE.3) THEN
                  PRINT *,'FMFZI - bank from catalogue'
                  CALL FMSHOW(GENEN(1:NCH),LOLDFA,KEYS,'A',IC)
               ENDIF
 
*
*     Match fields against current bank, depending on media type
*
               IF(KEYS(MKMTFA) .EQ. 1) THEN
*
*     Disk data set, match MFQNFA, MHSNFA
*
                  DO 40 IOFF=MFQNFA+KOFUFA,MCPLFA+KOFUFA-1
                     IF(IQ(LNEWFA+IOFF).NE.IQ(LOLDFA+IOFF)) THEN
                        PRINT 9009,IKEY,IOFF-KOFUFA, IQ(LOLDFA+IOFF),
     +                  IQ(LNEWFA+IOFF)
 9009 FORMAT(' FMFZI. Comparison with # ',I3,' failed at ',I5,1X,
     +       A4,1X,A4,' (old/new)')
                        GOTO 60
                     ENDIF
   40             CONTINUE
 
               ELSE
*
*     Tape data set, match MVSNFA, MVIDFA, MFSQFA
*
                  DO 50 IOFF=MVSNFA+KOFUFA,MVIPFA+KOFUFA-1
                     IF(IQ(LNEWFA+IOFF).NE.IQ(LOLDFA+IOFF)) THEN
                        IF(IDEBFA.GE.1) PRINT 9009,IKEY,IOFF-KOFUFA,
     +                     IQ(LOLDFA+IOFF),IQ(LNEWFA+IOFF)
                        GOTO 60
                     ENDIF
   50             CONTINUE
                  IF(IQ(LNEWFA+MFSQFA+KOFUFA).NE. IQ(LOLDFA+MFSQFA+
     +            KOFUFA)) THEN
                     IF(IDEBFA.GE.1) PRINT 9010,IKEY,IOFF-KOFUFA,
     +                  IQ(LOLDFA+IOFF),IQ(LNEWFA+IOFF)
 9010 FORMAT(' FMFZI. Comparison with # ',I3,' failed at ',I5,I4,1X,I4)
                     GOTO 60
                  ENDIF
 
               ENDIF
*
*     Found a matching bank
*
               IF(IDEBFA.GE.1)
     +         PRINT *,'FMFZI. match succeeded against candidate # ',
     +         IKEY
               GOTO 80
 
   60          CONTINUE
*
*     Finished with this bank, drop it
*
               CALL MZDROP(IDIVFA,LOLDFA,'L')
               LOLDFA = 0
   70       CONTINUE
            GOTO 120
*
*     Get here if we have found a matching bank
*
   80       CONTINUE
            IF(COMAND(1:3) .EQ. 'PUT') THEN
               IF(IDEBFA.GE.0) PRINT *,
     +         'FMFZI. Error - file already exists, will not be added'
               CALL MZDROP(IDIVFA,LOLDFA,'L')
               LOLDFA = 0
               GOTO 140
            ELSE
               IF(IDEBFA.GE.0) PRINT *,
     +         'FMFZI. File already exists, will be replaced'
*
*     Drop bank from catalogue
*
               CALL MZDROP(IDIVFA,LOLDFA,'L')
               LOLDFA = 0
               ICYCLE = 9999
*
*     Check if keys match...
*
               IREP = 0
               DO 90 I=2,LKEYFA
                  IF(KEYSOU(I,IKEY).NE.KEYS(I)) THEN
                     IREP = 1
                     GOTO 100
                  ENDIF
*                 IF(KEYSOU(I,IKEY).NE.KEYS(I)) GOTO 100
   90          CONTINUE
  100          CONTINUE
*
*     Update using RZOUT option R
*
               CALL RZOUT (IDIVFA, LNEWFA, KEYSOU(1,IKEY), ICYCLE, 'LR')
*
*     Replace key vector in place if it is different
*
               IF(IREP.NE.0) THEN
                  KEYS(1) = KEYSOU(1,IKEY)
                  CALL FMRKEY(KEYSOU(1,IKEY),KEYS,IRC)
               ENDIF
               CALL MZDROP(IDIVFA,LOLDFA,'L')
               GOTO 140
 
  110          CONTINUE
               PRINT *,'Deleting entry with following key vector...'
               CALL FMPKEY(KEYSOU(1,IKEY),LKEYFA)
               CALL RZDELK(KEYSOU(1,IKEY),ICYCLE,'C')
               IF(IQUEST(2).NE.0) THEN
                  NFMPDK = NFMPDK + 1
                  PRINT *,'FMFZI. warning from RZDELK - cycles '//
     +               'structure contains previously deleted objects'
                  CALL RZFREE('RZFILE')
                  LTOP = LENOCC(TOPDIR)
                  CALL RZVERI(TOPDIR(1:LTOP),'B')
                  CALL RZLOCK('RZFILE')
               ENDIF
               KEYOLD = KEYSOU(1,IKEY)
 
            ENDIF
            GOTO 130
*
*     Get here if there is no matching bank
*
  120       CONTINUE
 
            IF(COMAND(1:3) .EQ. 'REP') THEN
               PRINT *,
     +         'FMFZI. Error - replace operation for non-existant bank'
               NFMPRP = NFMPRP + 1
               CALL MZDROP(IDIVFA,LNEWFA,'L')
               LNEWFA = 0
               GOTO 150
            ENDIF
 
         ENDIF
 
  130    CONTINUE
         LEND = INDEXB(GENEN,'/') - 1
         CALL RZCDIR (GENEN(1:LEND), ' ')
*        NKEYFA = IQUEST(7)
*        KEYS(1) = NKEYFA + 1
         IF(KEYOLD.NE.0) THEN
            IF(IDEBFA.GE.3)
     +      PRINT *,'FMFZI. reusing key # ',KEYOLD
            KEYS(1) = KEYOLD
         ELSE
            CALL FMALLK(KEYS(1),IRC)
            IF(IDEBFA.GE.3)
     +      PRINT *,'FMFZI. allocated new key # ',KEYS(1)
         ENDIF
*
*        Write new bank to RZ file
*
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMFZI. adding new entry with following KEYS'
            CALL FMPKEY(KEYS,LKEYFA)
         ENDIF
 
* (can eventually use option Q for speed)
         CALL RZOUT (IDIVFA, LNEWFA, KEYS, ICYCLE, 'L')
         IF(IDEBFA.GE.3) PRINT *,'FMFZI. new entry has cycle # ',
     +      ICYCLE
  140    CONTINUE
*
*    Delete previous cycle
*
*        IF(KEYOLD.NE.0) THEN
*           CALL RZDELK(KEYS,ICYCLE-1,' ')
*        ENDIF
#if defined(CERNLIB_SQLCOM)
         MODE = 0
         IF(COMAND(1:3) .EQ. 'REP') MODE = 1
         CALL FMZTOR(GENEN,LNEWFA+KOFUFA,MODE,IRC)
         PRINT *,'FMFZI. Return code ',IRC,' from ORACLE/SQL'
*
*     Attempt reconnect in case ORACLE has died
*
         IF((IRC.EQ.-6065).OR.(IRC.EQ.-3114)) THEN
            PRINT *,'FMFZI. attempting reconnect to ORACLE'
            CALL FMLOGI(IRC)
            CALL FMZTOR(GENEN,LNEWFA+KOFUFA,MODE,IRC)
            PRINT *,'FMFZI. Return code ',IRC,' from ORACLE/SQL'
         ENDIF
 
         IF (IRC .NE. 0) THEN
            CALL DZSHOW('FMFZI.',IDIVFA,LNEWFA,'L',0,0,0,0)
         ENDIF
*        ENDIF
#endif
         CALL MZDROP(IDIVFA,LNEWFA,'L')
         LNEWFA = 0
      ELSEIF(COMAND.EQ.'LOG') THEN
*
*     Logging record
*
         NFMLOG = NFMLOG + 1
         IF(IDEBFA.GE.3) CALL FMPLOG(LPRTFA,IUHEAD,LHEAD,IRC)
#if defined(CERNLIB_FATLOG)
         LFORM = IQUEST(20)
         CALL UCOPY(IQUEST(21),IFORM,LFORM)
         CALL FATLOG(IUHEAD,LHEAD,IFORM,LFORM,IRC)
#endif
      ELSE
*
*     Unrecognised command
*
         NFMBAD = NFMBAD + 1
         WRITE(LPRTFA,9011) COMAND
 9011 FORMAT(' FMFZI. !!! unrecognised command !!! ',A)
      ENDIF
 
      CALL FMRTIM(DELTIM)
      CALL TIMED(T)
      PRINT *,'FMFZI. Elapsed time = ',
     +         DELTIM,' CP time = ',T,' sec.'
 
      GOTO 10
 
  150 CONTINUE
      CALL FMRTIM(DELTIM)
      CALL TIMED(T)
      PRINT *,'FMFZI. Elapsed time = ',
     +         DELTIM,' CP time = ',T,' sec.'
  160 CONTINUE
      CLOSE (LUFZFA)
      CALL RZSAVE
#if !defined(CERNLIB_IBMVM)
*
*     Did we see a ZEBRA end of run?
*
      IF(IEOR+ISTYLE.EQ.0) GOTO 170
#endif
 
*     CALL FMSAVE(LUNRZ)
      CALL FZENDI(LUFZFA,'T')
      IQUEST(1) = 0
      RETURN
 
#if !defined(CERNLIB_IBMVM)
  170 CONTINUE
      WRITE(LPRTFA,*) 'FMFZI. ZEBRA e-o-r not found on unit ',LUFZFA
      CALL FZENDI(LUFZFA,'T')
      IQUEST(1) = 2
      RETURN
 
#endif
  180 CONTINUE
      WRITE(LPRTFA,*) 'FMFZI. Error opening input file on unit ',LUFZFA
      CALL FZENDI(LUFZFA,'T')
      IQUEST(1) = 1
      RETURN
 
  190 CONTINUE
      WRITE(LPRTFA,*) 'FMFZI. fast exit on warning from RZDELK'
      CALL FZENDI(LUFZFA,'T')
      IQUEST(1) = 2
      RETURN
*
      END
 
