*
* $Id: fmrzin.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fmrzin.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMRZIN (CHNAME,IUDIV,LSUP,JBIAS,NWKEY,KEYS,IFLAG)
*
************************************************************************
*
*
*   Inputs one data structure from the Random Access file
*
*   Arguments :
*
*     GENAME   Generic name
*     IUDIV    Division index where the data structure exists
*     LSUP     Address supporting the data structure
*     JBIAS    < 1 : LSUP is the supporting bank and JBIAS is the link
*                    bias specifying where the data structure has to
*                    be introduced
*              = 1 : LSUP is the supporting link
*              = 2 : Stand alone data structure with address at LSUP
*     NWKEY    Maximum size available for KEYS vector
*              On return it carries the actual size of the KEYS
*     KEYS     Key vector
*     IFLAG    = 0 Input key vector ignored - select 'best' copy
*              = 1 Input key vector used to select
*
*   Called by User
*
*   Error Condition :
*
*     IQUEST(1) =  0 : No error
*               = 10 : Path name contains wild-cards
*               = 11 : Illegal Path name
*               = 12 : Path name cannot be found in the list of those
*                      initialized
*               = 13 : Error in RZ for retrieving the data structure
*               = 14 : File not found
*               = 15 : File rejected by KEYS selection
*               = 16 : Volume not known
*               = 17 : Volume unavailable
*               = 42 : RZ file inaccessible
*
*   Selection is performed as follows:
*
*      If IFLAG=1, input key vector is used
*              =0, First available of
*                     {local disk|local robot|local tape} is taken
*
************************************************************************
*
#include "fatmen/faust.inc"
#include "fatmen/fatapol3.inc"
#include "fatmen/fatclio.inc"
#if defined(CERNLIB_SHIFT)
#include "fatmen/fatshift.inc"
#endif
#include "fatmen/slate.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatsel.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#if defined(CERNLIB_CRAY)
      PARAMETER       (IQCHAW=8)
#endif
#if !defined(CERNLIB_CRAY)
      PARAMETER       (IQCHAW=4)
#endif
      CHARACTER*15    XVID
      CHARACTER*8     VIP
      CHARACTER*(*)   CHNAME
      CHARACTER       PATH*255,   PATHN*255,
     +                PATHX*255,  FNAME*20
      CHARACTER*255   GENAME
*     DIMENSION       LSUP(9), KEYS(*)
      PARAMETER       (LKEYFA=10)
*     DIMENSION       LSUP(9), KEYS(LKEYFA)
      DIMENSION       KEYS(LKEYFA)
#include "fatmen/fmaxcop.inc"
      DIMENSION KEYSOU(LKEYFA,MAXCOP),KEYSIN(LKEYFA)
      CHARACTER*8     HOST1,HOST2,HTYPE,HSYS
      CHARACTER*12    FSEQ
      CHARACTER*6     VID,VSN
      CHARACTER*4     FFORM
      CHARACTER*8     CLUNAM,CLUNOD
      CHARACTER*255   CHDSN,DFSNAM
      CHARACTER*256   DSN
*     Save 'best' copy of each media type
      DIMENSION       IFOUND(KMXMTP)
      INTEGER         FMHOST
#if defined(CERNLIB_VAXVMS)
      INTEGER SYS$GETSYIW,SYS$GETMSG
      INTEGER FAFNDF
      INCLUDE '($RMSDEF)'
      INCLUDE '($SSDEF)'
       STRUCTURE /ITMLST/
         UNION
            MAP
            INTEGER*2 BUFFER_LENGTH
              INTEGER*2 ITEM_CODE
            INTEGER*4 BUFFER_ADDRESS
              INTEGER*4 RETURN_LENGTH_ADDRESS
            ENDMAP
            MAP
            INTEGER*4 END_LIST /0/
            ENDMAP
         END UNION
      END STRUCTURE
 
      RECORD /ITMLST/ SYI_LIST(2)
 
      INCLUDE '($SYIDEF)'
      CHARACTER*255 EQUNAM
      CHARACTER*255 CHMESS
      CHARACTER*255 CHFILE
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)||defined(CERNLIB_IBMMVS)
      CHARACTER*255 CHNFS
      LOGICAL       IEXIST
#endif
      CHARACTER*80  CHLINE
      PARAMETER     (MAXTRY=100)
      PARAMETER     (NSECS=10)
#include "fatmen/tmsdef.inc"
*
*     ------------------------------------------------------------------
*
* *** Find number of characters in the path name
*
      LGN    = LENOCC(CHNAME)
      GENAME = CHNAME(1:LGN)
      CALL CLTOU(GENAME(1:LGN))
 
*     Check for wild-cards in generic-name
*
      IWILD = ICFMUL('*%(<>[]',GENAME,1,LGN)
      IF(IWILD.LE.LGN) THEN
         PATH = ' '
         PATH(IWILD:IWILD+3) = '^---'
         WRITE(LPRTFA,9001) GENAME(1:LGN),PATH(1:IWILD+3)
 9001 FORMAT(' FMRZIN. wild-cards not permitted in generic name',/,
     +       1X,A,/,1X,A)
         IQUEST(1) = 10
         GOTO 999
      ENDIF
 
      ICH    = INDEXB(GENAME(1:LGN),'/')
      PATHN  = GENAME(1:ICH-1)
      FNAME  = GENAME(ICH+1:LGN)
      NCH    = ICH-1
      NCHN   = LGN-ICH
      IF (NCH.LT.3.OR.PATHN(1:2).NE.'//') THEN
        IQUEST(1) = 11
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9054) PATHN(1:NCH)
        GOTO 999
      ENDIF
*
*     Retry loop
*
      NTRY   = 0
   10 CONTINUE
      ICODE  = 0
*
* *** Set the current directory
*
      CALL FACDIR (PATHN, 'U')
*     CALL FACDIR (PATHN, ' ')
      IF (IQUEST(1).NE.0) THEN
        IQUEST(1) = 11
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9055) PATHN(1:NCH)
        GOTO 999
      ENDIF
*
*     Get modification date of the directory
*
      IDATEM = IQUEST(16)
      ITIMEM = IQUEST(17)
      IF(IDEBFA.GE.2) WRITE(LPRTFA,9062) IDATEM,ITIMEM
 
      NKEYFA = IQUEST(7)
      NWKYFA = IQUEST(8)
      LDIRFA = IQUEST(11)
      IKDRFA = IQUEST(13)
      CALL FACDIR (PATH, 'R')
*     NCH    = INDEX (PATH, ' ') - 1
      NCH    = LENOCC(PATH)
      IF (NCH.LE.0) NCH = MAXLFA
*
* *** Check if the Path name matches
*
      LSAVFA = LTOPFA
   20 IF (LSAVFA.NE.0) THEN
        NCHR   = IQ(KOFUFA+LSAVFA+MNCHFA)
        CALL UHTOC (IQ(KOFUFA+LSAVFA+MCHRFA), 4, PATHX, NCHR)
        IF (PATHN(1:NCHR).NE.PATHX(1:NCHR)) THEN
          LSAVFA = LQ(KOFUFA+LSAVFA)
          GOTO 20
        ENDIF
      ELSE
        IQUEST(1) = 12
        IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9055) PATHN(1:NCH)
        GOTO 999
      ENDIF
      IQ(KOFUFA+LSAVFA+MTOTFA) = NKEYFA
 
*------------------ new FATMEN selection -------------------------------
      IF(IFLAG.EQ.0) THEN
 
      CALL VBLANK(KEYSIN(2),5)
      KEYSIN(1)  = 0
      KEYSIN(10) = 10
      CALL UCTOH(FNAME,KEYSIN(2),4,MIN(NCHN,20))
      CALL VZERO(IFOUND,KMXMTP)
*
*     Don't compare copy level
*
        KEYSIN(MKCLFA) = -1
*
*     Don't compare location code (for the time being)
*
        KEYSIN(MKLCFA) = -1
*
*     Get current host
*
        IC = FMHOST(HOST1,HTYPE,HSYS)
        CALL CLTOU(HOST1)
        LH1 = LENOCC(HOST1)
*
*     Pointer for manually mounted tape
*
        ISAVE = 0
*
*     Pointer for 'served' dataset (e.g. NFS/DFS/FPACK)
*
        JSAVE = 0
*
*     N.B. see warning below (locate !!!!)
*
*
*       Range of media types
*
        ILOW  = MRMTFA(1)
        IHIGH = MRMTFA(2)
 
        IMEDIA = ILOW
        ICOUNT = 1
        IF(NUMMTP.GT.0) IMEDIA = MFMMTP(ICOUNT)
   30   CONTINUE
 
*       DO 100 IMEDIA = ILOW,IHIGH
*
*     For each media type, look for 'best' copy. For tapes,
*     a robotically mounted tape is always taken over a manually
*     mounted one.
*     If the user has defined a list of media, process in order
*
        KEYSIN(MKMTFA) = IMEDIA
 
        CALL FMSELK(PATHN(1:NCH)//'/'//FNAME(1:NCHN),
     +   KEYSIN,KEYSOU,NMATCH,MAXCOP,IRC)
        IF(IDEBFA.GE.2) WRITE(LPRTFA,9002) NMATCH,IMEDIA
 9002 FORMAT(' FMRZIN. found ',I6,' matches for media type ',I6)
*
*     See if any of these copies are directly accessible
*
        DO 130 I=1,NMATCH
*
*     Check that keys match those selected
*     Location code:
*
        IF(NUMLOC.GT.0) THEN
           IF(IUCOMP(KEYSOU(MKLCFA,I),MFMLOC,NUMLOC).EQ.0) THEN
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9003) I
 9003 FORMAT(' FMRZIN. candidate # ',I6,' fails location code check')
           ICODE = 15
           GOTO 130
           ENDIF
        ENDIF
*
*     Copy level:
*
        IF(NUMCPL.GT.0) THEN
           IF(IUCOMP(KEYSOU(MKCLFA,I),MFMCPL,NUMCPL).EQ.0) THEN
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9004) I
 9004 FORMAT(' FMRZIN. candidate # ',I6,' fails copy level check')
           ICODE = 15
           GOTO 130
           ENDIF
        ENDIF
*
*     If IMEDIA=1, check that host name matches
*     else, use VID to check whether in a robot or manual library
*
        ICYCL  = 9999
        JBIAS  = 2
        CALL RZIN(IUDIV,LFAT,JBIAS,KEYSOU(1,I),ICYCL,' ')
        NFBANK = NFBANK + 1
        IF (IQUEST(1).NE.0) THEN
           IF (IDEBFA.GE.-3) THEN
              WRITE (LPRTFA, 9056) IQUEST(1),PATHN(1:NCH)
              CALL FMSHOK('Object: ',KEYSOU(1,I),LKEYFA)
           ENDIF
           IQUEST(1) = 13
        ENDIF
 
        IF(IMEDIA.EQ.1) THEN
*
*     Check that host name matches. If not, reject
*
           CALL UHTOC(IQ(LFAT+KOFUFA+MHSNFA),4,HOST2,8)
           LH2 = LENOCC(HOST2)
           CALL CLTOU(HOST2)
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9005) HOST1(1:LH1),
     +        HOST2(1:LH2)
 9005 FORMAT(' FMRZIN. Checking hostname: local host/ catalogue = ',
     +       A8,' / ',A8)
           IF(HOST1(1:LH1).EQ.HOST2(1:LH2)) THEN
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9006)
 9006 FORMAT(' FMRZIN. found matching dataset on a disk')
              CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
              IF(ISEL.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
 9007 FORMAT(' FMRZIN. user selection passed')
                 GOTO 140
              ELSE
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
 9008 FORMAT(' FMRZIN. user selection failed')
              ENDIF
           ELSE
              IF(IDEBFA.GE.2) WRITE(LPRTFA,9009)
 9009 FORMAT(' FMRZIN. check on host name failed')
           ENDIF
#if defined(CERNLIB_FPACK)
*
*     Check .netrc file for host name
*
           CALL UHTOC(IQ(LFAT+KOFUFA+MFLFFA),4,FFORM,4)
           IF(FFORM(1:2).EQ.'FP') THEN
              IF(IDEBFA.GE.2) WRITE(LPRTFA,9010)
 9010 FORMAT(' FMRZIN. checking FPACK server access (.netrc file)')
              CALL FMNTRC(HOST2(1:LH2),FFORM,IRC)
              IF(IRC.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9011)
 9011 FORMAT(' FMRZIN. found matching dataset accessible through ',
     +              'FPACK server')
                 CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
                 IF(ISEL.EQ.0) THEN
                    IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                    IF(JSAVE.EQ.0) THEN
                       JSAVE          = I
                       IFOUND(IMEDIA) = I
                    ENDIF
                 ELSE
                    IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                 ENDIF
              ELSE
                 IF(IDEBFA.GE.3) WRITE(LPRTFA,9012) HOST2(1:LH2)
 9012 FORMAT(' FMRZIN. host ',A8,' not accessible via FPACK server')
              ENDIF
           ENDIF
#endif
#if defined(CERNLIB_UNIX)
*
*     Check for AFS mounted files
*
      DSN  = ' '
      CALL UHTOC(IQ(LFAT+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      LDSN = LENOCC(DSN)
      CALL CUTOL(DSN(1:LDSN))
      IF(IDEBFA.GE.2) WRITE(LPRTFA,9013) DSN(1:LDSN)
 9013 FORMAT(' FMRZIN. checking AFS access to ',A)
      IF(IDEBFA.GE.2) WRITE(LPRTFA,9014) DSN(1:LDSN)
 9014 FORMAT(' FMRZIN. looking for file ',A)
      INQUIRE(FILE=DSN(1:LDSN),EXIST=IEXIST)
      IF(IEXIST) THEN
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9015)
 9015 FORMAT(' FMRZIN. found matching dataset accessible via AFS')
         CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
         IF(ISEL.EQ.0) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
            IF(JSAVE.EQ.0) THEN
               JSAVE          = I
               IFOUND(IMEDIA) = I
            ENDIF
         ELSE
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
         ENDIF
      ELSE
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9016)
 9016 FORMAT(' FMRZIN. file not found via AFS')
      ENDIF
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
*
*     Check for NFS mounted files
*
      DSN  = ' '
      CALL UHTOC(IQ(LFAT+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      LDSN = LENOCC(DSN)
      IF(DSN(1:1).EQ.'$') THEN
         LSLASH = INDEX(DSN(1:LDSN),'/')
         IF(LSLASH.NE.0) THEN
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9017) DSN(1:LSLASH-1)
 9017 FORMAT(' FMRZIN. found environmental variable/logical',
     +       ' name in DSN - ',A)
#endif
#if defined(CERNLIB_UNIX)
            CALL GETENVF(DSN(2:LSLASH-1),CHNFS)
            LCHNFS = IS(1)
#endif
#if defined(CERNLIB_VAXVMS)
            CALL FMGTLG(DSN(2:LSLASH-1),CHNFS,'LNM$SYSTEM',ILOGRC)
*
*     Logical name not defined: convert to VMS format
*     $ENV/filename -> ENV:filename
*
            IF(ILOGRC.NE.0) THEN
               CHNFS  = DSN(2:LSLASH-1)//':'//DSN(LSLASH+1:LDSN)
               LCHNFS = LDSN - 1
            ELSE
               LCHNFS = IS(1)
            ENDIF
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
            IF(LCHNFS.GT.0) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9018) CHNFS(1:LCHNFS)
 9018 FORMAT(' FMRZIN. value = ',A)
*
*     Now build file name
*
#endif
#if defined(CERNLIB_UNIX)
               CHNFS(LCHNFS+1:) = DSN(LSLASH:LDSN)
               LCHNFS           = LCHNFS + LDSN - LSLASH + 1
               CALL CUTOL(CHNFS(1:LCHNFS))
#endif
#if defined(CERNLIB_VAXVMS)
*
*     If there is more than one slash in file name
*     assume that the intervening elements are directory names
*
               IF(ILOGRC.EQ.0) THEN
                  JSLASH              = INDEXB(DSN(1:LDSN),'/')
                  IF(JSLASH.EQ.LSLASH) THEN
                     CHNFS(LCHNFS+1:) = DSN(LSLASH+1:LDSN)
                     LCHNFS           = LCHNFS + LDSN - LSLASH
                  ELSE
                     CHNFS(LCHNFS+1:) = '[' // DSN(LSLASH+1:JSLASH-1)
     +                                  // ']' // DSN(JSLASH+1:LDSN)
                     LCHNFS           = LCHNFS + LDSN - LSLASH + 1
                     CALL CTRANS('/','.',CHNFS,1,LCHNFS)
                  ENDIF
               ENDIF
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Does file exist? (This will also work for the same VAXcluster)
*
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9019) CHNFS(1:LCHNFS)
 9019 FORMAT(' FMRZIN. checking NFS access to ',A)
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9020) CHNFS(1:LCHNFS)
 9020 FORMAT(' FMRZIN. looking for file ',A)
               INQUIRE(FILE=CHNFS(1:LCHNFS),EXIST=IEXIST)
               IF(IEXIST) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9021)
 9021 FORMAT(' FMRZIN. found matching dataset on a disk mounted ',
     +               'by NFS')
                  CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
                  IF(ISEL.EQ.0) THEN
                     IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                     IF(JSAVE.EQ.0) THEN
                        JSAVE          = I
                        IFOUND(IMEDIA) = I
                     ENDIF
                  ELSE
                     IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                  ENDIF
               ELSE
                  IF(IDEBFA.GE.3) WRITE(LPRTFA,9022)
 9022 FORMAT(' FMRZIN. file not found via NFS')
               ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
*
*      File not found, try DECnet access
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9023)
     +         HOST2(1:LH2)//'::'//CHNFS(1:LCHNFS)
 9023 FORMAT(' FMRZIN. checking DECnet access to ',A)
            INQUIRE(FILE=HOST2(1:LH2)//'::'//CHNFS(1:LCHNFS),
     +              EXIST=IEXIST)
            IF(IEXIST) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9024)
 9024 FORMAT(' FMRZIN. found matching ',
     +           'dataset on a disk accessible via DECnet')
               CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
               IF(ISEL.EQ.0) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                  IF(JSAVE.EQ.0) THEN
                     JSAVE          = I
                     IFOUND(IMEDIA) = I
                  ENDIF
               ELSE
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
               ENDIF
            ELSE
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9025)
 9025 FORMAT(' FMRZIN. file not found via DECnet')
            ENDIF
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Check whether the node in the catalogue is the VAXcluster alias
*
      CALL FMGTLG('SYS$CLUSTER_NODE',CLUNAM,'LNM$SYSTEM',IRC)
      LC = INDEX(CLUNAM,':') -1
      IF(IDEBFA.GE.2) WRITE(LPRTFA,9026) HOST2,CLUNAM(1:LC)
 9026 FORMAT(' FMRZIN. host/cluster-alias = ',A,' ',A)
      IF(HOST2(1:LH2).EQ.CLUNAM(1:LC))THEN
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9027)
 9027 FORMAT(' FMRZIN. found matching dataset on a disk in this ',
     +             'VAXcluster')
              CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
              IF(ISEL.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                 GOTO 140
                 ELSE
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                 ENDIF
         ENDIF
*
*     Check whether the node in the catalogue is in
*     the same VAXcluster as us
*
      SYI_LIST(1).BUFFER_LENGTH         = 1
      SYI_LIST(1).ITEM_CODE             = SYI$_CLUSTER_MEMBER
      SYI_LIST(1).BUFFER_ADDRESS        = %LOC(IMEMB)
      SYI_LIST(1).RETURN_LENGTH_ADDRESS = %LOC(LTEMP)
      SYI_LIST(2).END_LIST              = 0
 
      ISTAT = SYS$GETSYIW(,,HOST2(1:LH2),SYI_LIST,,,)
 
*     IF (ISTAT .NE. SS$_NOSUCHNODE) THEN
      IF (IMEMB.NE.0) THEN
         IF(IDEBFA.GE.3) WRITE(LPRTFA,9028) HOST2,ISTAT
 9028 FORMAT(' FMRZIN. status from check on ',A,
     +       ' for VAXcluster membership = ',I10)
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9029)
 9029 FORMAT(' FMRZIN. found matching dataset on a disk in this',
     +       ' VAXcluster')
              CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
              IF(ISEL.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                 GOTO 140
                 ELSE
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                 ENDIF
 
         ELSE
*
*     No, check whether the disk in question is known
*     on this system and mounted via DFS
*     (Set JSAVE)
*
         CALL UHTOC(IQ(LFAT+KOFUFA+MFQNFA),4,DSN,NFQNFA)
         LDSN = LENOCC(DSN)
*
*     First, get full file name
*
         ICONT = 0
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9030) DSN(1:LDSN)
 9030 FORMAT(' FMRZIN. issuing LIB$FIND_FILE for ',A)
         IFIND = FAFNDF(DSN(1:LDSN),CHDSN,ICONT)
         IEND  = LIB$FIND_FILE_END(ICONT)
         IF(IFIND.EQ.RMS$_SUC) THEN
            IF(IDEBFA.GE.2) THEN
               WRITE(LPRTFA,9031) DSN(1:LDSN),CHDSN(1:LENOCC(CHDSN))
 9031 FORMAT(' FMRZIN. file ',A,' exists, full name = ',A)
               WRITE(LPRTFA,9032)
 9032 FORMAT(' FMRZIN. checking for DFS access...')
            ENDIF
 
            LCHDSN = INDEX(CHDSN,':') - 1
            IF(LCHDSN.GT.0) THEN
               CALL FMGTLG(CHDSN(1:LCHDSN),DFSNAM,'LNM$SYSTEM',JRC)
               IF((JRC.EQ.0).AND.(INDEX(DFSNAM,'DFSC').NE.0)) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9033)
 9033 FORMAT(' FMRZIN. found matching dataset on a disk mounted by DFS')
                  CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
                  IF(ISEL.EQ.0) THEN
                     IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                     IF(JSAVE.EQ.0) THEN
                        JSAVE          = I
                        IFOUND(IMEDIA) = I
                     ENDIF
                  ELSE
                     IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                  ENDIF
               ENDIF
            ENDIF
         ELSE
*
*      Show error message
*
            IF(IDEBFA.GE.2) THEN
               WRITE(LPRTFA,9034)
 9034 FORMAT(' FMRZIN. file not found via DFS')
               IC = SYS$GETMSG(%VAL(IFIND),LMESS,CHMESS,,)
               PRINT 9063,DSN(1:LDSN),IFIND,CHMESS(1:LMESS)
            ENDIF
*
*      File not found, try DECnet access
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9023)
     +         HOST2(1:LH2)//'::'//DSN(1:LDSN)
            INQUIRE(FILE=HOST2(1:LH2)//'::'//DSN(1:LDSN),EXIST=IEXIST)
            IF(IEXIST) THEN
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9035)
 9035 FORMAT(' FMRZIN. found matching dataset on a disk accessible',
     +       ' via DECnet')
               CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
               IF(ISEL.EQ.0) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                  IF(JSAVE.EQ.0) THEN
                     JSAVE          = I
                     IFOUND(IMEDIA) = I
                  ENDIF
               ELSE
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
               ENDIF
            ELSE
               IF(IDEBFA.GE.2) WRITE(LPRTFA,9036)
 9036 FORMAT(' FMRZIN. file not found via DECnet')
            ENDIF
         ENDIF
      ENDIF
 
#endif
#if defined(CERNLIB_CSPACK)
*
*     For ZEBRA FZ files in binary exchange mode, see if remote node
*     is accessible via CSPACK (check .netrc file)
*
           CALL UHTOC(IQ(LFAT+KOFUFA+MFLFFA),4,FFORM,4)
           IF(FFORM(1:3).NE.'FXN'.AND.INDEX(FFORM,'FX').NE.0) THEN
              IF(IDEBFA.GE.2) WRITE(LPRTFA,9037)
 9037 FORMAT(' FMRZIN. checking CSPACK server access (.netrc file)')
              CALL FMNTRC(HOST2(1:LH2),FFORM,IRC)
              IF(IRC.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9038)
 9038 FORMAT(' FMRZIN. found matching dataset accessible through ',
     +              'CSPACK server')
                 CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
                 IF(ISEL.EQ.0) THEN
                    IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                    IF(JSAVE.EQ.0) THEN
                       JSAVE          = I
                       IFOUND(IMEDIA) = I
                    ENDIF
                 ELSE
                    IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                 ENDIF
              ELSE
                 IF(IDEBFA.GE.3) WRITE(LPRTFA,9039) HOST2(1:LH2)
 9039 FORMAT(' FMRZIN. host ',A,' not accessible via CSPACK server')
              ENDIF
           ENDIF
 
#endif
           ELSE
*
*     Check that tape is in an active library (when TMS is available)
*
*     Choose robot over manual
*
           CALL UHTOC(IQ(LFAT+KOFUFA+MVIDFA),4,VID,6)
           LVID = LENOCC(VID)
           CALL UHTOC(IQ(LFAT+KOFUFA+MVSNFA),4,VSN,6)
           LVSN = LENOCC(VSN)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
           IQUEST(11) = IQ(LFAT+KOFUFA+MMTPFA)
#if defined(CERNLIB_PREFIX)
           CALL FMXVID(VID,IQ(LFAT+KOFUFA+MVIPFA),XVID,VIP,'C',IC)
           LXVID = LENOCC(XVID)
           CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if !defined(CERNLIB_PREFIX)
           CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
           IF(IC.EQ.100) THEN
              ICODE = 16
              IF(IDEBFA.GE.0) WRITE(LPRTFA,9040) VID
 9040 FORMAT(' FMRZIN. Volume ',A8,' unknown to TMS')
              GOTO 120
           ENDIF
 
           IF(IC.EQ.312.OR.IC.EQ.315) THEN
              ICODE = 17
              IF(IDEBFA.GE.0) WRITE(LPRTFA,9041) VID
 9041 FORMAT(' FMRZIN. Volume ',A8,' unavailable')
              GOTO 120
           ENDIF
*
*     Check whether required device exists on this node
*
           LM   = LENOCC(MODEL)
           IDEV = 1
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_DSYIBM))
*
*     Assume all devices are available
*
           IDEV = 0
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_DSYIBM))
*
*     Assume available if file is catalogued
*
      DSN  = ' '
      CALL UHTOC(IQ(LFAT+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      LDSN = LENOCC(DSN)
      CALL CUTOL(DSN(1:LDSN))
      INQUIRE(FILE='/'//DSN(1:LDSN),EXIST=IEXIST)
      IF(IEXIST) IDEV = 0
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_VAXTAP))
*
*     Check if this file is already staged ...
*
           CALL FMITOC(IQ(LFAT+KOFUFA+MFSQFA),FSEQ,LFSEQ)
           CHFILE = VSN(1:LVSN)//'_'//VID(1:LVID)//'.'//
     +              FSEQ(1:LFSEQ)//'_'//LABTYP(1:LENOCC(LABTYP))
           LFILE  = LENOCC(CHFILE)
           ISIZE  = IQ(LFAT+KOFUFA+MFSZFA)
           CALL FMSTGC(CHFILE(1:LFILE),ISIZE,JSIZE,ISTAGE)
           IF(ISTAGE.NE.0) THEN
           CALL FMGTLG('SETUP_'//MODEL(1:LM)//'S',EQUNAM,
     +                 'LNM$SYSTEM',IDEV)
*
*     If logical name does not exist, or in the case of remote
*     libraries, read also setup configuration file
*
           INQUIRE(FILE='SETUP_EXE:TPSERV.CONF',EXIST=IEXIST)
           IF(IDEV.NE.0.AND.IEXIST) THEN
 
              ISTAT = LIB$GET_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
              OPEN(LUNTAP,FILE='SETUP_EXE:TPSERV.CONF',
     +           FORM='FORMATTED',STATUS='OLD',
     +           READONLY,SHARED,IOSTAT=ISTAT)
 
              IF(ISTAT.NE.0) THEN
                 IF(IDEBFA.GE.0) PRINT 9042,ISTAT
 9042 FORMAT(' FMRZIN. cannot open TPSERV configuration file',
     +       ' (SETUP_EXE:TPSERV.CONF), IOSTAT = ',I6)
 
              ELSE
 
   40            CONTINUE
                 READ(LUNTAP,'(A)',END=50) CHLINE
                 LLINE = LENOCC(CHLINE)
                 IF(ISTAT.NE.0) GOTO 50
 
                 IF(INDEX(CHLINE(1:LLINE),'TPSERV').NE.0.AND.
     +              INDEX(CHLINE(1:LLINE),MODEL(1:LM)).NE.0) THEN
                    IDEV = 0
                    GOTO 50
                 ENDIF
 
                 GOTO 40
   50            CONTINUE
                 CLOSE(LUNTAP)
              ENDIF
 
              ISTAT = LIB$FREE_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
 
           ENDIF
           ENDIF
#endif
#if defined(CERNLIB_IBMVM)
*
*     Node specific restrictions - hard coded until NAMES file
*     exists.
*
           IF(HOST1(1:LH1).EQ.'CERNVMB'.AND.MODEL(1:LM)
     +        .EQ.'SMCF') THEN
              IDEV = 1
           ELSE
              IDEV = 0
           ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_SOMETIME))
*
*     Read BATCH resource file
*
           OPEN(LUFZFA,FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +          STATUS='OLD',IOSTAT=ISTAT,
     +          FILE='/'//HOST1(1:LH1)// ' BATRES *')
           IF(ISTAT.NE.0) THEN
              IF(IDEBFA.GE.1) WRITE(LPRTFA,9043) HOST1(1:LH1)
 9043 FORMAT(' FMRZIN. cannot open batch resource file ',A,
     +       ' BATRES *')
           ELSE
   60         CONTINUE
              CHLINE = ' '
              READ(LUFZFA,'(A)',END=70) CHLINE
              IF(INDEX(CHLINE(1:LENOCC(CHLINE)),MODEL(1:LM)).EQ.0)
     +        GOTO 60
              IDEV = 0
   70         CLOSE(LUFZFA)
           ENDIF
#endif
#if defined(CERNLIB_SHIFT)||defined(CERNLIB_CRAY)
*
*     Read /etc/shift.conf file
*
           INQUIRE(FILE=SHCONF(1:LSHCONF),EXIST=IEXIST)
           IF(IEXIST) THEN
              CALL CIOPEN(LUNPTR,'r',SHCONF(1:LSHCONF),ISTAT)
              IF(ISTAT.NE.0) THEN
                 IF(IDEBFA.GE.0) WRITE(LPRTFA,9044) SHCONF(1:LSHCONF)
 9044 FORMAT(' FMRZIN. cannot open SHIFT configuration file: ',A)
              ELSE
   80            CONTINUE
                 CALL FMCFGL(LUNPTR,CHLINE,LLINE,' ',ISTAT)
                 IF(ISTAT.NE.0) GOTO 90
                 IF(IDEBFA.GE.3) WRITE(LPRTFA,9045) CHLINE(1:LLINE),
     +                                              SHCONF(1:LSHCONF)
 9045 FORMAT(' FMRZIN. read line ',A,' from ',A)
                 IF(INDEX(CHLINE(1:LLINE),'TPSERV').NE.0.AND.
     +              INDEX(CHLINE(1:LLINE),MODEL(1:LM)).NE.0) THEN
                    IDEV = 0
                    GOTO 90
                 ENDIF
                 GOTO 80
   90            CONTINUE
                 CALL FMCFGL(LUNPTR,CHLINE,LLINE,'F',ISTAT)
                 CALL CICLOS(LUNPTR)
              ENDIF
           ENDIF
 
#endif
#if defined(CERNLIB_SHIFT)
*
*      If device type not found, check in local TPCONFIG file
*
           IF(IDEV.NE.0) THEN
              INQUIRE(FILE=TPCONF(1:LTPCONF),EXIST=IEXIST)
              IF(IEXIST) THEN
                 CALL CIOPEN(LUNPTR,'r',TPCONF(1:LTPCONF),ISTAT)
                 IF(ISTAT.NE.0) THEN
                    IF(IDEBFA.GE.0) WRITE(LPRTFA,9046) TPCONF(1:LTPCONF)
 9046 FORMAT(' FMRZIN. cannot open tape daemon configuration file ',A)
                 ELSE
  100               CONTINUE
                    CALL FMCFGL(LUNPTR,CHLINE,LLINE,' ',ISTAT)
                    IF(ISTAT.NE.0) GOTO 110
                    IF(IDEBFA.GE.3) WRITE(LPRTFA,9047) CHLINE(1:LLINE),
     +                                                 TPCONF(1:LTPCONF)
 9047 FORMAT(' FMRZIN. read line ',A,' from ',A)
                       IF(INDEX(CHLINE(1:LLINE),MODEL(1:LM)).NE.0) THEN
                       IDEV = 0
                       GOTO 110
                    ENDIF
                    GOTO 100
  110               CONTINUE
                    CALL FMCFGL(LUNPTR,CHLINE,LLINE,'F',ISTAT)
                    CALL CICLOS(LUNPTR)
                 ENDIF
              ENDIF
           ENDIF
#endif
#if defined(CERNLIB_UNIX)
*
*     Assume all devices are available
*
           IF(ICLIO.NE.0) IDEV = 0
#endif
#if defined(CERNLIB_APOLLO)
*
*     Assume all devices are available
*
           IF(IAPOL3.NE.0) IDEV = 0
#endif
           IF(IDEV.NE.0) THEN
              IF(IDEBFA.GE.2) WRITE(LPRTFA,9048) MODEL(1:LM)
 9048 FORMAT(' FMRZIN. device type ',A,' not available on this node')
              GOTO 120
           ENDIF
 
           IF(MNTTYP.EQ.'R') THEN
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9049)
 9049 FORMAT(' FMRZIN. found matching dataset on a robot tape')
              CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
              IF(ISEL.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                 GOTO 140
                 ELSE
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
                 ENDIF
              ENDIF
*
*     Here we have a manually mounted tape
*     Save counter in case no robot tape found
*
              CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
              IF(ISEL.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                 IF(ISAVE.EQ.0) THEN
                    ISAVE = I
                    IFOUND(IMEDIA) = I
                 ENDIF
              ELSE
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
              ENDIF
           ENDIF
 
  120   CONTINUE
 
        CALL MZDROP(IUDIV,LFAT,'L')
        LFAT = 0
 
  130   CONTINUE
*
*    Found a match for this media (manual tape or network disk)
*    - use it
*
        IF(IFOUND(IMEDIA).NE.0) THEN
           I = IFOUND(IMEDIA)
           GOTO 140
        ENDIF
 
        IF(NUMMTP.EQ.0) THEN
           IMEDIA = IMEDIA + 1
           IF(IMEDIA.LE.IHIGH) GOTO 30
        ELSE
           ICOUNT = ICOUNT + 1
           IF(ICOUNT.LE.NUMMTP) THEN
              IMEDIA = MFMMTP(ICOUNT)
              GOTO 30
           ENDIF
        ENDIF
*100    CONTINUE
 
        IF(JSAVE.NE.0) THEN
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
* The following +SELF is ONLY to avoid compiler warnings on IBM systems
* running VS FORTRAN 2.5 (or above, presumably) and MUST BE DELETED
* when an appropriate client is available (e.g. FPACK, NFS, etc.)
*
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#if !defined(CERNLIB_IBM)||defined(CERNLIB_FPACK)
           I = JSAVE
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9050)
 9050 FORMAT(' FMRZIN. found matching dataset on served disk')
              CALL FMUSEL(GENAME,LBANK,KEYSOU(1,I),ISEL)
              IF(ISEL.EQ.0) THEN
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9007)
                 GOTO 140
              ELSE
                 IF(IDEBFA.GE.2) WRITE(LPRTFA,9008)
              ENDIF
*
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
#endif
        ELSEIF(ISAVE.NE.0) THEN
           I = ISAVE
           IF(IDEBFA.GE.2) WRITE(LPRTFA,9051)
 9051 FORMAT(' FMRZIN. found matching dataset on a manual tape')
           GOTO 140
 
           ELSE
*
*     No match, error
*
           IQUEST(1) = 14
           IF(ICODE.NE.0) IQUEST(1) = ICODE
           IF (IDEBFA.GE.-3) WRITE (LPRTFA, 9057)
     +        PATHN(1:NCH)//'/'//FNAME(1:NCHN)
           GOTO 999
           ENDIF
*
*     Have got a matching data set, ok
 
  140   CONTINUE
        CALL MZDROP(IUDIV,LFAT,'L')
        LFAT = 0
        CALL UCOPY(KEYSOU(1,I),KEYS,10)
 
        ENDIF
 
        ICYCL  = 9999
        IF(IDEBFA.GE.3) THEN
           WRITE(LPRTFA,9052)
 9052 FORMAT(' FMRZIN. attempting retrieval of entry with following',
     +       ' KEYS vector')
           CALL FMPKEY(KEYS,LKEYFA)
        ENDIF
        NRETRY = 0
  150   CONTINUE
        CALL RZIN(IUDIV, LSUP, JBIAS, KEYS, ICYCL, ' ')
        NFBANK = NFBANK + 1
        IF(IQUEST(1).NE.0) THEN
 
           IF(IDEBFA.GE.-3) THEN
              WRITE (LPRTFA, 9056) IQUEST(1),PATHN(1:NCH)
              CALL FMSHOK('Object: ',KEYS,LKEYFA)
           ENDIF
 
           IQUEST(1) = 13
*
*       If full keys vector was specified, retry up to 5 times
*
           IF(IFLAG.EQ.1.AND.NRETRY.LT.5) THEN
              NRETRY = NRETRY + 1
              CALL SLEEPF(1)
              CALL RZCDIR(PATHN(1:NCH),'U')
              IF(IDEBFA.GE.0) WRITE(LPRTFA,9053) NRETRY
 9053 FORMAT(' FMRZIN. retry # ',I10)
              GOTO 150
           ENDIF
        ELSE
           IF(IDEBFA.GE.2) THEN
              CALL RZDATE(IQUEST(14),IDATE,ITIME,1)
              PRINT 9058,(IQUEST(I),I=2,4)
              PRINT 9059,(IQUEST(I),I=5,8)
              PRINT 9060,IQUEST(12),IDATE,ITIME,IQUEST(20)
           ENDIF
        ENDIF
 
      NTRY = NTRY + 1
      IF(NTRY.LE.MAXTRY) THEN
*
*     Check that directory has not been modified
*
         CALL FACDIR (PATHN, 'U')
         IF(IQUEST(16).GT.IDATEM) THEN
            IF(IDEBFA.GE.1) WRITE(LPRTFA,9061) PATHN(1:NCH),NTRY,NSECS
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9062) IDATEM,ITIMEM
            CALL SLEEPF(NSECS)
            GOTO 10
         ENDIF
 
         IF(IQUEST(16).EQ.IDATEM.AND.IQUEST(17).GT.ITIMEM) THEN
            IF(IDEBFA.GE.1) WRITE(LPRTFA,9061) PATHN(1:NCH),NTRY,NSECS
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9062) IDATEM,ITIMEM
            CALL SLEEPF(NSECS)
            GOTO 10
         ENDIF
      ENDIF
*
 9054 FORMAT (' FMRZIN. Illegal path name ',A)
 9055 FORMAT (' FMRZIN. RZ file is not initialized for ',A)
 9056 FORMAT (' FMRZIN. Error ',I10,' in RZIN for directory ',A)
 9057 FORMAT (' FMRZIN. No match for ',A,
     +        ' found with current selection')
 9058 FORMAT (' FMRZIN. # records read = ',I3,' rec1 = ',I5,
     +        ' offset = ',I4)
 9059 FORMAT (' FMRZIN. rec2 = ',I5,' icycle = ',I6,' nkeys = ',
     +        I4,' nwkey = ',I3)
 9060 FORMAT (' FMRZIN. nwbk = ',I3,' date/time = ',
     +        I6,1X,I4,' ksn = ',I6)
 9061 FORMAT (' FMRZIN. directory ',A,' modified during processing ',
     +        '- retry # ',I3,' in ',I3,' seconds')
 9062 FORMAT (' FMRZIN. modification date/time = ',I6.6,1X,I4.4)
 9063 FORMAT (' FMRZIN. LIB$FIND_FILE for ',A,
     +        ' failed with status ',Z8/,' error message ',A)
*                                                             END FMRZIN
  999 END
 
