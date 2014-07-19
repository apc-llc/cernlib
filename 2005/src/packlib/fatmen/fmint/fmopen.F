*
* $Id: fmopen.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmopen.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMOPEN(GNAME,CHLUN,LENTRY,CHOPT,IRC)
*
*
*     Return codes:  1-10 : FMOPEN (to be implemented)
*                   11-20 : FMDISK
*                   21-30 : FMTAPE
*                   31-40 : FMFPAK
*     Information:     -1 : invalid option in CHOPT ignored
*
*     Options:
*
*           a - append
*           b - *free*
*           c - concatenate (for M)
*           D - make duplicate into robot
*           E - when used with T, issue SETUP END
*           F - issue FZFILE or FPARM as appropriate
*           g - *free* >>> Use to select G option for SHIFT ???
*           H - stage wHole tape
*           I - deselect -G option for SHIFT s/w
*           j - *free*
*           K - KEEP option on STAGE OUT
*           L - override tape label information with DCB from catalogue
*           m - multi-file?
*           N - don't use DSN on STAGE
*           O - override size with IQUEST(11)
*           P - autoput on STAGE OUT
*           Q - queue stage request (nowait)
*           R - read
*           S - update catalogue with size returned from STAGE IN
*           T - use tape directly (i.e. not stage)
*           U - user will issue open
*           V - as S, but even if file size is non-zero
*           W - write
*           Y - write stage command into file on unit FORLUN, but do not execute
*           X - D/A
*           Z - issue RZFILE
*
      CHARACTER*(*) GNAME,CHLUN
      CHARACTER*255 GENAM
#include "fatmen/faust.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fatmon.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatapol3.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatstg.inc"
#include "fatmen/fatinfo.inc"
#include "zebra/zmach.inc"
#include "fatmen/slate.inc"
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
#if defined(CERNLIB_SHIFT)
#include "fatmen/fmshft.inc"
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*16    RECTYP
#endif
#if defined(CERNLIB_IBMMVS)
#include "fatmen/fatdcb.inc"
#endif
      CHARACTER*8   MODEL
      CHARACTER*9   CHACT
      CHARACTER*8   CHSTAT
      CHARACTER*12  CHFRMT
      CHARACTER*256 DSN
      CHARACTER*255 CHFILE
      CHARACTER*2   MODE
      CHARACTER*4   CFMODE
      CHARACTER*8   FORLUN
      CHARACTER*4   CHFORM,CHTEMP
      CHARACTER*4   FZOPT,RZOPT,SHOPT
      CHARACTER*1   VMOPT
      CHARACTER*40  DCB
      CHARACTER*20  FNAME
      CHARACTER*4   RECFM1
      CHARACTER*4   RECFM
      CHARACTER*8   DDNAME
      CHARACTER*12  CHDIR
      LOGICAL       IEXIST,IVMIO
#if defined(CERNLIB_VAXVMS)
      EXTERNAL      FMCFIO
#endif
#if defined(CERNLIB_IBMVM)
      EXTERNAL      FMVMIO
#endif
#if defined(CERNLIB_SHIFT)
      EXTERNAL      FMXYIO
#endif
#if defined(CERNLIB_CSPACK)
      CHARACTER*8   KOPT
      CHARACTER*8   CHHOST
      EXTERNAL      FMXZIO
      LOGICAL       ICSPAK
#endif
*     DIMENSION     LENTRY(1)
      INTEGER       FMJDAT
#include "fatmen/fatopts.inc"
 
      NCH  = LENOCC(GNAME)
      GENAM = GNAME(1:NCH)
      CALL CLTOU(GENAM)
      IRC  = 0
      INFO = 0
 
#if defined(CERNLIB_IBMVM)
      CALL FMONIT('FMOPEN. '//GENAM(1:NCH)//' CHOPT '//CHOPT)
#endif
#if !defined(CERNLIB_APOLLO)
      IAPOL3 = 0
#endif
*
*     Monitoring information
*
      NFOPEN = NFOPEN + 1
      IHOWFA = 0
      ITIMFA = 0
      CHFNFA = ' '
*
*     Check options
*
      CALL FMCHOP('FMOPEN',CHOPT,'CDEFHIKLMONPQRSTUVWXYZ',IC)
      IF(IC.NE.0) INFO = -1
*
*     Default is read
*
      IF(IOPTR.EQ.0.AND.IOPTW.EQ.0) IOPTR = 1
 
*
*     Take file size from IQUEST vector, if option O is specified
*
      ISIZSG = 0
      IF(IOPTO.NE.0) ISIZSG = IQUEST(11)
*
*     FZ I/O options
*
      ICFOP  = 0
      IF(IOPTF.NE.0) ICFOP = IQUEST(10)
 
      CALL DATIME(ID,IT)
      IDATE  = FMJDAT(IS(1),IS(2),IS(3))
      ISECS  = (IS(4)*60 + IS(5))*60 + IS(6)
 
      LCH = LENOCC(CHOPT)
 
      IF (LENTRY .EQ. 0) THEN
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) GENAM(1:NCH),CHOPT(1:LCH),
     +      ID,IT,IS(6)
 9001 FORMAT(' FMOPEN. enter for ',A,/,' options: ',A,' at ',
     +       I6.6,1X,I4.4,I2.2)
         CALL FMGET(GENAM,LENTRY,KEYS,IRC)
         IF (IRC.NE.0) GOTO 999
      ELSE
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9002) GENAM(1:NCH),LENTRY,
     +      CHOPT(1:LCH),ID,IT,IS(6)
 9002 FORMAT(' FMOPEN. enter for ',A,/,' using user supplied bank at ',
     +       I8,' options: ',A,' at ',I6.6,1X,I4.4,I2.2)
*
*     Update keys from bank
*
         CALL FMUPKY(GENAM(1:NCH),LENTRY,KEYS,IRC)
      ENDIF
*
*     Is this entry a link?
*
      IF(KEYS(MKLCFA).EQ.0) THEN
         CALL UHTOC(IQ(LENTRY+KOFUFA+MFQNFA),4,GENAM,NFQNFA)
         LGN = LENOCC(GENAM)
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9003) GNAME(1:NCH),GENAM(1:LGN)
 9003 FORMAT(' FMOPEN. ',A,' --> ',A)
         NCH = LGN
         CALL VZERO(KEYS,LKEYFA)
         CALL FMGET(GENAM,L,KEYS,IRC)
         IF (IRC.NE.0) GOTO 999
      ELSE
         L = LENTRY
      ENDIF
*
*     MB read/written
*
      IF(IQ(L+KOFUFA+MFSZFA).EQ.0) THEN
         IF(IOPTR.NE.0) THEN
            FATMBR = FATMBR + IQ(L+KOFUFA+MFSZFA)
         ELSEIF(IOPTW.NE.0) THEN
            FATMBW = FATMBW + IQ(L+KOFUFA+MFSZFA)
         ENDIF
      ENDIF
*
*     FATMEN file format (for call to FZFILE,RZFILE)
*
      CALL UHTOC(IQ(L+KOFUFA+MFLFFA),4,CHFORM,4)
      ISIZE  = 0
*
*     Get LUN
*
      LUN    = 0
      LCHLUN = LENOCC(CHLUN)
 
      IF(CHLUN(1:LCHLUN).EQ.'NOWAIT') IOPTQ = 1
 
      IF (LCHLUN.EQ.1)    THEN
         READ(CHLUN,'(I1)') LUN
      ELSEIF(LCHLUN.EQ.2) THEN
         READ(CHLUN,'(I2)') LUN
      ENDIF
 
*
*     Get LUN from CHLUN (DDNAME) if necessary
*
      IF(LUN.EQ.0) CALL FMDD2L(CHLUN(1:LCHLUN),LUN,IRC)
 
*
*     New Zebra uses FORTRAN I/O as default...
*
      LFORM = LENOCC(CHFORM)
 
#if !defined(CERNLIB_IBMVM)
      IF((CHFORM(1:2).EQ.'FX').AND.(ICFOP.LE.1)) THEN
#endif
#if defined(CERNLIB_IBMVM)
      IF((CHFORM(1:2).EQ.'FX').AND.(ICFOP.NE.1).AND.(ICFOP.NE.3)) THEN
#endif
         CHTEMP = CHFORM
         CHFORM = 'F'//CHTEMP(1:LFORM)
      ENDIF
 
#if defined(CERNLIB_FPACK)
      IF(INDEX(CHFORM,'FP').NE.0) THEN
 
         CALL FMFPAK(GENAM(1:NCH),LENTRY,KEYS,CHLUN,CHOPT,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) WRITE(LPRTFA,9004) IRC
 9004 FORMAT(' FMOPEN. return code ',I6,' from FMFPAK')
            GOTO 999
         ENDIF
 
      ELSE
#endif
*
*     Media dependant handling
*
         IF(KEYS(MKMTFA).EQ.1) THEN
 
            IF(IOPTQ.NE.0) THEN
*              IOPTQ = 0
*              IF(IDEBFA.GE.0) WRITE(LPRTFA,9005)
*9005 FORMAT(' FMOPEN. option Q ignored for disk files')
*
*     NOWAIT option - update use count and return as for tapes
*
               IF(IDEBFA.GE.1) WRITE(LPRTFA,9005)
 9005 FORMAT(' FMOPEN. option Q for disk files = just return')
               GOTO 10
            ENDIF
 
            CALL FMDISK(GENAM(1:NCH),L,KEYS,CHLUN,CHOPT,IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9006) IRC
 9006 FORMAT(' FMOPEN. return code ',I6,' from FMDISK')
                  GOTO 999
               ENDIF
*
*   Information for FMCLOS
*
            LFMODE(LUN) = 1
 
         ELSE
 
            IQUEST(10) = ICFOP
            CALL FMTAPE(GENAM(1:NCH),L,KEYS,CHLUN,CHOPT,IRC)
            IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9007) IRC
 9007 FORMAT(' FMOPEN. return code ',I6,' from FMTAPE')
               GOTO 999
            ENDIF
*
*     NOWAIT option
*
            IF(IOPTQ.NE.0)                  GOTO 10
            IF(CHLUN(1:LCHLUN).EQ.'NOWAIT') GOTO 10
*
*     Option Y
*
            IF(IOPTY.NE.0) GOTO 10
*
*     Information for FMCLOS
*
#if defined(CERNLIB_DSYIBM)
*
*     Input staging only available
*
            IF(IOPTW.NE.0) IOPTT = 1
#endif
            IF(IOPTT.EQ.0) THEN
               LFMODE(LUN) = 2
            ELSE
               LFMODE(LUN) = 3
            ENDIF
 
         ENDIF
*
*     Get DSN
*
      CALL FMGDSN(L,DSN,LDSN,IRC)
*
*     Package setup
*
#if defined(CERNLIB_IBMVM)
      IVMIO  = .FALSE.
*
*     Suppress user open for FX, FXN files
*
      IF(ICFOP.EQ.2) THEN
         IVMIO = .TRUE.
         IF(IOPTU.NE.0) THEN
            IF(IDEBFA.GE.-3) WRITE(LPRTFA,9008)
 9008 FORMAT(' FMOPEN. user open not allowed for direct-access',
     +       ' I/O with Zebra FZ')
          ENDIF
          IOPTU = 0
       ENDIF
#endif
 
#if defined(CERNLIB_IBM)
         IF(IOPTR.EQ.0.AND.IOPTW.NE.0) THEN
            CHACT = 'WRITE'
            LCHACT = 5
         ELSEIF(IOPTR.NE.0.AND.IOPTW.NE.0) THEN
            CHACT = 'READWRITE'
            LCHACT = 9
         ELSE
            CHACT  = 'READ'
            LCHACT = 4
         ENDIF
#endif
*
*     I/O options for FZ:
*            IQUEST(10) = 1 - Use C I/O in FZ
*            IQUEST(10) = 2 - Use FORTRAN D/A I/O
*            IQUEST(10) = 3 - Use 'package' I/O in FZ (e.g. IOPACK)
*
         IF(ICFOP.EQ.2) IOPTX = 1
 
         IF((IOPTX.NE.0).AND.(IOPTT.NE.0)) THEN
            IF(IDEBFA.GE.-3) WRITE(LPRTFA,9009)
 9009 FORMAT(' FMOPEN. FORTRAN direct-access ',
     +         'not valid for tape files - ignored')
            IOPTX = 0
         ENDIF
 
         IF(ICFOP.EQ.1) THEN
            IF(IOPTU.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9010)
 9010 FORMAT(' FMOPEN. user open not allowed',
     +            ' for C I/O with Zebra FZ')
            ENDIF
            IOPTU = 0
         ENDIF
*
*     Set mode (read/write)
*
         IMODE  = IOPTW
*
*     CHLUN can have the following formats:
*        nn, FTnnFlll, VMnnFlll, IOFILEnn, FORnnn, fort.nn
*     Other formats may be used with FPACK, for example BOSINPUT
*
         LUN    = 0
         LCHLUN = LENOCC(CHLUN)
 
         IF (LCHLUN .EQ. 1) THEN
            READ(CHLUN,'(I1)') LUN
         ELSEIF(LCHLUN .EQ. 2) THEN
            READ(CHLUN,'(I2)') LUN
         ENDIF
*
*     Get LUN from CHLUN (DDNAME) if necessary
*
         IF(LUN.EQ.0) CALL FMDD2L(CHLUN(1:LCHLUN),LUN,IRC)
*
*     Set FZFILE options: C I/O, FORTRAN I/O, package etc.
*
         JFMODE(LUN) = ICFOP
*
*     Mode for FZENDx (In or Out)
*
         IFMODE(LUN) = IMODE
*
*     Issue FZFILE and do the OPEN
*
         IF(IOPTF.NE.0) THEN
*
*     Build FZ options
*        Direction...
*
         FZOPT  = 'I'
         IF(IOPTW.NE.0) FZOPT = 'O'
         LFZOPT = 1
*
*     Medium...
*
#if !defined(CERNLIB_SETUP)
 
         IF((IQ(L+KOFUFA+MMTPFA).GT.1).AND.(IOPTT.NE.0)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'T'
#endif
#if (!defined(CERNLIB_SETUP))&&(!defined(CERNLIB_IBMVM))
*SELF,IF=-SETUP. !! When FZHOOK -> FMVMIO -> VMIO working
         ELSEIF(IOPTX.NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'D'
#endif
#if !defined(CERNLIB_SETUP)
         ENDIF
#endif
#if defined(CERNLIB_SETUP)
         IF((IQ(L+KOFUFA+MMTPFA).EQ.1).AND.(IOPTX.NE.0)) THEN
#endif
#if (defined(CERNLIB_SETUP))&&(!defined(CERNLIB_IBMVM))
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'D'
#endif
#if defined(CERNLIB_SETUP)
         ELSE
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'T'
         ENDIF
#endif
*
*     Data Format...
*
         IF(INDEX(CHFORM,'A').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'A'
         ELSEIF(INDEX(CHFORM,'X').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'X'
         ENDIF
*
*     FORTRAN I/O...
*
         IF((INDEX(CHFORM,'FFX').NE.0).AND.(ICFOP.EQ.0)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'F'
         ENDIF
*
*     Package I/O... (e.g. IOPACK, MAGTAP)
*
         IF((INDEX(CHFORM,'FX').NE.0).AND.(ICFOP.EQ.3)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'Y'
         ENDIF
*
*     File format X, but native data...
*
         IF(INDEX(CHFORM,'FXN').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'N'
         ENDIF
#if defined(CERNLIB_CSPACK)
*
*     File does not exist but we can access remote node
*
         INQUIRE(FILE=DSN(1:LDSN),EXIST=IEXIST)
         ICSPAK = .FALSE.
         IF(.NOT.IEXIST.AND.
     +      (CHFORM(1:3).NE.'FXN'.AND.INDEX(CHFORM,'FX').NE.0)) THEN
            CALL UHTOC(IQ(L+KOFUFA+MHSNFA),4,CHHOST,NHSNFA)
            LHOST  = LENOCC(CHHOST)
            CALL FMNTRC(CHHOST(1:LHOST),CHFORM,IRC)
            IF(IRC.EQ.0) THEN
               LFZOPT = LFZOPT + 1
               FZOPT(LFZOPT:LFZOPT) = 'C'
               IF(CHFORM(1:2).EQ.'FX') THEN
                  LFZOPT = LFZOPT + 1
                  FZOPT(LFZOPT:LFZOPT) = 'D'
                  KOPT = 'D'
               ELSE
                  KOPT = ' '
               ENDIF
               ICSPAK = .TRUE.
               IOPTU  = 1
               CALL SBIT1(IHOWFA,JCSPFA)
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9011) CHHOST(1:LHOST),
     +            DSN(1:LDSN),KOPT
 9011 FORMAT(' FMOPEN. call XZOPEN for host = ',A,' file = ',A,
     +       ' options = ',A)
               CALL XZOPEN(LUN,DSN(1:LDSN),CHHOST(1:LHOST),
     +                     IQ(L+KOFUFA+MRLNFA)*4,KOPT,IRC)
            ENDIF
         ENDIF
#endif
#if defined(CERNLIB_IBMVM)
*
*     File format X, direct access
*
         IF((INDEX(CHFORM,'FX').NE.0).AND.(IOPTX.NE.0)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'C'
         ENDIF
#endif
 
         LRECL  = IQ(L+KOFUFA+MRLNFA)
#if defined(CERNLIB_VAXVMS)
*
*     Activate channel mode--> FMCFIO if STREAM_LF
*
         INQUIRE(FILE=DSN(1:LDSN),RECORDTYPE=RECTYP,EXIST=IEXIST)
         IF(INDEX(RECTYP,'STREAM_LF').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'C'
            IF(IOPTR.NE.0) CFMODE = 'r'
            IF(IOPTW.NE.0) CFMODE = 'w'
*
*     Medium
*
            IF(LFMODE(LUN).EQ.3) THEN
               MEDIUM = 1
            ELSE
               MEDIUM = 0
            ENDIF
            NBUF                  = 0
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9012) DSN(1:LDSN),
     +         MEDIUM,LRECL,CFMODE,NBUF
 9012 FORMAT(' FMOPEN. call CFOPEN for ',A,' medium = ',A,
     +       ' LRECL = ',I6,' mode = ',A,
     +       ' NBUF = ',I6)
            CALL CFOPEN(LUNPTR,MEDIUM,LRECL,CFMODE,NBUF,
     +                  DSN(1:LDSN),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9013) IRC,DSN(1:LDSN)
 9013 FORMAT(' FMOPEN. return code ',I6,
     +             ' from CFOPEN for ',A)
               GOTO 999
            ENDIF
            IFPNTR(LUN) = LUNPTR
            IOPTU       = 1
         ENDIF
 
#endif
#if defined(CERNLIB_UNIX)
         IF(ICFOP.EQ.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'C'
         ELSE
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT)  = 'L'
            IF(IOPTR.NE.0) CFMODE = 'r'
            IF(IOPTW.NE.0) CFMODE = 'w'
 
            MEDIUM                = 0
            NBUF                  = 1
 
            IF(LUN.LT.10) THEN
               WRITE(FORLUN,'(I1)') LUN
               LFLUN = 1
            ELSE
               WRITE(FORLUN,'(I2)') LUN
               LFLUN = 2
            ENDIF
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
*
*     Use full file name to activate SHIFT RFIO
*
            LDSN = LENOCC(SHFNAM)
            DSN  = SHFNAM(1:LDSN)
#endif
#if defined(CERNLIB_UNIX)
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9014) DSN(1:LDSN),
     +         MEDIUM,LRECL,CFMODE,NBUF
 9014 FORMAT(' FMOPEN. call CFOPEN for ',A,' medium = ',A,
     +       ' LRECL = ',I6,' mode = ',A,
     +       ' NBUF = ',I6)
            CALL CFOPEN(LUNPTR,MEDIUM,LRECL,CFMODE,NBUF,
     +                  DSN(1:LDSN),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9015) IRC,DSN(1:LDSN)
 9015 FORMAT(' FMOPEN. return code ',I6,' from CFOPEN for ',A)
               GOTO 999
            ENDIF
 
            IQUEST(1) = LUNPTR
 
            IOPTU     = 1
 
         ENDIF
#endif
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9016) LRECL,FZOPT(1:LFZOPT)
 9016 FORMAT(' FMOPEN. call FZFILE with LRECL = ',I6,
     +       ' CHOPT = ',A)
         CALL FZFILE(LUN,LRECL,FZOPT(1:LFZOPT))
         CALL FZLOGL(LUN,IDEBFA)
#if defined(CERNLIB_VAXVMS)
         IF(INDEX(RECTYP,'STREAM_LF').NE.0) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9017) LUN
 9017 FORMAT(' FMOPEN. call FZHOOK for LUN = ',I6)
            CALL FZHOOK(LUN,FMCFIO,DBUF)
         ENDIF
#endif
#if defined(CERNLIB_CSPACK)
         IF(ICSPAK) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9018) LUN
 9018 FORMAT(' FMOPEN. call FZHOOK for LUN = ',I6)
            CALL FZHOOK(LUN,FMXZIO,DBUF)
         ENDIF
#endif
#if defined(CERNLIB_IBMVM)
         IF((IOPTF.NE.0).AND.(IOPTX.NE.0).AND.
     +      (INDEX(CHFORM,'FX').NE.0)) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9019) LUN
 9019 FORMAT(' FMOPEN. call FZHOOK for LUN = ',I6)
            CALL FZHOOK(LUN,FMVMIO,DBUF)
         ENDIF
#endif
 
      ENDIF
 
      IF(IOPTU.EQ.0) THEN
*
*     Decide on file format
*
         IF((INDEX(CHFORM,'AS').NE.0).OR.(INDEX(CHFORM,'FA').NE.0)) THEN
            CHFRMT = 'FORMATTED'
         ELSE
            CHFRMT = 'UNFORMATTED'
         ENDIF
*
*     Decide on file status
*
         IF(IOPTW.NE.0) THEN
            CHSTAT = 'NEW'
         ELSE
            CHSTAT = 'OLD'
         ENDIF
*
*     For disk files, check if file already exists
*
         IF(IQ(L+KOFUFA+MMTPFA).EQ.1) THEN
#if defined(CERNLIB_IBMMVS)
            INQUIRE(FILE='/'//DSN(1:LDSN),EXIST=IEXIST)
#endif
#if defined(CERNLIB_IBMVM)
            INQUIRE(FILE=DDNAME,EXIST=IEXIST)
#endif
#if (!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_IBMVM))
            INQUIRE(FILE=DSN(1:LDSN),EXIST=IEXIST)
#endif
            IF(IEXIST) CHSTAT = 'OLD'
         ELSE
#if defined(CERNLIB_VAXVMS)
            FORLUN = 'FOR00N'
            WRITE(FORLUN(4:6),'(I3.3)') LUN
            INQUIRE(FILE=FORLUN,EXIST=IEXIST)
            IF(IEXIST) CHSTAT = 'OLD'
#endif
         ENDIF
 
#if defined(CERNLIB_IBMMVS)
*
*     Only FORTRAN I/O currently supported...
*
         IF(IQ(L+KOFUFA+MMTPFA).EQ.1) THEN
*
*     Check if file exists...
*
            INQUIRE(FILE='/'//DSN(1:LDSN),EXIST=IEXIST)
            IF(.NOT.IEXIST) THEN
*
*     Issue FILEINF for DCB and SPACE information
*
               IF(INDEX('TRK/BLK/CYL',CHSPAC(1:3)).EQ.0) THEN
                  IF(IDEBFA.GE.-1) WRITE(LPRTFA,9020) CHSPAC
 9020 FORMAT(' FMOPEN. invalid value (',A,
     +       ') given for SPACE parameter. TRK will be used')
                  CHSPAC = 'TRK '
               ENDIF
 
               MODEL = CHMGEN(1)
               LMOD  = LENOCC(MODEL)
 
               CALL FILEINF(IRC,'DEVICE',MODEL(1:LMOD),CHSPAC(1:3),
     +            ISPACE(2),'SECOND',ISPACE(3),'DIR',ISPACE(4),
     +            'RECFM',RECFM,'LRECL',IQ(L+KOFUFA+MRLNFA)*4,
     +            'BLKSIZE',IQ(L+KOFUFA+MBLNFA)*4)
            ENDIF
         ENDIF
 
         IF((CHFORM(1:2).EQ.'FX'.AND.ICFOP.EQ.3)
     +      .OR.(CHFORM(1:2).EQ.'EP').OR.
     +      (INDEX(CHLUN,'IOFILE').NE.0)) THEN
*
*       IOPACK (implied or explicit)
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9021)
 9021 FORMAT(' FMOPEN. FORTRAN open suppressed',
     +       ' (IOPACK implied or explicit)')
*
*       FORTRAN direct access
*
         ELSEIF((CHFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)
     +         .AND.(CHLUN(1:2).NE.'VM')) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9022)
 9022 FORMAT(' FMOPEN. FORTRAN D/A open...')
            OPEN(UNIT=LUN,
     +           FILE='/'//DSN(1:LDSN),ACTION=CHACT(1:LCHACT),
     +           ACCESS='DIRECT',STATUS=CHSTAT,RECL=LRECL*4,
     +           IOSTAT=IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9023) IRC,DSN(1:LDSN)
 9023 FORMAT(' FMOPEN. IOSTAT ',I6,' from OPEN for ',A)
               GOTO 999
            ENDIF
         ELSE
*
*       FORTRAN sequential I/O
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9024)
 9024 FORMAT(' FMOPEN. FORTRAN sequential open...')
            OPEN(UNIT=LUN,
     +           FILE='/'//DSN(1:LDSN),ACTION=CHACT(1:LCHACT),
     +           FORM=CHFRMT,STATUS=CHSTAT,
     +           IOSTAT=IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9023) IRC,DSN(1:LDSN)
               GOTO 999
            ENDIF
         ENDIF
#endif
#if defined(CERNLIB_IBMVM)
         IF((CHLUN(1:2).EQ.'VM').OR.(IVMIO)) THEN
*
*       VMIO
*
            FNAME  = ' '
            LREC1  = LRECL*4
            LBLK1  = LBLOCK*4
            RECFM1 = RECFM
            IF(IOPTR.NE.0) VMOPT = 'R'
            IF(IOPTW.NE.0) VMOPT = 'W'
*
*     VMOPT = U requires VMUPDT to write the data
*
*           IF((IOPTX.NE.0).AND.(IOPTW.NE.0)) VMOPT = 'U'
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9025) LUN,RECFM1,LREC1,
     +         LBLK1,VMOPT
 9025 FORMAT(' FMOPEN. call VMOPEN for input  dataset on unit ',I6,
     +       ' with DCB ',A,1X,I6,1X,I6,' VMOPT ',A)
            CALL VMOPEN(LUN,FNAME,VMOPT,RECFM1,LREC1,LBLK1,IRC,INFO)
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9026) LUN,RECFM1,LREC1,
     +         LBLK1,VMOPT
 9026 FORMAT(' FMOPEN. return from VMOPEN for unit ',I6,
     +       ' with DCB ',A,1X,I6,1X,I6,' VMOPT ',A)
            IF(IABS(IRC).GT.1) THEN
               IF(IDEBFA.GT.-3)
     +            WRITE(LPRTFA,9027) IRC,INFO
 9027 FORMAT(' FMOPEN. return code ',I6,
     +       ' from VMOPEN for input file, INFO = ',I6)
               GOTO 999
            ELSE
               IRC = 0
            ENDIF
#endif
#if defined(CERNLIB_IBMVM)
         ELSEIF((CHFORM(1:2).EQ.'FX').OR.(CHFORM(1:2).EQ.'EP').OR.
     +          (INDEX(CHLUN,'IOFILE').NE.0)) THEN
*
*       IOPACK (implied or explicit)
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9028)
 9028 FORMAT(' FMOPEN. FORTRAN open supressed -',
     +       ' (IOPACK explicit or implied)')
*
*       FORTRAN direct access
*
         ELSEIF((CHFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)
     +         .AND.(CHLUN(1:2).NE.'VM')) THEN
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9029)
 9029 FORMAT(' FMOPEN. FORTRAN D/A open...')
            OPEN (UNIT=LUN,ACCESS='DIRECT',STATUS=CHSTAT,RECL=LRECL*4,
     +            ACTION=CHACT(1:LCHACT),IOSTAT=IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9030) IRC
 9030 FORMAT(' FMOPEN. IOSTAT ',I6,' from OPEN')
               GOTO 999
            ENDIF
         ELSE
*
*       FORTRAN sequential I/O
*
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9031)
 9031 FORMAT(' FMOPEN. FORTRAN sequential open...')
            OPEN (UNIT=LUN,FORM=CHFRMT,STATUS=CHSTAT,
     +            ACTION=CHACT(1:LCHACT),IOSTAT=IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) WRITE(LPRTFA,9030) IRC
               GOTO 999
            ENDIF
         ENDIF
#endif
#if defined(CERNLIB_APOLLO)
         IF(IAPOL3.NE.0) THEN
            IF((IQ(L+KOFUFA+MMTPFA).EQ.1).OR.(IMODE.NE.0)) THEN
               CHFILE = DSN(1:LDSN)
               LCHF   = LDSN
            ELSEIF((IQ(L+KOFUFA+MMTPFA).GT.1).AND.(IMODE.EQ.0)) THEN
               CHFILE = STGPTH
               LCHF   = LENOCC(CHFILE)
            ENDIF
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9032) CHFILE(1:LCHF)
 9032 FORMAT(' FMOPEN. open file ',A)
         ENDIF
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_APOLLO))
*
*     Fix for compiler warnings on HP/UX, Cray etc.
*
         CHFILE = ' '
         LCHF   = 1
#endif
#if defined(CERNLIB_UNIX)
         IF((CHFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)) THEN
*SELF,IF=APOLLO,NORD,SUN,IBMRT,MACMPW,AIX370,IF=UNIX.
#endif
#if (!defined(CERNLIB_CONVEX))&&(!defined(CERNLIB_DECS))&&(!defined(CERNLIB_SGI))&&(defined(CERNLIB_UNIX))
            LREC=LRECL*4
#endif
#if (defined(CERNLIB_CONVEX)||defined(CERNLIB_DECS)||defined(CERNLIB_SGI))&&(defined(CERNLIB_UNIX))
            LREC=LRECL
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX))&&(defined(CERNLIB_UNIX))&&(!defined(CERNLIB_DOUBLE))
            LREC=LRECL*8
 
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9033)
 9033 FORMAT(' FMOPEN. FORTRAN D/A open...')
            IF(IAPOL3.EQ.0) THEN
               OPEN (UNIT=LUN,ACCESS='DIRECT',STATUS=CHSTAT,RECL=LREC,
     +               IOSTAT=IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9034) IRC
 9034 FORMAT(' FMOPEN. IOSTAT ',I6,' from OPEN')
                  GOTO 999
               ENDIF
            ELSE
               OPEN (UNIT=LUN,FILE=CHFILE(1:LCHF),
     +               ACCESS='DIRECT',STATUS=CHSTAT,RECL=LREC,
     +               IOSTAT=IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9034) IRC
                  GOTO 999
               ENDIF
            ENDIF
         ELSE
            IF(IDEBFA.GE.2) WRITE(LPRTFA,9035)
 9035 FORMAT(' FMOPEN. FORTRAN sequential open...')
            IF(IAPOL3.EQ.0) THEN
               OPEN (UNIT=LUN,FORM=CHFRMT,STATUS=CHSTAT,
     +               IOSTAT=IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9034) IRC
                  GOTO 999
               ENDIF
            ELSE
               OPEN (UNIT=LUN,FILE=CHFILE(1:LCHF),
     +               FORM=CHFRMT,STATUS=CHSTAT,
     +               IOSTAT=IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) WRITE(LPRTFA,9034) IRC
                  GOTO 999
               ENDIF
            ENDIF
         ENDIF
 
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
*
*     LRECL in bytes for SHIFT...
*
            LREC  = LRECL*4
            SHOPT = 'D'
         ELSE
            LREC  = LRECL*4
            SHOPT = ' '
         ENDIF
 
         IF(ICFOP.EQ.0) THEN
            CALL SBIT1(IHOWFA,JRFIFA)
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9036) LUN,LRECL,SHOPT
 9036 FORMAT(' FMOPEN. calling XYOPEN with LUN - ',I6,
     +       ' LRECL = ',I6,' CHOPT = ',A)
            CALL XYOPEN(LUN,LREC,SHOPT,IRC)
            IF(IRC.NE.0) THEN
               WRITE(LPRTFA,9037) IRC
 9037 FORMAT(' FMOPEN. return code ',I6,' from XYOPEN')
               GOTO 999
            ENDIF
 
            IF(IOPTF.NE.0) THEN
               IF(IDEBFA.GE.0) WRITE(LPRTFA,9038) LUN
 9038 FORMAT(' FMOPEN. call FZHOOK for LUN = ',I6)
               CALL FZHOOK(LUN,FMXYIO,DBUF)
            ENDIF
 
         ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
         IF(INDEX(RECTYP,'STREAM_LF').EQ.0.AND.
     +      INDEX(CHFORM,'FX').NE.0) THEN
*
*     Always open VAX files SHARED
*
            IF(IOPTR.NE.0) THEN
               IF((CHFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9039)
 9039 FORMAT(' FMOPEN. FORTRAN D/A open...')
                  OPEN (UNIT=LUN,STATUS=CHSTAT,
     +                 ACCESS='DIRECT',RECL=LRECL,
     +                 SHARED, READONLY,
     +               IOSTAT=IRC)
                  IF(IRC.NE.0) THEN
                     IF(IDEBFA.GE.-3) WRITE(LPRTFA,9040)
 9040 FORMAT(' FMOPEN. IOSTAT ',I6,' from OPEN')
                     GOTO 999
                  ENDIF
               ELSE
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9041)
 9041 FORMAT(' FMOPEN. FORTRAN sequential open...')
                  OPEN (UNIT=LUN, FORM=CHFRMT, STATUS=CHSTAT,
     +                  SHARED, READONLY,
     +               IOSTAT=IRC)
                  IF(IRC.NE.0) THEN
                     IF(IDEBFA.GE.-3) WRITE(LPRTFA,9040)
                     GOTO 999
                  ENDIF
               ENDIF
 
            ELSE
               IF((CHFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)) THEN
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9039)
                  OPEN (UNIT=LUN,STATUS=CHSTAT,
     +                  ACCESS='DIRECT',RECL=LRECL,
     +                  SHARED,
     +               IOSTAT=IRC)
                  IF(IRC.NE.0) THEN
                     IF(IDEBFA.GE.-3) WRITE(LPRTFA,9040)
                     GOTO 999
                  ENDIF
               ELSE
                  IF(IDEBFA.GE.2) WRITE(LPRTFA,9041)
                  OPEN (UNIT=LUN, FORM=CHFRMT, STATUS=CHSTAT,
     +                  SHARED,
     +               IOSTAT=IRC)
                  IF(IRC.NE.0) THEN
                     IF(IDEBFA.GE.-3) WRITE(LPRTFA,9040)
                     GOTO 999
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF(IDEBFA.GE.1) WRITE(LPRTFA,9042)
 9042 FORMAT(' FMOPEN. Fortran OPEN supressed for STREAM_LF files')
         ENDIF
#endif
      ENDIF
*
*
*     Issue RZOPEN and RZFILE
*
      IF(IOPTZ.NE.0) THEN
*
*     Mode for FMCLOS
*
         IFMODE(LUN) = 2
*
*     Build RZ options
*
         LRECL = IQ(L+KOFUFA+MRLNFA)
         LRECL = LRECL*4/IQCHAW
         RZOPT = 'W'
         IF(IOPTW.NE.0) RZOPT = 'UW'
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9043) LUN,CHDIR,
     +      DSN(1:LDSN),RZOPT,LRECL
 9043 FORMAT(' FMOPEN. call RZOPEN with LUN = ',I6,
     +       ' CHDIR = ',A,' DSN = ',A,' RZOPT = ',A,' LRECL = ',I6)
         CALL RZOPEN(LUN,CHDIR,DSN(1:LDSN),RZOPT,LRECL,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT 9044,IRC
 9044 FORMAT(' FMOPEN. return code ',I6,' from RZOPEN')
            GOTO 999
         ENDIF
 
         RZOPT = ' '
         IF(IOPTW.NE.0) RZOPT = 'ULD'
         IF(IOPT1.NE.0) RZOPT = '1ULD'
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9045) LUN,CHDIR,RZOPT
 9045 FORMAT(' FMOPEN. call RZFILE with LUN = ',I6,
     +       ' CHDIR = ',A,' RZOPT = ',A)
         CALL RZFILE(LUN,CHDIR,RZOPT)
         IRC = IQUEST(1)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9046) IRC
 9046 FORMAT(' FMOPEN. return code ',I6,' from RZFILE')
            GOTO 999
         ENDIF
      ENDIF
 
#if defined(CERNLIB_FPACK)
      ENDIF
#endif
 
   10 CONTINUE
*
*     Now compute elapsed time in seconds
*
         CALL DATIME(ID,IT)
         JDATE  = FMJDAT(IS(1),IS(2),IS(3))
         JSECS  = (IS(4)*60 + IS(5))*60 + IS(6)
 
         IF(IDATE.EQ.JDATE) THEN
            JTIME = JSECS - ISECS
         ELSE
            JTIME = (JDATE - IDATE)*86400 + JSECS
         ENDIF
*
*     Record last access date and use count in bank send to server
*
         CALL FMPKTM(ID,IT,IP,IRC)
         IQ(L+KOFUFA+MLATFA) = IP
         IF(IMODE.EQ.1) THEN
            IQ(L+KOFUFA+MUSCFA) = 1
         ELSE
           IQ(L+KOFUFA+MUSCFA) = IQ(L+KOFUFA+MUSCFA) + 1
         ENDIF
         IF(LUFZFA.GT.0.AND.IOPTQ.EQ.0) THEN
            IF(IDEBFA.GE.0.AND.IMODE.EQ.0) WRITE(LPRTFA,9047)
 9047 FORMAT(' FMOPEN. - updating last access date and use count')
            IF(IDEBFA.GE.3)
     +         CALL FMSHOW(GENAM(1:NCH),L,KEYS,'A',IRC)
*
*     Monitoring information
*
            ITIMFA = JTIME
            CALL FMREPL(GENAM(1:NCH),L,'O',IRC)
            IF((IRC.NE.0).AND.(IDEBFA.GE.0)) THEN
               WRITE(LPRTFA,9048) IRC
 9048 FORMAT(' FMOPEN. error updating usage information -',
     +       ' Return code from FMREPL = ',I6)
            ENDIF
         ENDIF
      IF(IDEBFA.GE.1) THEN
         CALL DATIME(ID,IT)
         WRITE(LPRTFA,9049) GENAM(1:NCH),CHOPT(1:LCH),ID,IT,IS(6)
 9049 FORMAT(' FMOPEN. exit for ',A,' options: ',A,' at ',
     +       I6.6,1X,I4.4,I2.2)
         WRITE(LPRTFA,9050) JTIME
 9050 FORMAT(' FMOPEN. elapsed time since entry = ',I6,' seconds')
      ENDIF
 
  999 END
