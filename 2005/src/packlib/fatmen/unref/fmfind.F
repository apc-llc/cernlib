*
* $Id: fmfind.F,v 1.1.1.1 1996/03/07 15:18:23 mclareni Exp $
*
* $Log: fmfind.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:23  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFIND(GENAME,CHLUN,LENTRY,IRC)
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/tmsdef.inc"
#include "fatmen/fatstg.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fatinfo.inc"
#include "zebra/zmach.inc"
#include "fatmen/fatvidp.inc"
      PARAMETER     (LKEYFA=10)
      DIMENSION     KEYS(LKEYFA)
      CHARACTER*8   ROUTIN,STATE
      CHARACTER*16  SHUSER,SHPOOL
      CHARACTER*12  FORMT
      CHARACTER*255 COMAND,SETUP,LABELDEF,FILEDEF,CHFILE
      CHARACTER*255 CHNFS,CHDSN
      CHARACTER*4   DEVTYP
      CHARACTER*6   VSN,VID,FSEQ
      CHARACTER*15  XVID
      CHARACTER*8   VIP
      CHARACTER*2   LABEL
      CHARACTER*6   CHRECL,CHBLK
      CHARACTER*6   VAXLAB(3)
      CHARACTER*2   IBMLAB(3)
      CHARACTER*(*) GENAME
      CHARACTER*8   HNAME,HTYPE,HSYS,HDISK
      CHARACTER*8   USER,ADDR
      CHARACTER*8   CHUSER
      CHARACTER*256 DSN
      CHARACTER*2   MODE
      CHARACTER*80  CHLINE
      CHARACTER*8   FORLUN
      INTEGER       FMHOST,FMUSER
      CHARACTER*5   IOMODE
      CHARACTER*4   FFORM,FTEMP
      CHARACTER*(*) CHLUN
      CHARACTER*6   CDEN
      CHARACTER*4   CSIZE
      CHARACTER*4   FZOPT,RZOPT,SHOPT
      CHARACTER*20  STGOPT
      CHARACTER*1   VMOPT
      CHARACTER*40  DCB
      CHARACTER*20  FNAME
      CHARACTER*4   RECFM1
      CHARACTER*4   RECFM
      CHARACTER*8   RING
      CHARACTER*8   DDNAME
      CHARACTER*1   DEVNAM
      CHARACTER*3   DEVNUM
      CHARACTER*12  CHDIR
#if defined(CERNLIB_UNIX)
      INTEGER       SYSTEMF
#endif
      LOGICAL       IWAIT
      DIMENSION     LENTRY(1)
      DATA          NENTRY/0/
      DATA          IBMLAB(1)/'SL'/,IBMLAB(2)/'NL'/IBMLAB(3)/'AL'/
      DATA          VAXLAB(1)/'EBCDIC'/,
     +              VAXLAB(2)/'NONE  '/,
     +              VAXLAB(3)/'ASCII '/
#include "fatmen/fatoptd.inc"
*
*     Set routine name
*
      ROUTIN = 'FMFIND. '
      NCH = LENOCC(GENAME)
      CALL VZERO(IOPT,36)
C======================== Modified by C. Onions =================
      IF (LENTRY(1) .EQ. 0) THEN
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9011) GENAME(1:NCH)
9011  FORMAT(' FMFIND. find ',A)
         CALL FMGET(GENAME,LENTRY,KEYS,IRC)
         IF (IRC.NE.0) RETURN
         ELSE
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9012) GENAME(1:NCH)
9012  FORMAT(' FMFIND. find ',A,' using user supplied bank')
      ENDIF
C======================== End of C. Onions modification =========
      IMODE = 0
      IOMODE  = '/IN  '
#if defined(CERNLIB_IBMVM)
      CALL FMONIT('FMFIND. '//GENAME(1:NCH))
#endif
      GOTO 10
 
10    CONTINUE
      L = LENTRY(1)
*
*     CHLUN can have the following formats:
*
*                                          nn
*                                          FTnnFlll
*                                          IOFILEnn
*                                          FORnnn
*                                          fort.nn
*
      LUN  = 0
      LCHLUN = LENOCC(CHLUN)
*     Dirty trick to satisfy Unix machines
      IF (LCHLUN .EQ. 1) THEN
         READ(CHLUN,1) LUN
         ELSEIF(LCHLUN .EQ. 2) THEN
         READ(CHLUN,2) LUN
         ENDIF
1     FORMAT(I1)
2     FORMAT(I2)
 
      IC = FMUSER(CHUSER)
 
#if defined(CERNLIB_SETUP)
      IF((IOPTT.EQ.0).AND.(IDEBFA.GE.0).AND.(NENTRY.EQ.0))
     +PRINT *,ROUTIN//'Tape staging has been disabled at this location'
      NENTRY = 1
      IOPTT  = 1
#endif
#if defined(CERNLIB_STAGE)
      IF((IOPTT.NE.0).AND.(IDEBFA.GE.0).AND.(NENTRY.EQ.0))
     +PRINT *,ROUTIN//'Tape staging is enforced at this location'
      NENTRY = 1
      IOPTT  = 0
#endif
      CALL CLTOU(GENAME)
      CALL UHTOC(IQ(L+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      LDSN = LENOCC(DSN)
      LBLANK = INDEX(DSN,' ')
      IF(LBLANK.NE.0) LDSN = LBLANK
#if defined(CERNLIB_UNIX)
*
*     If DSN starts with a $, assume that it is an environmental
*     variable.
      IF(DSN(1:1).EQ.'$') THEN
         LENV = INDEX(DSN(1:LDSN),'/')
         CALL GETENVF(DSN(2:LENV-1),CHNFS)
         IF(IS(1).GT.0) THEN
            CHDSN = CHNFS(1:IS(1)) // DSN(LENV:LDSN)
            LDSN  = LENOCC(DSN)
            DSN   = CHDSN(1:LDSN)
         ELSE
            IF(IDEBFA.GE.-3) PRINT *,'FMFIND. cannot translate ',
     +         ' environmental variable ',DSN(1:LENV-1)
         ENDIF
      ENDIF
#endif
*
*     Get DCB information
*
      CALL UHTOC(IQ(L+KOFUFA+MRFMFA),4,RECFM,4)
      LRECL  = IQ(L+KOFUFA+MRLNFA)*4
      LBLOCK = IQ(L+KOFUFA+MBLNFA)*4
*
*     FATMEN file format (for call to FZFILE,RZFILE)
*
      CALL UHTOC(IQ(L+KOFUFA+MFLFFA),4,FFORM,4)
*
*     New Zebra uses FORTRAN I/O as default...
*
      LFORM = LENOCC(FFORM)
      IF(FFORM(1:2).EQ.'FX') THEN
         FTEMP = FFORM
         FFORM = 'F'//FTEMP(1:LFORM)
      ENDIF
*
*     Find file and STAGE if necessary
*
*
*     Disk files ...
*
      IF (IQ(L+KOFUFA+MMTPFA) .EQ. 1) THEN
*
*     File is on disk. Check on Node etc. has been done in FMRZIN
*
         IC = FMHOST(HNAME,HTYPE,HSYS)
         CALL UHTOC(IQ(L+KOFUFA+MHSNFA),4,HDISK,8)
         LHOST  = LENOCC(HNAME)
 
#if defined(CERNLIB_IBMVM)
         COMAND = 'FILEDEF       FTnnF001 DISK '
         IF ((FFORM(1:2) .EQ. 'FX') .OR. (FFORM(1:2) .EQ. 'EP'))
     +   COMAND = 'FILEDEF       IOFILEnn     DISK '
         WRITE(COMAND(17:18),9001) LUN
         IF (COMAND(17:17) .EQ. ' ') COMAND(17:17) = '0'
         IF (LUN .EQ. 0) COMAND(15:22) = CHLUN
*
*     Get disk name and link to it
*
 
         LSTA = INDEX(DSN,'<')
         IF (LSTA .NE. 0) THEN
*
*     Format of DSN is <user.address>filename.filetype on VM
*
            LDOT = INDEX(DSN,'.')
            LBRA = INDEX(DSN,'>')
 
            IF ((LDOT .NE. 0) .AND. (LDOT .LE. LBRA)) THEN
               LEND = LDOT
               ELSE
               LEND = LBRA
               ENDIF
 
            USER = DSN(LSTA+1:LEND-1)
            LUSR = LEND - LSTA + 1
            ADDR = '    '
 
            IF ((LDOT .NE. 0) .AND. (LDOT .LE. LBRA)) THEN
              ADDR= DSN(LDOT+1:LBRA-1)
              ENDIF
 
            CALL VMCMS('EXEC GIME '//USER(1:LUSR)//ADDR//
     +      '(QUIET NONOTICE STACK)',IRC)
            IF(IRC.GT.4) THEN
                IF(IDEBFA.GE.0)
     +          PRINT *,ROUTIN//' return code from GIME = ',IRC
                RETURN
                ENDIF
 
            CALL VMRTRM(CHLINE,LENGTH)
            MODE = CHLINE(1:1)
*
*     Use mode 4 for all CMS files, except RECFM F
*     N.B. files in CMS format V will be incorrectly handled!
*     To be read correctly, RECFM=U
*
            IF (FFORM(1:2) .EQ. 'RZ')  THEN
               MODE(2:2) = '6'
               ELSE
               MODE(2:2) = '4'
               ENDIF
 
            IF (RECFM(1:1) .EQ. 'U') MODE(2:2) = '1'
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9015) ROUTIN,USER,ADDR,MODE
9015   FORMAT(1X,A8,'linked to ',A8,' address ',A3,' mode ',A4)
            ELSE
            MODE = '*'
            ENDIF
 
         LDOT = INDEXB(DSN,'.')
         DSN(LDOT:LDOT) = ' '
         COMAND = COMAND(1:30) // DSN(LBRA+1:LDSN) // ' ' // MODE
         LENCOM = LENOCC(COMAND)
         WRITE(DCB,8001) RECFM,LRECL,LBLOCK
*        IF(IMODE.NE.0) THEN
         COMAND = COMAND(1:LENOCC(COMAND)) // '(' // DCB
*           ENDIF
 
         LENCOM = LENOCC(COMAND)
         IF(IDEBFA.GE.0)
     +   PRINT *,ROUTIN//'running ',COMAND(1:LENCOM)
 
         CALL VMCMS(COMAND(1:LENCOM),IRC)
 
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Find disk with most space
*
      IF(IMODE.NE.0) CALL FMXDSK(DSN,IRC)
*
*     Just assign the relevant logical name...
*
      FORLUN = 'FOR00N'
      WRITE(FORLUN(4:6),9002) LUN
9002  FORMAT(I3)
      IF (FORLUN(4:4) .EQ. ' ') FORLUN(4:4) = '0'
      IF (FORLUN(5:5) .EQ. ' ') FORLUN(5:5) = '0'
      IF (LUN .EQ. 0) FORLUN = CHLUN
      IC = LIB$SET_LOGICAL(FORLUN(1:LENOCC(FORLUN)),
     +                     DSN(1:LDSN))
      IF (.NOT. IC) CALL LIB$SIGNAL(%VAL(STATUS))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'Assign ',DSN(1:LDSN),
     +                                   FORLUN(1:LENOCC(FORLUN))
#endif
#if defined(CERNLIB_UNIX)
*
*     Just issue the assign...
*
      FORLUN = 'fort.   '
      IF(LUN.LT.10) THEN
        WRITE(FORLUN(6:6),'(I1)') LUN
        ELSE
        WRITE(FORLUN(6:7),'(I2)') LUN
        ENDIF
 
      IF (LUN .EQ. 0) FORLUN = CHLUN
      LFLUN = LENOCC(FORLUN)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
      CALL CUTOL(DSN)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_APOLLO))
      IC = SYSTEMF('ln -sf '//DSN(1:LDSN)//' '
     +            //FORLUN(1:LFLUN))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'ln for logical unit ',
     +   FORLUN(1:LFLUN),' dsn = ',DSN(1:LDSN)
#endif
#if defined(CERNLIB_APOLLO)
      IC = SYSTEMF('ln -sf '//DSN(1:LDSN)//' '
     +            //FORLUN(1:LFLUN))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'ln for logical unit ',
     +   FORLUN(1:LFLUN),' dsn = ',DSN(1:LDSN)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CRAY))
      IC = SYSTEMF('assign -a '//DSN(1:LDSN)//' '
     +            //FORLUN(1:LFLUN))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' dsn = ',DSN(1:LDSN)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
*
*     Check if link already exists...
*
      INQUIRE(FILE=FORLUN(1:LFLUN),EXIST=ILINK)
      IF(ILINK) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMFIND. removing existing symbolic link...'
         IC = SYSTEMF('rm '//FORLUN(1:LFLUN))
      ENDIF
 
      IF(LUN.LT.10) THEN
         WRITE(FORLUN,'(I1)') LUN
      ELSE
         WRITE(FORLUN,'(I2)') LUN
      ENDIF
 
      LFLUN = LENOCC(FORLUN)
 
      CALL CTRANS('<','[',DSN,1,LDSN)
      CALL CTRANS('>',']',DSN,1,LDSN)
      ILSQB = INDEX(DSN(1:LDSN),'[')
      IRSQB = INDEX(DSN(1:LDSN),']')
 
 
      IF(ILSQB.NE.0) THEN
 
         IF(IDEBFA.GE.0) PRINT *,'FMFIND. SHIFT POOL file...'
 
         IDOT   = INDEX(DSN(1:IRSQB),'.')
         SHPOOL = DSN(2:IDOT-1)
         SHUSER = DSN(IDOT+1:IRSQB-1)
         ISTART = IRSQB+1
         IEND   = LDSN
      IF (IDEBFA.GE.0)    WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' pool = ',SHPOOL,
     +   ' user = ',SHUSER,' dsn = ',DSN(ISTART:IEND)
         IC = SYSTEMF('assign ` sfget -k -p '//SHPOOL//
     +               ' -u '//SHUSER// ' '//DSN(ISTART:IEND)//'  `
     +              '//FORLUN(1:LFLUN)//'  ')
         IF(IC.NE.0) THEN
            PRINT *,'FMFIND. return code ',IC,' from SFGET'
            RETURN
          ENDIF
      ELSE
 
      IF(IDEBFA.GE.0) PRINT *,'FMFIND. SHIFT private file...'
      IF (IDEBFA.GE.0)    WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' dsn = ',DSN(1:LDSN)
         IC = SYSTEMF('assign '//DSN(1:LDSN)//' '//
     +               FORLUN(1:LFLUN))
         IF(IC.NE.0) THEN
            PRINT *,'FMFIND. return code ',IC,' from SFGET'
            RETURN
          ENDIF
      ENDIF
 
#endif
*
*     Tape files ...
*
         ELSEIF(IQ(L+KOFUFA+MMTPFA) .GT. 1) THEN
 
         CDEN = CHMDEN(IQ(L+KOFUFA+MMTPFA))
*
*        "EXEC STAGE IN ddname vsn.fseq.label.vid"              (IBM)
*        "STAGE/IN vsn vid /NAME=/NUMB=/GENERIC=/LABEL= ddname" (VAX)
*        "stagein fort.lun -v vsn -V vid -l sl|nl|al|blp
*                          -g TAPE|CART|SMCF -d 6250|1600"      (CRAY)
*        "stagein -U unit -v vsn -V vid -l sl|nl|al|blp
*                          -g TAPE|CART|SMCF -d 6250|1600"      (SHIFT)
*
         CALL UHTOC(IQ(L+KOFUFA+MVSNFA),4,VSN,6)
         LVSN = LENOCC(VSN)
         CALL CLTOU(VSN)
         CALL UHTOC(IQ(L+KOFUFA+MVIDFA),4,VID,6)
         LVID = LENOCC(VID)
         CALL CLTOU(VID)
*
*        Generate eXtended VID - with VID prefix
*
         JP = IQ(L+KOFUFA+MVIPFA)
            IF(JP.NE.0) THEN
            LVIP  = LENOCC(PREVID(JP))
            VIP   = PREVID(JP)(1:LVIP)
            XVID  = PREVID(JP)(1:LENOCC(PREVID(JP)))
     +             // '.' // VID(1:LVID)
            LXVID = LENOCC(XVID)
 
            ELSE
            XVID  = VID
            LXVID = LVID
            LVIP  = 0
            ENDIF
 
 
         WRITE(FSEQ,9003) IQ(L+KOFUFA+MFSQFA)
9003     FORMAT(I6)
 
         JFSEQ = INDEXB(FSEQ,' ') + 1
*
*     File size, if zero take default size for current medium
*
         IF(IOPTO.EQ.0) ISIZE = IQ(L+KOFUFA+MFSZFA)
         IF(ISIZE .NE. 0) THEN
*
*     May need slightly more space on disk, due to VBS format!
*
            IFUDGE = MAX(2,ISIZE/15)
#if defined(CERNLIB_CERNVM)
*
*     Increase max. size by 10 MB (a few 210 MB staging disks exist...)
*
            WRITE(CSIZE,9004) MIN(ISIZE+IFUDGE,
     +                            MEDSIZ(IQ(L+KOFUFA+MMTPFA))+10)
#endif
#if !defined(CERNLIB_CERNVM)
            WRITE(CSIZE,9004) MIN(ISIZE+IFUDGE,
     +                            MEDSIZ(IQ(L+KOFUFA+MMTPFA)))
#endif
         ELSE
            WRITE(CSIZE,9004) MEDSIZ(IQ(L+KOFUFA+MMTPFA))
         ENDIF
9004        FORMAT(I4)
 
         IF(CHLUN(1:LCHLUN) .EQ. 'NOWAIT') THEN
            STGOPT = 'NOWAIT'
            IWAIT  = .FALSE.
         ELSE
            STGOPT = 'WAIT'
            IWAIT  = .TRUE.
         ENDIF
 
#if defined(CERNLIB_IBMVM)
C======================== Modified by C. Onions =================
         IF ((FFORM(1:2) .EQ. 'FX') .OR. (FFORM(1:2) .EQ. 'EP'))THEN
            COMAND = 'EXEC STAGE IN IOFILEnn   '
            WRITE(COMAND(21:22),9001) LUN
            IF (COMAND(21:21) .EQ. ' ') COMAND(21:21) = '0'
            IF (LUN .EQ. 0) COMAND(15:22) = CHLUN
         ELSE
            COMAND = 'EXEC STAGE IN FTnnF001   '
            WRITE(COMAND(17:18),9001) LUN
9001        FORMAT(I2)
            IF (COMAND(17:17) .EQ. ' ') COMAND(17:17) = '0'
            IF (LUN .EQ. 0) COMAND(15:22) = CHLUN
         ENDIF
*
*     Output staging?
*
         IF (IMODE .NE. 0) COMAND(12:13) = 'OU'
C======================== End of C. Onions modification =========
         IF(.NOT.IWAIT) COMAND(15:22) = 'FT00F001'
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(L+KOFUFA+MMTPFA)
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
         CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
         CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_IBMVM)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMFIND. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
 
         IF(MODEL(1:4).EQ.'3480') THEN
            IF(MNTTYP.EQ.'R') THEN
               MODEL = 'SMCF'
            ELSE
               MODEL = 'CT1 '
            ENDIF
         ENDIF
 
         IF(MODEL(1:4).EQ.'3420') MODEL = 'TAPE'
 
*
*     Believe density from TMS if tape is known
*
         IF(IC.EQ.0) CDEN = DENS
         CALL CLTOU(LABTYP)
         LLAB = LENOCC(LABTYP)
         IF(IMODE.EQ.1) STGOPT = 'AUTOPUT DELAY'
         COMAND = COMAND(1:25) // VSN(1:LVSN) // '.'
     +   // FSEQ(JFSEQ:LEN(FSEQ))
     +   // '.' // LABTYP(1:LLAB) // '.' // VID(1:LVID)
 
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
         IF(LVIP.NE.0) COMAND = COMAND(1:LENOCC(COMAND))
     +   // '.' // VIP(1:LVIP)
#endif
#if defined(CERNLIB_IBMVM)
         COMAND = COMAND(1:LENOCC(COMAND))
     +   // ' (' //STGOPT//' SIZE '//CSIZE // ' DEN '//CDEN
*
*     Specify dataset name only if option N not specified
*
           IF((LDSN.NE.0) .AND. (IOPTN.EQ.0))
     +       COMAND = COMAND(1:LENOCC(COMAND)) // ' DSN ' //DSN(1:LDSN)
 
*        IF((VID(1:1).EQ.'I') .AND. (IQ(L+KOFUFA+MMTPFA).EQ.2) .AND.
*    +      (ICNUM(VID,2,6).EQ.7)) THEN
         IF(LIB(1:4).EQ.'SMCF') THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // ' DEVTYPE SMCF'
            ENDIF
*
*     Output STAGing only - add DCB information (also NL tapes)
*
         IF((IMODE.NE.0).OR.(LABTYP(1:2).EQ.'NL')
     +      .OR.(IOPTL.NE.0))                    THEN
            WRITE(DCB,8001) RECFM,LRECL,LBLOCK
8001        FORMAT(' RECFM ',A4,' LRECL ',I5,' BLOCK ',I5)
            COMAND = COMAND(1:LENOCC(COMAND)) // DCB
            ENDIF
*
*     Output STAGing only - options Keep, autoPut
*
         IF((IMODE.NE.0).AND.(IOPTK.NE.0)) THEN
            COMAND = COMAND(1:LENOCC(COMMAND)) // ' KEEP'
         ENDIF
 
         IF((IMODE.NE.0).AND.(IOPTP.NE.0)) THEN
            COMAND = COMAND(1:LENOCC(COMMAND)) // ' AUTOPUT'
         ENDIF
 
         LENCOM = LENOCC(COMAND)
 
         IF(IOPTT.EQ.0) THEN
*
*        Use STAGE
*
20          CONTINUE
            IF(IDEBFA.GE.0) PRINT *,ROUTIN//'running ',COMAND(1:LENCOM)
 
            CALL VMCMS(COMAND(1:LENCOM),IRC)
*
*     NOWAIT specified - just return
*
            IF(.NOT.IWAIT) RETURN
 
            IF(IRC.EQ.0) THEN
*
*     Read mode only, if file size is currently zero, option S
*     specified, and DBS opened for write...
*
               IF((IOPTS.NE.0).AND.(LUFZFA.GT.0).AND.(IMODE.EQ.0)
     +            .AND.(IQ(L+KOFUFA+MFSZFA).EQ.0)) THEN
*
*     Build STAGE Query command
*
         COMAND = 'EXEC STAGE QUERY ' // VSN(1:LVSN) // '.'
     +   // FSEQ(JFSEQ:LEN(FSEQ))
     +   // '.' // LABTYP(1:LLAB) // '.' // VID(1:LVID)
     +   // ' (LIFO'
         CALL VMCMS(COMAND(1:LENOCC(COMAND)),IRC)
*
*     Get answer and extract file size
*
         CALL VMRTRM(CHLINE,LENGTH)
         ISLASH = INDEX(CHLINE,'/')
         IDOT   = INDEXB(CHLINE(1:ISLASH),'.')
         IBLANK = INDEXB(CHLINE(1:IDOT),' ')
         READ(CHLINE(IBLANK+1:IDOT-1),*) ISIZE
*
*     Add 1 MB to file size as we ignore the fraction...
*
         IQ(L+KOFUFA+MFSZFA) = ISIZE + 1
 
                  IF(IDEBFA.GE.0) THEN
         PRINT *,ROUTIN//'- updating file size from STAGE information'
         PRINT *,ROUTIN//CHLINE(1:LENOCC(CHLINE))
                     ENDIF
                  ENDIF
*
*     Option D - make a duplicate copy into the robot
*
               IF((IOPTD.NE.0).AND.(IMODE.EQ.0)) THEN
                  CALL FMSMCF(GENAME,L,IC)
                  IF(IC.NE.0) THEN
                     PRINT *,ROUTIN//'- return code ',IC,' from FMSMCF'
                     ENDIF
 
                  ENDIF
 
               ELSEIF(IRC.EQ.400) THEN
*
*     STAGE failed - cannot allocate disk size of size requested
*
                  READ(CSIZE,9004) ISIZE
                  IF(ISIZE.LT.MEDSIZ(IQ(L+KOFUFA+MMTPFA))) THEN
                    IF(IDEBFA.GE.0)
     +          PRINT *,ROUTIN//' unable to allocate staging disk for',
     +                          ' size ',CSIZE,' - will try larger disk'
*
*     Increase size by MAX of file size in FATMEN catalogue and 20MB
*     up to maximum size for this media type
*
                    ISIZE = MIN(ISIZE +
     +                      MAX(20,IQ(L+KOFUFA+MFSZFA)),
     +                      MEDSIZ(IQ(L+KOFUFA+MMTPFA)))
                    WRITE(CSIZE,9004) ISIZE
                    ISTART = INDEX(COMAND,'SIZE ') + 5
                    COMAND(ISTART:ISTART+3) = CSIZE
                    GOTO 20
                    ELSE
                    IF(IDEBFA.GE.0)
     +              PRINT *,ROUTIN//' return code from STAGE = ',IRC
                    RETURN
                    ENDIF
 
               ELSEIF(IRC.GE.20) THEN
 
                IF(IDEBFA.GE.0)
     +          PRINT *,ROUTIN//' return code from STAGE = ',IRC
                RETURN
 
               ENDIF
 
            ELSE
*
*        Use SETUP
*
            RING = ' NORING '
 
            CALL FMWORD(DDNAME,3,' ',COMAND,IRC)
            LDD = LENOCC(DDNAME)
            IF(LDD.LE.2) THEN
              READ(DDNAME(1:LDD),*) LUN
              DDNAME = 'FT00F001'
              WRITE(DDNAME(3:4),9001) LUN
              ENDIF
*
*     Find first free tape drive
*
            ITAPE = IUCOMP(0,IDEV,16)
            IF(ITAPE.EQ.17) THEN
               IF(IDEBFA.GE.-2) PRINT *,ROUTIN
     +         //' no free virtual address for tape unit'
               IRC = 1
               RETURN
               ENDIF
 
            ITEMP = 179 + ITAPE
            IF(ITAPE.GT.8) ITEMP = 277 + ITAPE
            WRITE(DEVNUM,'(I3)') ITEMP
            WRITE(DEVNAM,'(Z1)') ITAPE-1
 
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_HEPVM))
            SETUP = 'SETUP CT1  '//DEVNUM//' '
     +      //VSN(1:LVSN)//' VID '//XVID//' '
     +      //LABTYP//CDEN//RING
 
            IF(IOPTE.NE.0) THEN
               LENS  = LENOCC(SETUP)
               SETUP = SETUP(1:LENS)//' (END'
               ENDIF
 
            IF(LIB(1:4).EQ.'SMCF') SETUP(7:10) = 'SMCF'
 
            LENS = LENOCC(SETUP)
            IF(IDEBFA.GE.0) PRINT *,ROUTIN//'running ',SETUP(1:LENS)
            CALL VMCMS(SETUP(1:LENS),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,ROUTIN//'return code ',IRC,
     +                                 ' from SETUP'
               RETURN
            ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_VMTAPE))
 
            RING = ' READ   '
 
            SETUP = 'VMTAPE MOUNT '//VSN(1:LVSN)//DEVNAM
     +              //' DSN ? (DEN '//CDEN//' LABEL '//LABTYP
     +              //RING//' WAIT '//
            LENS = LENOCC(SETUP)
            IF(IDEBFA.GE.0) PRINT *,ROUTIN//'running ',SETUP(1:LENS)
            CALL VMCMS(SETUP(1:LENS),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,ROUTIN//'return code ',IRC,
     +                                 ' from VMTAPE'
               RETURN
            ENDIF
            CALL VMSTAK(DSN(1:LDSN),'L',IRC)
#endif
#if defined(CERNLIB_IBMVM)
 
            FILEDEF = 'FILEDEF '//DDNAME//' TAP'
     +      //DEVNAM//' '//LABTYP//' (DEN '//CDEN
            LENF = LENOCC(FILEDEF)
 
            IF(IOPTW.NE.0) THEN
               FILEDEF = FILEDEF(1:LENF) // DCB
               LENF = LENOCC(FILEDEF)
               ENDIF
 
            IF(IDEBFA.GE.0) PRINT *,ROUTIN//'running ',FILEDEF(1:LENF)
            CALL VMCMS(FILEDEF(1:LENF),IRC)
 
            IF((LDSN.GT.0).AND.(IOPTN.EQ.0)) THEN
               LABELDEF = 'LABELDEF '//DDNAME//' FID ?'
               LENL = LENOCC(LABELDEF)
 
               IF(IDEBFA.GE.0)
     +         PRINT *,ROUTIN//'running ',LABELDEF(1:LENL)
*
*     Put DSN on program STACK for LABELDEF
*
CSELF,IF=-QMIBMXA,IF=IBMVM.
C              CALL VMCMS('EXEC FATSTACK '//DSN(1:LDSN)//' (LIFO',IRC)
CSELF,IF=QMIBMXA,IF=IBMVM.
               IF(IDEBFA.GE.0)
     +         PRINT *,ROUTIN//'DSN is ',DSN(1:LDSN)
               CALL VMSTAK(DSN(1:LDSN),'L',IRC)
CSELF,IF=IBMVM.
               CALL VMCMS(LABELDEF(1:LENL),IRC)
 
               ENDIF
 
            ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Build the relevant logical name for STAGE
*
         FORLUN = 'FOR00N'
         WRITE(FORLUN(4:6),9002) LUN
         IF (FORLUN(4:4) .EQ. ' ') FORLUN(4:4) = '0'
         IF (FORLUN(5:5) .EQ. ' ') FORLUN(5:5) = '0'
         IF (LUN .EQ. 0) FORLUN = CHLUN
 
         JX = ICFNBL(CSIZE,1,4)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(L+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_PREFIX))
         CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_PREFIX))
         CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_VAXVMS)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMFIND. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
*
*     Translate IBM to VAX labels (SL->EBCDIC etc.)
*
         JL = ICNTH(LABTYP,IBMLAB,3)
         COMAND = '$STAGE ' // VSN(1:LVSN) // ' ' // VID(1:LVID) // ' '
     +            // FORLUN // IOMODE // '/NAME=' // DSN(1:LDSN)
     +            // '/NUMBER=' // FSEQ(JFSEQ:LEN(FSEQ))
     +            // '/SIZE='   // CSIZE(JX:4)
     +            // '/GENERIC='// MODEL
     +            // '/LABEL='//   VAXLAB(JL)
*
*     Output STAGing only - add DCB information (also NL tapes)
*
         IF((IMODE.NE.0).OR.(LABTYP(1:2).EQ.'NL')
     +      .OR.(IOPTL.NE.0))                    THEN
            IF(INDEX(RECFM,'F').NE.0) THEN
               COMAND = COMAND(1:LENOCC(COMAND)) // '/FIXED'
            ELSEIF(INDEX(RECFM,'V').NE.0) THEN
               COMAND = COMAND(1:LENOCC(COMAND)) // '/VARIABLE'
            ENDIF
            WRITE(CHRECL,'(I6.6)') LRECL
            WRITE(CHBLK, '(I6.6)') LBLOCK
            IF(LRECL.GT.0) COMAND = COMAND(1:LENOCC(COMAND)) //
     +        '/RECORDSIZE='//CHRECL
            IF(LBLOCK.GT.0) COMAND = COMAND(1:LENOCC(COMAND)) //
     +        '/BLOCKSIZE='//CHBLK
         ENDIF
*
         LENCOM = LENOCC(COMAND)
*
*        'T' option - read directly from tape
*
         IF(IOPTT.NE.0) THEN
           COMAND = COMAND(1:LENCOM) // '/DIRECT'
           LENCOM = LENCOM + 7
           ENDIF
 
         IF(IDEBFA.GE.0) PRINT *'FMFIND. running ',COMAND(1:LENCOM)
 
         IRC = LIB$SPAWN(COMAND(1:LENCOM))
         IF (.NOT. IRC) CALL LIB$SIGNAL(%VAL(IRC))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_APOL3))
         PRINT *,'FMFIND. Tape support is not available for this ',
     +           'system'
         IRC = 999
         RETURN
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
         IQUEST(11) = IQ(L+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3))&&(!defined(CERNLIB_PREFIX))
         CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3))&&(defined(CERNLIB_PREFIX))
         CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMFIND. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
 
         IF(MODEL(1:4).EQ.'3480') THEN
            IF(MNTTYP.EQ.'R') THEN
               MODEL = 'SMCF'
            ELSE
#endif
#if defined(CERNLIB_APOL3)
               MODEL = 'CART'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
               MODEL = 'CT1'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
            ENDIF
         ENDIF
 
         IF(MODEL(1:4).EQ.'3420') MODEL = 'TAPE'
*
*     Believe density from TMS if tape is known
*
         IF(IC.NE.0) CDEN = DENS
         CALL CUTOL(LABTYP)
         LLAB = LENOCC(LABTYP)
 
#endif
#if defined(CERNLIB_CRAY)
      FORLUN = 'fort.   '
      IF(LUN.LT.10) THEN
        WRITE(FORLUN(6:6),'(I1)') LUN
        ELSE
        WRITE(FORLUN(6:7),'(I2)') LUN
        ENDIF
 
#endif
#if defined(CERNLIB_SHIFT)
      IF(LUN.LT.10) THEN
        WRITE(FORLUN,'(I1)') LUN
        ELSE
        WRITE(FORLUN,'(I2)') LUN
        ENDIF
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
      WRITE(FSEQ,9003) IQ(L+KOFUFA+MFSQFA)
 
      IF (LUN .EQ. 0) FORLUN = CHLUN
*        "stagein fort.lun -v vsn -V vid -l sl|nl|al|blp
*                          -g TAPE|CART|SMCF -d 6250|1600"
#endif
#if defined(CERNLIB_APOL3)
        IF(IMODE.EQ.0) THEN
           COMAND = 'stage -i '
        ELSEIF(IMODE.EQ.1) THEN
           COMAND = 'stage -o '
        ENDIF
        COMAND = COMAND(1:LENOCC(COMAND))
     +           // ' -v '
     +           //VID(1:LVID)//' -l '//LABTYP//' -t '//MODEL
     +           // ' -f ' //FSEQ// ' -d '//CDEN // ' -s '//CSIZE
        IF(IWAIT) COMAND = COMAND(1:LENOCC(COMAND)) // ' -w '
#endif
#if defined(CERNLIB_CRAY)
        COMAND = 'stagein  '//FORLUN
#endif
#if defined(CERNLIB_SHIFT)
        COMAND = 'stagein  -G -U '//FORLUN
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
     +           // ' -v '//VSN(1:LVSN)// ' -V '
     +           //VID(1:LVID)//' -l '//LABTYP//' -g '//MODEL
     +           // ' -q ' //FSEQ(JFSEQ:LEN(FSEQ))
#endif
#if defined(CERNLIB_SHIFT)
     +           // ' -s ' //CSIZE // ' -u '//CHUSER
     +           // ' -p shift1'
#endif
#if defined(CERNLIB_CRAY)
     +           // ' -K -S sbin'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
 
      IF(IMODE.NE.0) COMAND(1:8) = 'stageout'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
      LENCOM = LENOCC(COMAND)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
*
*     Add DSN if IOPTN not specified
*
      IF(IOPTN.EQ.0) THEN
         COMAND = COMAND(1:LENCOM) // ' -f '//DSN(1:LDSN)
         LENCOM = LENOCC(COMAND)
         ENDIF
 
#endif
#if defined(CERNLIB_APOL3)
*
*     Add DSN if IOPTN not specified
*
      IF(IOPTN.EQ.0) THEN
         COMAND = COMAND(1:LENCOM) // ' -n '//DSN(1:LDSN)
         LENCOM = LENOCC(COMAND)
      ENDIF
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
*
*     Option T - direct access to tapes
*
      IF(IOPTT.NE.0) THEN
         COMAND(1:8) = 'setup   '
*
*     Option W - write access
*
         IF(IOPTW.NE.0) THEN
           COMAND = COMAND(1:LENCOM) // '-r in'
           LENCOM = LENCOM + 5
           ENDIF
 
         ENDIF
*
*     Add DCB information
*
            WRITE(DCB,8002) RECFM(1:1),LRECL,LBLOCK
8002        FORMAT(' -F ',A1,' -L ',I5,' -b ',I5)
            COMAND = COMAND(1:LENOCC(COMAND)) // DCB
            LENCOM = LENOCC(COMAND)
 
#endif
#if defined(CERNLIB_APOL3)
*
*     Add DCB information, direct output to temporary file
*
         CALL FMFNME(CHFILE)
         LCHF = LENOCC(CHFILE)
         IF(IDEBFA.GE.3) PRINT *,'FMFIND. output of STAGE command ',
     +      'will be sent to /tmp/'//CHFILE(1:LCHF)
         WRITE(DCB,8002) RECFM,LRECL,LBLOCK
8002     FORMAT(' -r ',A,' -c ',I5,' -b ',I5)
         COMAND = COMAND(1:LENOCC(COMAND)) // DCB
     +             // ' ' // VSN(1:LVSN)
     +            // ' > /tmp/'//CHFILE(1:LCHF)
         LENCOM = LENOCC(COMAND)
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
 
      CALL CSQMBL(COMAND,1,LENCOM)
      LENCOM = LENOCC(COMAND)
      IF(IDEBFA.GE.0) PRINT *,'FMFIND. executing ',COMAND(1:LENCOM)
#endif
#if defined(CERNLIB_APOL3)
      ISTAT = 0
100   CONTINUE
      IC =  SYSTEMF(COMAND(1:LENCOM))
*
*     Check output of stage command
*
      OPEN(LUN,FILE='/tmp/'//CHFILE(1:LCHF),STATUS='OLD',
     +     FORM='FORMATTED')
101   READ(LUN,'(A)',END=102) CHLINE
      LCHL = LENOCC(CHLINE)
      IF(IDEBFA.GE.0) PRINT *,'FMOPEN. ',CHLINE(1:LCHL)
      IF(INDEX(CHLINE(1:LCHL),'path : ').NE.0) THEN
         ISTART = INDEX(CHLINE(1:LCHL),'/')
         STGPTH = CHLINE(ISTART:)
      ELSEIF(INDEX(CHLINE(1:LCHL),'stat : ').NE.0) THEN
         ISTAT = 1
         IF(INDEX(CHLINE(1:LCHL),'ABORTED').NE.0) THEN
            IRC = -1
            RETURN
         ELSEIF(INDEX(CHLINE(1:LCHL),'ENDED_OK').NE.0) THEN
            GOTO 102
         ENDIF
      ENDIF
      GOTO 101
102   CLOSE(LUN)
      IF(ISTAT.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. no status return ',
     +      'from stage. Will retry in 60 seconds'
         CALL SLEEPF(60)
         GOTO 100
      ENDIF
103   CONTINUE
#endif
#if defined(CERNLIB_SHIFT)
      IC = SYSTEMF(COMAND(1:LENCOM))
      IF(IC.NE.0) THEN
         PRINT *,'FMFIND. return code ',IC,' from stage command'
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_CRAY)
      IC = SYSTEMF(COMAND(1:LENCOM))
#endif
 
         ENDIF
 
*
*     Record last access date and use count in bank send to server
*
      CALL DATIME(ID,IT)
      CALL FMPKTM(ID,IT,IP,IRC)
      IQ(L+KOFUFA+MLATFA) = IP
      IF(IMODE.EQ.1) THEN
        IQ(L+KOFUFA+MUSCFA) = 1
        ELSE
        IQ(L+KOFUFA+MUSCFA) = IQ(L+KOFUFA+MUSCFA) + 1
        ENDIF
      IF((LUFZFA.GT.0).AND.(IMODE.EQ.0))THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,ROUTIN//'- updating last access date and use count'
         IF(IDEBFA.GE.3)
     +   CALL FMSHOW(GENAME(1:NCH),L,KEYS,'A',IRC)
         CALL FMMOD(GENAME(1:NCH),L,0,IRC)
         IF((IRC.NE.0).AND.(IDEBFA.GE.0)) THEN
            PRINT *,ROUTIN//
     +'- error updating use count/last access date'
            PRINT *,'Return code from FMMOD = ',IRC
            ENDIF
            ENDIF
 
 
      END
