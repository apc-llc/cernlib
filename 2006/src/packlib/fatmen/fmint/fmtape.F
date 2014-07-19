*
* $Id: fmtape.F,v 1.3 1997/05/28 16:24:33 jamie Exp $
*
* $Log: fmtape.F,v $
* Revision 1.3  1997/05/28 16:24:33  jamie
* use I6 for lrecl and lblk
*
* Revision 1.2  1997/01/07 16:06:32  jamie
* raise 200MB stage limit to 2GB
*
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMTAPE(GENAM,LBANK,KEYS,CHLUN,CHOPT,IRC)
*
      CHARACTER*(*) GENAM,CHLUN,CHOPT
#include "fatmen/fatget.inc"
#include "fatmen/fatmon.inc"
#include "fatmen/fmshft.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatstg.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fatinfo.inc"
#include "zebra/zmach.inc"
#include "fatmen/slate.inc"
#include "fatmen/fabalq.inc"
      PARAMETER     (MEGA=1024*1024)
      PARAMETER     (LKEYFA=10)
#if defined(CERNLIB_CERNVM)
      PARAMETER     (MAXSTG=210)
#endif
#if defined(CERNLIB_DSYIBM)
      PARAMETER     (MAXSTG=250)
#endif
#if (!defined(CERNLIB_CERNVM))&&(!defined(CERNLIB_DSYIBM))
      PARAMETER     (MAXSTG=2048)
#endif
#if defined(CERNLIB_IBMMVS)
      CHARACTER*8   CHFUNC,DDNAME
      CHARACTER*20  CHLEVL
      PARAMETER     (MODEFT=1)
#endif
      DIMENSION     KEYS(LKEYFA)
#if defined(CERNLIB_APOLLO)
#include "fatmen/fatapol3.inc"
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*10  CHCODE
      CHARACTER*8   CHSERV
      CHARACTER*255 EQUNAM
      CHARACTER*255 CHGRP,CHSTFL
      INCLUDE       '($DVIDEF)'
      INCLUDE       '($RMSDEF)'
      INTEGER       FAFNDF
#include "fatmen/fmqued.inc"
#endif
#if defined(CERNLIB_IBMMVS)
*
*     For tape files, a DD statement of the form
*     //FTnnFffff DD UNIT(=model,,DEFER),VOL=PRIVATE
*     is required.
*
      DIMENSION     HDISP(3),HVOL(2),HLAB(3),HDCB(4),HUNIT(2)
#endif
#if defined(CERNLIB_IBMMVS)
#include "fatmen/fatdcb.inc"
#endif
      CHARACTER*64  CHOPEN
      CHARACTER*8   CHUSER,CHPOOL
      CHARACTER*255 CHCOMM,SETUP,LABELDEF,FILEDEF,CHFILE,L3PATH
      CHARACTER*255 CHDSN
      CHARACTER*4   DEVTYP
      CHARACTER*6   VSN,VID,FSEQ
      CHARACTER*15  XVID
      CHARACTER*8   VIP
      CHARACTER*6   CHREC,CHBLK
      CHARACTER*80  CHLINE
      CHARACTER*8   FORLUN
      INTEGER       FMUSER,FMNODE
      CHARACTER*5   IOMODE
      CHARACTER*4   FFORM,FTEMP
      CHARACTER*6   CDEN
      CHARACTER*4   CSIZE
      CHARACTER*20  STGOPT
      CHARACTER*40  DCB
      CHARACTER*20  FNAME
      CHARACTER*4   RECFM1
      CHARACTER*4   RECFM
      CHARACTER*1   DEVNAM
      CHARACTER*3   DEVNUM
      LOGICAL       IWAIT,ILINK,IEXIST
      INTEGER       SYSTEMF
#if defined(CERNLIB_IBMVM)
      CHARACTER*8   RING,DDNAME
      CHARACTER*8   CHACC
      CHARACTER*2   CHUNIT
      CHARACTER*1   CHDISK,CMXDSK
#endif
      CHARACTER*8   CHHOST,CHTYPE,CHSYS
      INTEGER       FMHOST
#if defined(CERNLIB_VAXVMS)
      EXTERNAL      FMBALQ
#endif
#if defined(CERNLIB_VAXVMS)
#include "fatmen/fatlab0.inc"
#endif
#include "fatmen/tmsdef0.inc"
#include "fatmen/fatopt0.inc"
#include "fatmen/fatvidp.inc"
#if defined(CERNLIB_VAXVMS)
#include "fatmen/fatlab1.inc"
#endif
#include "fatmen/tmsdef1.inc"
#include "fatmen/fatopt1.inc"
#if defined(CERNLIB_IBMMVS)
      DATA          HDISP(2)/4HKEEP/,HDISP(3)/4HKEEP/
#endif
      DATA          NENTRY/0/
#include "fatmen/fatoptc.inc"
 
      IF(IDEBFA.GE.1) THEN
         CALL DATIME(ID,IT)
         PRINT 9001,ID,IT,IS(6)
 9001 FORMAT(' FMTAPE. enter at ',I6.6,1X,I4.4,I2.2)
      ENDIF
 
      IRC   = 0
      NCH   = LENOCC(GENAM)
 
      ICODE = FMHOST(CHHOST,CHTYPE,CHSYS)
 
#if (defined(CERNLIB_SHIFT))&&(defined(CERNLIB_OLDDPM))
      CHPOOL = 'shift1'
#endif
#if (defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_OLDDPM))
      CHPOOL = ' '
#endif
*
*     Take file size from IQUEST vector, if option O is specified
*
      IF(IOPTO.NE.0) ISIZE = ISIZSG
*
*     Set mode (read/write)
*
      IMODE  = IOPTW
      IOMODE  = '/IN '
      IF(IMODE.NE.0) IOMODE  = '/OUT '
 
      LUN    = 0
      LCHLUN = LENOCC(CHLUN)
 
      LUN    = ICDECI(CHLUN,1,LCHLUN)
*
*     Get LUN from CHLUN (DDNAME) if necessary
*
      IF(LUN.EQ.0) CALL FMDD2L(CHLUN(1:LCHLUN),LUN,IRC)
*
*     Get CHDSN
*
      CALL FMGDSN(LBANK,CHDSN,LDSN,IRC)
 
      IC = FMUSER(CHUSER)
      LUSER = LENOCC(CHUSER)
 
#if defined(CERNLIB_VAXVMS)
*
*     Get directory for writing stage requests
*
      IF(NENTRY.EQ.0) THEN
         CALL FMGTLG('STAGE_QUEUE',CHQUED,
     +               'LNM$SYSTEM',ISTAT)
         LQUED = IS(1)
         IF(IDEBFA.GE.0) THEN
            IF(LQUED.EQ.0) THEN
               WRITE(LPRTFA,9002)
 9002 FORMAT(' FMTAPE. directory for queuing stage requests is',
     +       ' not defined.')
               IF(IOPTQ.NE.0) PRINT *,'FMTAPE. option Q ignored.'
            ELSE
               WRITE(LPRTFA,9003) CHQUED(1:LQUED)
 9003 FORMAT(' FMTAPE. directory for queuing stage requests is ',A)
            ENDIF
         ENDIF
      ENDIF
 
#endif
#if defined(CERNLIB_SETUP)||defined(CERNLIB_TAPESYS)
      IF((IOPTT.EQ.0).AND.(IDEBFA.GE.0).AND.(NENTRY.EQ.0))
     +PRINT *,'FMTAPE. Tape staging has been disabled at this location'
      NENTRY = 1
      IOPTT  = 1
#endif
#if defined(CERNLIB_STAGE)
      IF((IOPTT.NE.0).AND.(IDEBFA.GE.0).AND.(NENTRY.EQ.0))
     +PRINT *,'FMTAPE. Tape staging is enforced at this location'
      NENTRY = 1
      IOPTT  = 0
#endif
 
      IF(IOPTT.NE.0) CALL SBIT1(IHOWFA,JTPMFA)
*
*     I/O options for FZ:
*         IQUEST(10) = 1 - Use C I/O in FZ
*         IQUEST(10) = 2 - Use FORTRAN D/A I/O
*         IQUEST(10) = 3 - Use 'package' I/O in FZ (e.g. IOPACK)
*
      ICFOP = 0
      IF(INDEX(FFORM,'FP').EQ.0.AND.IOPTF.NE.0) THEN
         ICFOP = IQUEST(10)
         IF(ICFOP.EQ.2) IOPTX = 1
      ENDIF
*
*     RZ files: create staged file with ACCESS=DIRECT
*
      CALL UHTOC(IQ(LBANK+KOFUFA+MFLFFA),4,FFORM,4)
      IF(INDEX(FFORM,'RZ').NE.0.OR.INDEX(FFORM,'RX').NE.0) THEN
         IOPTX = 1
      ENDIF
*
*     Get DCB information
*
      CALL UHTOC(IQ(LBANK+KOFUFA+MRFMFA),4,RECFM,4)
      LRECL  = IQ(LBANK+KOFUFA+MRLNFA)*4
      LBLOCK = IQ(LBANK+KOFUFA+MBLNFA)*4
*
*     Find file and STAGE if necessary
*
 
      CDEN = CHMDEN(IQ(LBANK+KOFUFA+MMTPFA))
*
*     "EXEC STAGE IN ddname vsn.fseq.label.vid"              (IBM)
*     "STAGE/IN vsn vid /NAME=/NUMB=/GENERIC=/LABEL= ddname" (VAX)
*     "stagein fort.lun -v vsn -V vid -l sl|nl|al|blp
*                       -g TAPE|CART|SMCF -d 6250|1600"      (CRAY)
*     "stagein -U unit -v vsn -V vid -l sl|nl|al|blp
*                       -g TAPE|CART|SMCF -d 6250|1600"      (SHIFT)
*
      CALL UHTOC(IQ(LBANK+KOFUFA+MVSNFA),4,VSN,6)
      LVSN = LENOCC(VSN)
      CALL CLTOU(VSN)
      CALL UHTOC(IQ(LBANK+KOFUFA+MVIDFA),4,VID,6)
      LVID = LENOCC(VID)
      CALL CLTOU(VID)
*
*        Generate eXtended VID - with VID prefix
*
      JP = IQ(LBANK+KOFUFA+MVIPFA)
      IF(JP.NE.0) THEN
         LVIP  = LENOCC(PREVID(JP))
         VIP   = PREVID(JP)(1:LVIP)
         XVID  = PREVID(JP)(1:LENOCC(PREVID(JP)))
     +          // '.' // VID(1:LVID)
         LXVID = LENOCC(XVID)
 
      ELSE
         XVID  = VID
         LXVID = LVID
         LVIP  = 0
      ENDIF
 
      CALL FMITOC(IQ(LBANK+KOFUFA+MFSQFA),FSEQ,LFSEQ)
*
*     File size, if zero take default size for current medium
*
      IF(IOPTO.EQ.0) ISIZE = IQ(LBANK+KOFUFA+MFSZFA)
      IF(ISIZE.NE.0) THEN
         IF(ISIZE.GT.MAXSTG) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. Warning - ',
     +         'staging disks are limited to ',MAXSTG,
     +         ' MB on this system'
         ENDIF
#if defined(CERNLIB_IBM)
*
*     May need slightly more space on disk, due to VBS format!
*
         IFUDGE = MAX(2,ISIZE/15)
#endif
#if !defined(CERNLIB_IBM)
         IFUDGE = 0
#endif
         CALL FMITOC(MIN(ISIZE+IFUDGE,MAXSTG),CSIZE,LCSIZE)
      ELSE
         CALL FMITOC(MIN(MEDSIZ(IQ(LBANK+KOFUFA+MMTPFA)),MAXSTG),
     +               CSIZE,LCSIZE)
      ENDIF
 
      IF(CHLUN(1:LCHLUN).EQ.'NOWAIT'.OR.IOPTQ.NE.0) THEN
         STGOPT = 'NOWAIT'
         IWAIT  = .FALSE.
      ELSE
         STGOPT = 'WAIT'
         IWAIT  = .TRUE.
      ENDIF
 
#if defined(CERNLIB_IBMMVS)
*=======================================================================
*     I B M   M V S
*=======================================================================
*
*     Build arguments for call to FTDD...
*
*
      CALL UCTOH(CHDSN(1:LDSN)//' ',HDSN(1),4,LDSN+1)
*
*      Check if file is catalogued
*
      INQUIRE(FILE='/'//CHDSN(1:LDSN),EXIST=IEXIST)
      IF(.NOT.IEXIST) THEN
*
*      Get media details
*
         CALL FMQVOL(GENAM(1:NCH),L,KEYS,
     +               LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
         CALL UHTOC(IQ(LBANK+KOFUFA+MVSNFA),4,VSN,6)
         LVSN = LENOCC(VSN)
         CALL CLTOU(VSN)
 
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMTAPE. return from FMQVOL with ',
     +              LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
         LLAB = LENOCC(LABTYP)
         LMOD = LENOCC(MODEL)
         CALL UCTOH('NEW ',HDISP(1),4,4)
         CALL UCTOH(VSN(1:LVSN),HVOL,4,LVSN)
         CALL UCTOH(MODEL,HUNIT(1),4,LMOD)
         HLAB(1) = IQ(LBANK+KOFUFA+MFSQFA)
         CALL UCTOH(LABTYP,HLAB(2),4,LLAB)
         IF(IOPTW.NE.0) THEN
            CALL UCTOH('OUT ',HLAB(3),4,4)
         ELSE
            CALL UCTOH('IN  ',HLAB(3),4,4)
         ENDIF
         HDCB(2) = IQ(LBANK+KOFUFA+MRLNFA)*4
         HDCB(3) = IQ(LBANK+KOFUFA+MBLNFA)*4
         HDCB(4) = MEDDEN(IQ(LBANK+KOFUFA+MMTPFA))
         CALL FTDD(LUN,MODEFT,HDSN,HDISP,HVOL,HLAB,HDCB,HUNIT,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. return code ',IRC,
     +         ' from FTDD'
            GOTO 80
         ENDIF
      ELSE
*
*     Stage?
*
         IF(IOPTT.EQ.0) THEN
*
*     Build parameter list
*
            DDNAME = CHLUN(1:LCHLUN)
            LDD    = LCHLUN
 
            IF(IDEBFA.GE.3) THEN
               CHLEVL = 'PRINTLEV=ALL       ;'
            ELSEIF(IDEBFA.EQ.2) THEN
               CHLEVL = 'PRINTLEV=TRACE     ;'
            ELSEIF(IDEBFA.EQ.1) THEN
               CHLEVL = 'PRINTLEV=WARNING   ;'
            ELSEIF(IDEBFA.LE.-3) THEN
               CHLEVL = 'PRINTLEV=NOMESSAGE ;'
            ELSE
               CHLEVL = 'PRINTLEV=ERROR     ;'
            ENDIF
 
            CHFUNC = 'INPUT   '
            CALL STAGE(CHFUNC,IRC,ICODE,
     +                 'DSN='//CHDSN(1:LDSN)//' ;',
     +                 'DDNAME='//DDNAME(1:LDD)//' ;',
     +                 CHLEVL,
     +                 'SPACE='//CSIZE(1:LCSIZE)//' ;')
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. return code ',IRC,
     +            ' from STAGE, reason code = ',ICODE
               GOTO 80
            ENDIF
         ELSE
*
*     Just set DISP=OLD
*
            CALL UCTOH('OLD ',HDISP(1),4,4)
            CALL FTDD(LUN,MODEFT,HDSN,HDISP,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. return code ',IRC,
     +            ' from FTDD'
               GOTO 80
            ENDIF
         ENDIF
      ENDIF
*=======================================================================
*     end I B M   M V S
*=======================================================================
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_NEEDFILE))
*=======================================================================
*     I B M   V M
*=======================================================================
*
*     Interface to FNAL NEEDFILE exec
*
      CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
      IF(IDEBFA.GE.3) PRINT *,'FMTAPE. return from FMQTMS with ',
     +           VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +           LABTYP,'/',IC
*
      IF(CHLUN(1:LCHLUN) .EQ. 'NOWAIT'.OR.IOPTQ.NE.0) THEN
         STGOPT = 'NOREPLY'
         IWAIT  = .FALSE.
      ELSE
         STGOPT = 'WAIT'
         IWAIT  = .TRUE.
         WRITE(CHUNIT,'(I2.2)') LUN
      ENDIF
 
      CHCOMM = 'EXEC NEEDFILE '//VSN(1:LVSN) //
     +         ' ( UNIT '//CHUNIT//' '//MODEL//' '//STGOPT
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_NEEDFILE))
 
      IF((FFORM(1:2).EQ.'FX'.AND.IOPTX.EQ.0.AND.ICFOP.EQ.3)
     +   .OR.(FFORM(1:2).EQ.'EP')) THEN
         CHCOMM = 'EXEC STAGE IN IOFILEnn   '
         WRITE(CHCOMM(21:22),'(I2.2)') LUN
      ELSEIF((FFORM(1:2).EQ.'FX').AND.(IOPTX.NE.0))THEN
         CHCOMM = 'EXEC STAGE IN VMnnF001   '
         WRITE(CHCOMM(17:18),'(I2.2)') LUN
      ELSE
         CHCOMM = 'EXEC STAGE IN FTnnF001   '
         WRITE(CHCOMM(17:18),'(I2.2)') LUN
      ENDIF
 
      IF(LCHLUN.GT.2) CHCOMM(15:22) = CHLUN
*
*     Output staging?
*
      IF(IMODE.NE.0) THEN
         CHCOMM(12:13) = 'OU'
*
*     Check DCB information
*
         IF(LRECL.EQ.0.OR.LBLOCK.EQ.0.OR.RECFM(1:1).EQ.' ') THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. DCB information ',
     +         'missing or invalid'
            IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. lrecl = ',LRECL,
     +         ' blocksize = ',LBLOCK,' recfm = ',RECFM
            IRC = 27
            GOTO 80
         ENDIF
 
      ENDIF
 
      IF(.NOT.IWAIT) CHCOMM(15:22) = 'FT00F001'
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
      IQUEST(11) = IQ(LBANK+KOFUFA+MMTPFA)
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
      CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
      CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_IBMVM)
      IF(IDEBFA.GE.3) THEN
         PRINT *,'FMTAPE. return from FMQTMS with ',
     +           VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +           LABTYP,'/',IC
      ENDIF
*
*     Believe density from TMS if tape is known
*
      IF(IC.EQ.0) CDEN = DENS
      CALL CLTOU(LABTYP)
      LLAB = LENOCC(LABTYP)
*
*     Input wHole tape staging
*
      IF(IMODE.EQ.0.AND.IOPTH.NE.0) THEN
         FSEQ   = '1-E'
         LFSEQ  = 3
         STGOPT = 'ONEDISK'
*
*     Get volinfo information
*
         CALL FMVINF(VID(1:LVID),MB,NFILES,'G',ICODE)
         IF(MB.EQ.0) THEN
            CALL FMITOC(MIN(MEDSIZ(IQ(LBANK+KOFUFA+MMTPFA)),MAXSTG),
     +                  CSIZE,LCSIZE)
         ELSE
            CALL FMITOC(MB,CSIZE,LCSIZE)
         ENDIF
      ENDIF
 
      IF(IMODE.EQ.1) STGOPT = 'AUTOPUT DELAY'
      CHCOMM = CHCOMM(1:25) // VSN(1:LVSN) // '.'
     +   // FSEQ(1:LFSEQ)
     +   // '.' // LABTYP(1:LLAB) // '.' // VID(1:LVID)
 
#endif
#if (defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM))
      IF(LVIP.NE.0) CHCOMM = CHCOMM(1:LENOCC(CHCOMM))
     +   // '.' // VIP(1:LVIP)
#endif
#if defined(CERNLIB_IBMVM)
      CHCOMM = CHCOMM(1:LENOCC(CHCOMM))
     +   // ' (' //STGOPT//' SIZE '//CSIZE // ' DEN '//CDEN
*
*     Specify dataset name only if option N not specified
*
      IF(LDSN.NE.0.AND.IOPTN.EQ.0)
     +   CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' DSN ' //CHDSN(1:LDSN)
 
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_TMS))
         CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' DEVTYPE '//MODEL
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_CERN))&&(defined(CERNLIB_TMS))
         CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' DEVTYPE '//MODEL
#endif
#if defined(CERNLIB_IBMVM)
*
*     Output STAGing only - add DCB information (also NL tapes)
*
      IF(IMODE.NE.0.OR.LABTYP(1:2).EQ.'NL'.OR.IOPTL.NE.0) THEN
         WRITE(DCB,9004) RECFM,LRECL,LBLOCK
 9004 FORMAT(' RECFM ',A4,' LRECL ',I5,' BLOCK ',I5)
         CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // DCB
      ENDIF
*
*     Output STAGing only - options Keep, autoPut
*
      IF(IMODE.NE.0) THEN
         IF(IOPTK.NE.0) CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' KEEP'
         IF(IOPTP.NE.0) CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' AUTOPUT'
      ENDIF
 
      LENCOM = LENOCC(CHCOMM)
 
      CALL CSQMBL(CHCOMM,1,LENCOM)
      LENCOM = IS(1)
 
      IF(IOPTT.EQ.0) THEN
*
*     Use STAGE
*
         IF(IOPTY.NE.0) THEN
            CHDISK = CMXDSK()
            CHOPEN = '/'//CHUSER(1:LUSER) // ' FMSTAGE ' // CHDISK
            LOPEN  = LUSER + 11
            IF(IDEBFA.GE.1) PRINT *,'FMTAPE. writing STAGE command to ',
     +         CHOPEN(1:LOPEN)
            OPEN(LUN,STATUS='UNKNOWN',ACCESS='SEQUENTIAL',IOSTAT=IRC,
     +           FILE=CHOPEN(1:LOPEN),
     +           ACTION='READWRITE',FORM='FORMATTED')
            IF(IRC.NE.0) GOTO 80
            WRITE(LUN,'(A)',IOSTAT=IRC) CHCOMM(1:LENCOM)
            IF(IRC.NE.0) GOTO 80
            CLOSE(LUN)
            GOTO 80
         ENDIF
   10 CONTINUE
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',CHCOMM(1:LENCOM)
 
         CALL VMCMS(CHCOMM(1:LENCOM),IRC)
*
*     NOWAIT specified - just return
*
         IF(.NOT.IWAIT) GOTO 80
 
*
*     If option S specified and file size currently zero OR
*        option V                                        AND
*        read mode and data base opened for write...
*
         IF(IRC.EQ.0) THEN
            IF(((IOPTS.NE.0.AND.IQ(LBANK+KOFUFA+MFSZFA).EQ.0)
     +          .OR.IOPTV.NE.0)
     +          .AND.(LUFZFA.GT.0.AND.IMODE.EQ.0)) THEN
*
*     Build STAGE Query command
*
               CHCOMM = 'EXEC STAGE QUERY ' // VSN(1:LVSN) // '.'
     +                  // FSEQ(1:LFSEQ) // '.' // LABTYP(1:LLAB)
     +                  // '.' // VID(1:LVID) // ' (LIFO'
               LC     = LENOCC(CHCOMM)
               IF(IDEBFA.GE.2) PRINT *,'FMTAPE. running ',
     +            CHCOMM(1:LC)
               CALL VMCMS(CHCOMM(1:LC),IRC)
*
*     Get answer and extract file size
*
               CALL VMRTRM(CHLINE,LENGTH)
               IF(IDEBFA.GE.2) PRINT *,'FMTAPE. reply from VMSTAGE: ',
     +            CHLINE(1:LENGTH)
               ISLASH = INDEX(CHLINE(1:LENGTH),'/')
               IF(ISLASH.EQ.0) GOTO 20
               IDOT   = INDEXB(CHLINE(1:ISLASH),'.')
               IBLANK = INDEXB(CHLINE(1:IDOT),' ')
               READ(CHLINE(IBLANK+1:IDOT-1),*) ISIZE
*
*     Add 1 MB to file size as we ignore the fraction...
*
               ISIZE  = ISIZE + 1
 
               IF(IDEBFA.GE.0.AND.IOPTV.NE.0.AND.
     +            IABS(IQ(LBANK+KOFUFA+MFSZFA)-ISIZE).GT.1)
     +                PRINT *,'FMTAPE. file size in catalogue ',
     +                        '(',IQ(LBANK+KOFUFA+MFSZFA),
     +                        ') disagress with that returned by ',
     +                        'VMSTAGE (',ISIZE,')'
 
               IQ(LBANK+KOFUFA+MFSZFA) = ISIZE
 
               IF(IDEBFA.GE.0) THEN
                  PRINT *,'FMTAPE. updating file size from STAGE ',
     +                    'information'
                  PRINT *,'FMTAPE. '//CHLINE(1:LENGTH)
               ENDIF
   20       CONTINUE
            ENDIF
*
*     Option D - make a duplicate copy into the robot
*
            IF(IOPTD.NE.0.AND.IMODE.EQ.0) THEN
               CALL FMSMCF(GENAM,L,IC)
               IF(IC.NE.0) THEN
                  PRINT *,'FMTAPE - return code ',IC,' from FMSMCF'
               ENDIF
            ENDIF
 
         ELSEIF(IRC.EQ.400) THEN
*
*     STAGE failed - cannot allocate disk size of size requested
*
            READ(CSIZE,'(I4)') ISIZE
            IF(ISIZE.LT.MEDSIZ(IQ(LBANK+KOFUFA+MMTPFA))) THEN
               IF(IDEBFA.GE.0)
     +            PRINT *,'FMTAPE. unable to allocate staging disk for',
     +                    ' size ',CSIZE,' - will try larger disk'
*
*     Increase size of staging disk requested up to the maximum of
*        - medium capacity
*        - maximum staging disk
*     The increase is done in units of 20 MB or the file size
*
*               ISIZE = MIN(ISIZE+MAX(20,IQ(LBANK+KOFUFA+MFSZFA)),
*    +                      MAXSTG,MEDSIZ(IQ(LBANK+KOFUFA+MMTPFA)))
                WRITE(CSIZE,'(I4)') ISIZE
                ISTART = INDEX(CHCOMM,'SIZE ') + 5
                CHCOMM(ISTART:ISTART+3) = CSIZE
                GOTO 10
             ELSE
                IF(IDEBFA.GE.0)
     +          PRINT *,'FMTAPE. return code from STAGE = ',IRC
                GOTO 80
             ENDIF
 
          ELSEIF(IRC.GE.20) THEN
             IF(IDEBFA.GE.0)
     +       PRINT *,'FMTAPE. return code from STAGE = ',IRC
             GOTO 80
          ENDIF
 
       ELSE
*
*        Use SETUP
*
         IF(IOPTW.EQ.0) THEN
            RING = ' NORING '
         ELSE
            RING = ' RING   '
         ENDIF
 
         CALL FMWORD(DDNAME,3,' ',CHCOMM,IRC)
         LDD = LENOCC(DDNAME)
         IF(LDD.LE.2) THEN
            READ(DDNAME(1:LDD),*) LUN
            DDNAME = 'FT00F001'
            WRITE(DDNAME(3:4),'(I2.2)') LUN
         ENDIF
*
*     Find first free tape drive
*
         ITAPE = IUCOMP(0,IDEV,16)
         IF(ITAPE.EQ.17) THEN
            IF(IDEBFA.GE.-2) PRINT *,'FMTAPE. no free ',
     +         'virtual address for tape unit'
            IRC = 1
            GOTO 80
         ENDIF
 
         ITEMP = 179 + ITAPE
         IF(ITAPE.GT.8) ITEMP = 277 + ITAPE
         WRITE(DEVNUM,'(I3)') ITEMP
         WRITE(DEVNAM,'(Z1)') ITAPE-1
*
*     Store address of tape unit
*
         IVADDR(LUN) = ITAPE
         IDEV(ITAPE) = IVADDR(LUN)
 
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_HEPVM))
         SETUP = 'SETUP '//MODEL//' '//DEVNUM//' '
     +      //VSN(1:LVSN)//' VID '//XVID//' '
     +      //LABTYP//CDEN//RING
 
         IF(IOPTE.NE.0) THEN
            LENS  = LENOCC(SETUP)
            SETUP = SETUP(1:LENS)//' (END'
         ENDIF
 
         LENS = LENOCC(SETUP)
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',SETUP(1:LENS)
         CALL VMCMS(SETUP(1:LENS),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. return code ',IRC,
     +                              ' from SETUP'
            GOTO 80
         ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_TAPESYS))
         SETUP = 'EXEC TAPESYS MOUNT '//VID(1:LVID)
     +           // ' ( ' // DEVNAM
 
         IF(VSN(1:LVSN).NE.VID(1:LVID)) THEN
            LENS  = LENOCC(SETUP)
            SETUP = SETUP(1:LENS) // ' EXTID '
         ENDIF
 
         IF(IOPTW.NE.0) THEN
            LENS  = LENOCC(SETUP)
            SETUP = SETUP(1:LENS) // ' RW '
         ENDIF
 
         IF(IOPTE.NE.0) THEN
            LENS  = LENOCC(SETUP)
            SETUP = SETUP(1:LENS) // ' WAIT '
         ENDIF
 
         LENS = LENOCC(SETUP)
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',SETUP(1:LENS)
         CALL VMCMS(SETUP(1:LENS),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. return code ',IRC,
     +                              ' from TAPESYS'
            GOTO 80
         ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_VMTAPE))
 
         IF(IOPTW.EQ.0) THEN
            RING = ' READ   '
         ELSE
            RING = ' WRITE  '
         ENDIF
 
         SETUP = 'VMTAPE MOUNT '//VSN(1:LVSN)//DEVNAM
     +           //' DSN ? (DEN '//CDEN//' LABEL '//LABTYP
     +           //RING//' WAIT UNIT '//MODEL
         LENS = LENOCC(SETUP)
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',SETUP(1:LENS)
         CALL VMCMS(SETUP(1:LENS),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. return code ',IRC,
     +                              ' from VMTAPE'
            GOTO 80
         ENDIF
         CALL VMSTAK(CHDSN(1:LDSN),'L',IRC)
#endif
#if defined(CERNLIB_IBMVM)
 
         FILEDEF = 'FILEDEF '//DDNAME//' TAP'
     +      //DEVNAM//' '//LABTYP//' ' //FSEQ(1:LFSEQ)
     +      //' (DEN '//CDEN
         LENF = LENOCC(FILEDEF)
 
         IF(IOPTW.NE.0) THEN
            FILEDEF = FILEDEF(1:LENF) // DCB
            LENF = LENOCC(FILEDEF)
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',FILEDEF(1:LENF)
         CALL VMCMS(FILEDEF(1:LENF),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. return code ',IRC,
     +         ' from FILEDEF'
            GOTO 80
         ENDIF
 
         IF((LDSN.GT.0).AND.(IOPTN.EQ.0)) THEN
            LABELDEF = 'LABELDEF '//DDNAME//' FID ? FSEQ '
     +                 //FSEQ(1:LFSEQ)
            LENL = LENOCC(LABELDEF)
 
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',LABELDEF(1:LENL)
*
*     Put DSN on program STACK for LABELDEF
*
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. DSN is ',CHDSN(1:LDSN)
            CALL VMSTAK(CHDSN(1:LDSN),'L',IRC)
            CALL VMCMS(LABELDEF(1:LENL),IRC)
         ENDIF
 
      ENDIF
 
*=======================================================================
*     end I B M   V M
*=======================================================================
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Build the relevant logical name for STAGE
*
      FORLUN = 'FOR00N'
      WRITE(FORLUN(4:6),'(I3.3)') LUN
      IF(LUN.EQ.0) FORLUN = CHLUN
      LFLUN = LENOCC(FORLUN)
*
*     Protect against logical names in the process table
*
      CALL FMGTLG(FORLUN(1:LFLUN),EQUNAM,'LNM$PROCESS',IC)
      IF(IC.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT 9005,FORLUN(1:LFLUN),EQUNAM(1:IS(1))
 9005 FORMAT(' FMTAPE. warning - conflicting logical name for ',A,/,
     +       ' = ',A,/,
     +       ' - deleted from process table')
         ISTAT = LIB$DELETE_LOGICAL(FORLUN(1:LFLUN),'LNM$PROCESS')
      ENDIF
 
      JX = ICFNBL(CSIZE,1,4)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
      IQUEST(11) = IQ(LBANK+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_PREFIX))
      CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (defined(CERNLIB_VAXVMS))&&(defined(CERNLIB_PREFIX))
      CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_VAXVMS)
      IF(IDEBFA.GE.3) THEN
         PRINT *,'FMTAPE. return from FMQTMS with ',
     +           VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +           LABTYP,'/',IC
      ENDIF
 
*
*     Translate IBM to VAX labels (SL->EBCDIC etc.)
*
      JL = ICNTH(LABTYP,IBMLAB,3)
      CHCOMM = '$STAGE ' // VSN(1:LVSN) // ' ' // VID(1:LVID) // ' '
     +         // FORLUN(1:LFLUN) // IOMODE
     +         // '/NAME=' // CHDSN(1:LDSN)
     +         // '/NUMBER=' // FSEQ(1:LFSEQ)
     +         // '/SIZE='   // CSIZE(JX:4)
     +         // '/GENERIC='// MODEL
     +         // '/LABEL='//   VAXLAB(JL)
*
*     Output STAGing only - add DCB information (also NL tapes)
*
      IF((IMODE.NE.0).OR.(LABTYP(1:2).EQ.'NL')
     +   .OR.(IOPTL.NE.0))                    THEN
         IF(INDEX(RECFM,'F').NE.0) THEN
            CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // '/FIXED'
         ELSEIF(INDEX(RECFM,'V').NE.0) THEN
            CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // '/VARIABLE'
         ENDIF
         WRITE(CHREC,'(I6.6)')  LRECL
         WRITE(CHBLK, '(I6.6)') LBLOCK
         IF(LRECL.GT.0) CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) //
     +     '/RECORDSIZE='//CHREC
         IF(LBLOCK.GT.0) CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) //
     +     '/BLOCKSIZE='//CHBLK
      ENDIF
*
      LENCOM = LENOCC(CHCOMM)
*
*     RMS format
*
      IF(RECFM(1:3).EQ.'RMS') THEN
         CHCOMM(LENCOM+1:LENCOM+4) = '/RMS'
         LENCOM = LENCOM + 4
      ENDIF
*
*     Full tape option
*
      IF(IOPTH.NE.0) THEN
         CHCOMM = CHCOMM(1:LENCOM) // '/FULLTAPE'
         LENCOM = LENCOM + 9
      ENDIF
*
*     'T' option - read directly from tape
*
      IF(IOPTT.NE.0) THEN
         CHCOMM = CHCOMM(1:LENCOM) // '/DIRECT'
         LENCOM = LENCOM + 7
      ENDIF
*
*     'X' option - create direct access file on disk
*
      IF(IOPTX.NE.0) THEN
         CHCOMM = CHCOMM(1:LENCOM) // '/ACCESS=DIRECT'
         LENCOM = LENCOM + 14
      ENDIF
 
      IF(IOPTY.NE.0) THEN
         CHOPEN = 'SYS$LOGIN:'//CHUSER(1:LUSER)//'.FMSTAGE'
         LOPEN  =  LUSER + 18
         IF(IDEBFA.GE.1) PRINT *,'FMTAPE. writing STAGE command to ',
     +      CHOPEN(1:LOPEN)
         OPEN(LUN,STATUS='NEW',FORM='FORMATTED',ACCESS='SEQUENTIAL',
     +        FILE=CHOPEN(1:LOPEN),
     +        IOSTAT=IRC)
         IF(IRC.NE.0) GOTO 80
         WRITE(LUN,'(A)',IOSTAT=IRC) CHCOMM(1:LENCOM)
         IF(IRC.NE.0) GOTO 80
         CLOSE(LUN)
         GOTO 80
      ENDIF
*
*     Check that we can use STAGE, before doing LIB$SPAWN...
*
      CALL FMSTGP(CHGRP,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. error checking STAGE',
     +      ' IRC = ',IRC
         GOTO 80
      ENDIF
 
      LCHGRP = LENOCC(CHGRP)
      CHSTFL = CHGRP(1:LCHGRP)//VSN(1:LVSN)//'_'//VID(1:LVID)
     +         //'.'//FSEQ(1:LFSEQ)//'_'//VAXLAB(JL)
      LCHST  = LENOCC(CHSTFL)
*
*     Output staging: define logical name and continue
*
      IF(IMODE.NE.0.AND.IOPTT.EQ.0) THEN
*
*     Set logical name
*
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. defining logical name ',
     +      FORLUN(1:LFLUN),' to point to ',CHSTFL(1:LCHST)
         ISTAT = LIB$SET_LOGICAL(FORLUN(1:LFLUN),
     +                         CHSTFL(1:LCHST),'LNM$JOB',,)
#include "fatmen/fatvaxrc.inc"
 
      ENDIF
 
      IF(IMODE.EQ.0.AND.IOPTT.EQ.0) THEN
*
*     Check whether file is already on disk
*
         INQUIRE(FILE=CHSTFL(1:LCHST),EXIST=IEXIST)
         IF(IEXIST) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. requested file ',
     +         'already on disk - checking file size...'
            OPEN(LUN,FILE=CHSTFL(1:LCHST),STATUS='OLD',
     +           FORM='UNFORMATTED',READONLY,SHARED,
     +           USEROPEN=FMBALQ,IOSTAT=ISTAT)
            ISIZE = (NBLOKS*512 + .9999*MEGA)/MEGA
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. size allocated = ',
     +         NBLOKS,' disk blocks = ',ISIZE,' MB'
*
*     If file already on disk, accept and set logical name
*
            IF(IABS(IQ(LBANK+KOFUFA+MFSZFA)-ISIZE).LT.2) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMTAPE. defining logical name ',
     +            FORLUN(1:LFLUN),' to point to ',CHSTFL(1:LCHST)
               ISTAT = LIB$SET_LOGICAL(FORLUN(1:LFLUN),
     +                               CHSTFL(1:LCHST),'LNM$JOB',,)
#include "fatmen/fatvaxrc.inc"
               GOTO 50
            ENDIF
         ENDIF
      ENDIF
*
*     Stage operation required
*
      CALL SBIT1(IHOWFA,JSTGFA)
*
*     Check if we should issue a local or remote stage...
*
      LM    = LENOCC(MODEL)
      INQUIRE(FILE='SETUP_EXE:TPSERV.CONF',EXIST=IEXIST)
*
*     Does a STAGE batch queue exist?
*
      CALL FMBQUE('STAGE_'//MODEL(1:LM)//'S',LBQ)
 
      IF(LBQ.GT.0.AND.IEXIST) THEN
         IF(IDEBFA.GE.1) PRINT *,'FMTAPE. stage batch queue exists '//
     +      '- stage request will be performed in batch'
      ELSE
         CALL FMGTLG('SETUP_'//MODEL(1:LM)//'S',EQUNAM,
     +               'LNM$SYSTEM',ILOCAL)
         IF(ILOCAL.NE.0.AND.IEXIST) THEN
            IF(IDEBFA.GE.1) PRINT *,'FMTAPE. generic device type ',
     +         MODEL(1:LM),' not found on this node - checking ',
     +         'served devices'
         ENDIF
      ENDIF
 
      IF(ILOCAL+LBQ.NE.0.AND.IEXIST) THEN
         ISTAT = LIB$GET_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
         OPEN(LUNTAP,FILE='SETUP_EXE:TPSERV.CONF',
     +        FORM='FORMATTED',STATUS='OLD',
     +        READONLY,SHARED,IOSTAT=IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. cannot open TPSERV ',
     +         'configuration file (SETUP_EXE:TPSERV.CONF)'
         ELSE
   30       CONTINUE
            READ(LUNTAP,'(A)',END=40) CHLINE
            LLINE = LENOCC(CHLINE)
            IF(IDEBFA.GE.2) PRINT *,'FMTAPE. tpserv line : ',
     +         CHLINE(1:LLINE)
            IF(INDEX(CHLINE(1:LLINE),'TPSERV').NE.0.AND.
     +         INDEX(CHLINE(1:LLINE),MODEL(1:LM)).NE.0) THEN
*
*     Get remote host name
*
               LBLNK  = INDEXB(CHLINE(1:LLINE),' ')
               CHSERV = CHLINE(LBLNK+1:LLINE)
               LSERV  = LLINE - LBLNK
               IF(IDEBFA.GE.0) PRINT *,'FMTAPE. served ',MODEL(1:LM),
     +            ' found on node ',CHSERV(1:LSERV)
               GOTO 40
            ENDIF
            GOTO 30
   40       CONTINUE
            CLOSE(LUNTAP)
         ENDIF
 
         ISTAT = LIB$FREE_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
 
         LSTA = INDEX(CHSTFL,']') + 1
*
*     Are we in server mode?
*
         IF(LQUED.GT.0) THEN
*
*     Look for an existing request for this file
*
*           INQUIRE(FILE=CHQUED(1:LQUED)//CHSTFL(LSTA:LCHST),
*    +              EXIST=IEXIST)
*           IF(IEXIST.AND.IOPTQ.NE.0) RETURN
            ICONT  = 0
            IEXIST = FAFNDF(CHQUED(1:LQUED)//CHSTFL(LSTA:LCHST),
     +                      CHFILE,ICONT)
            IF(IEXIST.EQ.RMS$_SUC.AND.IOPTQ.NE.0) RETURN
         ENDIF
*
*     Now submit remote job and wait for completion
*
         IF(FMNODE(CHSERV(1:LSERV)).EQ.0) THEN
*
*     Node is in the same cluster. (Can talk to job controller directly)
*
            IF(IDEBFA.GE.1) PRINT *,'FMTAPE. node ',CHSERV(1:LSERV),
     +         ' is in this VAXcluster - can talk to job controller'
            CALL FMCSTG(CHSTFL(LSTA:LCHST),MODEL(1:LM),CHCOMM(1:LENCOM),
     +         IQ(LBANK+KOFUFA+MFSZFA),CHOPT,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. return code ',
     +            IRC,' from FMCSTG'
               GOTO 80
            ENDIF
            IF(IOPTQ.NE.0) GOTO 80
 
         ELSE
*
*     Node is remote
*
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. node ',CHSERV(1:LSERV),
     +         ' is outside this VAXcluster - submitting job via DECnet'
            CALL FMRSTG(CHSERV(1:LSERV),CHSTFL(LSTA:LCHST),
     +         MODEL(1:LM),CHCOMM(1:LENCOM),IQ(LBANK+KOFUFA+MFSZFA),
     +         CHOPT,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. return code ',
     +            IRC,' from FMRSTG'
               GOTO 80
            ENDIF
            IF(IOPTQ.NE.0) GOTO 80
         ENDIF
*
*     Set logical name
*
         IF(IRC.EQ.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMTAPE. defining logical name ',
     +         FORLUN(1:LFLUN),' to point to ',CHSTFL(1:LCHST)
            ISTAT = LIB$SET_LOGICAL(FORLUN(1:LFLUN),
     +                            CHSTFL(1:LCHST),'LNM$JOB',,)
#include "fatmen/fatvaxrc.inc"
         ENDIF
 
      ELSE
 
         IF(IDEBFA.GE.0) PRINT *,'FMTAPE. running ',CHCOMM(1:LENCOM)
         ISTAT = LIB$SPAWN('@SETUP_EXE:STAGE "'//CHCOMM(1:LENCOM)//'"')
#include "fatmen/fatvaxrc.inc"
*
*     Get return code
*
         CALL FMGTLG('STAGE_RC',CHCODE,'LNM$JOB',IRC)
         IF(IRC.EQ.0.AND.CHCODE(1:2).EQ.'%X') THEN
            READ(CHCODE,'(2X,Z8)') ISTAT
            IF (.NOT.ISTAT)  IRC = 42
         ENDIF
 
      ENDIF
 
   50 CONTINUE
 
      IF(IRC.EQ.0.AND.IMODE.EQ.0.AND.IOPTT.EQ.0) THEN
*
*     Check file size on disk
*
         IF(((IOPTS.NE.0.AND.IQ(LBANK+KOFUFA+MFSZFA).EQ.0)
     +         .OR.IOPTV.NE.0)
     +         .AND.(LUFZFA.GT.0.AND.IMODE.EQ.0)) THEN
               OPEN(LUN,FILE=CHSTFL(1:LCHST),STATUS='OLD',
     +              FORM='UNFORMATTED',READONLY,SHARED,
     +              USEROPEN=FMBALQ,IOSTAT=ISTAT)
            ISIZE = (NBLOKS*512+.9999*MEGA)/MEGA
            IF(IDEBFA.GE.1) PRINT *,'FMTAPE. size allocated = ',
     +      NBLOKS,' disk blocks = ',ISIZE,' MB'
 
            IF(IDEBFA.GE.0.AND.IOPTV.NE.0.AND.
     +         IABS(IQ(LBANK+KOFUFA+MFSZFA)-ISIZE).GT.1)
     +         PRINT *,'FMTAPE. file size in ',
     +                  'catalogue (',IQ(LBANK+KOFUFA+MFSZFA),
     +                  ') disagress with that returned by STAGE (',
     +                  ISIZE,')'
 
            IQ(LBANK+KOFUFA+MFSZFA) = ISIZE
*
*     Get fully qualified file name
*           INQUIRE(FILE=CHSTFL(1:LCHST),NAME=CHFNFA)
 
            LNFNFA = LENOCC(CHFNFA)
*
*     How is this disk accessed? (DFS, VAXcluster)
*
            LCOLON = INDEX(CHFNFA(1:LNFNFA),':')
            IF(LCOLON.GT.0) THEN
               CALL FMGTLG(CHFNFA(1:LCOLON-1),EQUNAM,
     +            'LNM$SYSTEM',ICODE)
               IF(ICODE.EQ.0.AND.EQUNAM(1:4).EQ.'DFSC') THEN
                  CALL SBIT1(IHOWFA,JDFSFA)
               ELSE
*
*     Get host name
*
                  ICODE = LIB$GETDVI(DVI$_HOST_NAME,,CHFNFA(1:LCOLON),
     +                               ,EQUNAM,LNAME)
                  IF(EQUNAM(1:LNAME).EQ.CHHOST(1:LENOCC(CHHOST))) THEN
                     CALL SBIT1(IHOWFA,JLOCFA)
                  ELSE
                     CALL SBIT1(IHOWFA,JMSCFA)
                  ENDIF
               ENDIF
            ENDIF
 
         ENDIF
 
      ENDIF
 
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_HEPVM))&&(!defined(CERNLIB_VMTAPE))&&(!defined(CERNLIB_NEEDFILE))&&(!defined(CERNLIB_TAPESYS))
      PRINT *,'FMTAPE. Tape support is not available for this ',
     +        'system'
      IRC = 999
      GOTO 80
 
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_VAXTAP))
      PRINT *,'FMTAPE. Tape support is not available for this ',
     +        'system'
      IRC = 999
      GOTO 80
 
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_APOLLO))
      PRINT *,'FMTAPE. Tape support is not available for this ',
     +        'system'
      IRC = 999
      GOTO 80
 
#endif
#if defined(CERNLIB_APOLLO)
      IF(IAPOL3.EQ.0) THEN
         PRINT *,'FMTAPE. Tape support is not available for this ',
     +           'system'
         IRC = 999
         GOTO 80
      ENDIF
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
      IQUEST(11) = IQ(LBANK+KOFUFA+MMTPFA)
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO))&&(!defined(CERNLIB_PREFIX))
      CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO))&&(defined(CERNLIB_PREFIX))
      CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO)
      IF(IDEBFA.GE.3) THEN
         PRINT *,'FMTAPE. return from FMQTMS with ',
     +           VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +           LABTYP,'/',IC
      ENDIF
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
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO)
 
      CALL FMITOC(IQ(LBANK+KOFUFA+MFSQFA),FSEQ,LFSEQ)
 
      IF(LUN.EQ.0) FORLUN = CHLUN
*        "stagein fort.lun -v vsn -V vid -l sl|nl|al|blp
*                          -g TAPE|CART|SMCF -d 6250|1600"
#endif
#if defined(CERNLIB_APOLLO)
      IF(IMODE.EQ.0) THEN
         CHCOMM = 'l3stage -i '
      ELSEIF(IMODE.EQ.1) THEN
*
*     Output staging on Apollo:
*        Use temporary file in current directory
*        or in directory specified by L3STAGE
*
         CALL GETENVF('L3STAGE',L3PATH)
           IF(IS(1).EQ.0) THEN
              STGPTH = CHDSN(1:LDSN)
           ELSE
              STGPTH = L3PATH(1:IS(1)) // CHDSN(1:LDSN)
              LSTG   = IS(1) + LDSN
           ENDIF
        LSTG   = IS(1) + LDSN
        CHFNFA = STGPTH(1:LSTG)
        LNFNFA = LSTG
        IF(IDEBFA.GE.0) PRINT *,'FMTAPE. stage out file is ',
     +        STGPTH(1:LSTG)
        GOTO 80
      ENDIF
 
      CHCOMM = CHCOMM(1:LENOCC(CHCOMM))
     +         // ' -v '
     +         //VID(1:LVID)//' -l '//LABTYP//' -t '//MODEL
     +         // ' -f ' //FSEQ(1:LFSEQ)    // ' -d '//CDEN
     +         // ' -s '//CSIZE
      IF(IWAIT) CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' -w '
#endif
#if defined(CERNLIB_CRAY)
      CHCOMM = 'stagein  '//FORLUN
#endif
#if defined(CERNLIB_SHIFT)
      LFLUN  = LENOCC(FORLUN)
      CHFILE = 'T'//VID(1:LVID)//'.FSEQ'//FSEQ(1:LFSEQ)
      LFILE  = LENOCC(CHFILE)
*
*     Remove existing link (if any)
*
      CALL UNLINKF(CHFILE(1:LFILE))
*
*     Only specify -U option of FORLUN looks like a Fortran unit...
*
      IF(FORLUN(1:3).EQ.'ftn') THEN
         IFLUN = 4
      ELSEIF(FORLUN(1:5).EQ.'fort.') THEN
         IFLUN = 6
      ELSE
         IFLUN = 1
      ENDIF
 
      IF(ICNUM(FORLUN,IFLUN,LFLUN).EQ.LFLUN+1) THEN
         CHCOMM = 'stagein  -G -U '//FORLUN(1:LFLUN)
         LENCOM = 15 + LFLUN
      ELSE
         CHCOMM = 'stagein  -G '
         LENCOM = 12
         CHFILE = FORLUN(1:LFLUN)
      ENDIF
*
*     Option I - disable -G option 'individual'
*      -G     Specifies  that  the tape copy operations should be
*             issued on the tape server by the  'group  user'.  A
*             'group  user'  may  be  defined  for  each group in
*             /etc/shift.conf.
*             For example:
*                  GRPUSER ws     opalprod
*
      IF(IOPTI.NE.0) CHCOMM(10:11) = '  '
 
*    +         // ' '//CHFILE(1:LFILE)
 
      CHCOMM    = CHCOMM(1:LENCOM)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
     +         // ' -v '//VSN(1:LVSN)// ' -V '
     +         //VID(1:LVID)//' -l '//LABTYP//' -g '//MODEL
     +         // ' -q ' //FSEQ(1:LFSEQ)
#endif
#if defined(CERNLIB_SHIFT)
     +         // ' -s '//CSIZE
#endif
#if (defined(CERNLIB_SHIFT))&&(defined(CERNLIB_OLDDPM))
     +         // ' -u '//CHUSER(1:LUSER)
     +         // ' -p '//CHPOOL
#endif
#if defined(CERNLIB_CRAY)
     +         // ' -K -S sbin'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
 
      IF(IMODE.NE.0) CHCOMM(1:8) = 'stageout'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO)
      LENCOM = LENOCC(CHCOMM)
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
*
*     Add DSN if IOPTN not specified
*
      IF(IOPTN.EQ.0) THEN
         CHCOMM = CHCOMM(1:LENCOM) // ' -f '//CHDSN(1:LDSN)
         LENCOM = LENOCC(CHCOMM)
      ENDIF
 
#endif
#if defined(CERNLIB_APOLLO)
*
*     Add DSN if IOPTN not specified
*
      IF(IOPTN.EQ.0) THEN
         CHCOMM = CHCOMM(1:LENCOM) // ' -n '//CHDSN(1:LDSN)
         LENCOM = LENOCC(CHCOMM)
      ENDIF
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
*
*     Option T - direct access to tapes
*
      IF(IOPTT.NE.0) THEN
         CHCOMM(1:8) = 'setup   '
*
*     Option W - write access
*
         IF(IOPTW.NE.0) THEN
            CHCOMM = CHCOMM(1:LENCOM) // '-r in'
            LENCOM = LENCOM + 5
         ENDIF
 
      ENDIF
*
*     Add DCB information
*
      IF(LRECL.EQ.0.OR.LBLOCK.EQ.0.OR.RECFM(1:1).EQ.' ') THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. DCB information ',
     +      'missing or invalid'
         IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. lrecl = ',LRECL,
     +      ' blocksize = ',LBLOCK,' recfm = ',RECFM
         IRC = 27
         GOTO 80
      ENDIF
 
#endif
#if defined(CERNLIB_SHIFT)
      IF(RECFM.EQ.'U') RECFM = 'F'
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
 
      WRITE(DCB,9006) RECFM,LRECL,LBLOCK
 9006 FORMAT(' -F ',A,' -L ',I6,' -b ',I6)
      CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // DCB
      LENCOM = LENOCC(CHCOMM)
 
#endif
#if (defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_TMS))
*
*     Add density
*
      CHCOMM = CHCOMM(1:LENCOM) // ' -d ' // DENS
      LENCOM = LENOCC(CHCOMM)
#endif
#if defined(CERNLIB_SHIFT)
      CHCOMM = CHCOMM(1:LENCOM) // ' '//CHFILE(1:LFILE)
      LENCOM  = LENCOM + LFILE + 1
*
*     NOWAIT option
*
      IF(.NOT.IWAIT) THEN
         LENCOM  = LENCOM + 1
         CHCOMM(LENCOM:LENCOM) = '&'
      ENDIF
#endif
#if defined(CERNLIB_APOLLO)
*
*     Add DCB information, direct output to temporary file
*
      CALL FMFNME(CHFILE)
      LCHF = LENOCC(CHFILE)
      IF(IDEBFA.GE.2.AND.IOPTY.EQ.0)
     +   PRINT *,'FMTAPE. output of STAGE command ',
     +   'will be sent to /tmp/'//CHFILE(1:LCHF)
      WRITE(DCB,9006) RECFM,LRECL,LBLOCK
 9006 FORMAT(' -r ',A,' -c ',I5,' -b ',I5)
      CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // DCB
     +          // ' ' // VSN(1:LVSN)
     +         // ' > /tmp/'//CHFILE(1:LCHF)
      LENCOM = LENOCC(CHCOMM)
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO)
 
      CALL CSQMBL(CHCOMM,1,LENCOM)
      LENCOM = IS(1)
*
*     Write command to file and return
*
      IF(IOPTY.NE.0) THEN
         CHOPEN = '/tmp/'//CHUSER(1:LUSER)//'.fmstage'
         LOPEN  = LUSER + 12
         CALL CUTOL(CHOPEN(1:LOPEN))
         IF(IDEBFA.GE.1) PRINT *,'FMTAPE. writing STAGE command to ',
     +      CHOPEN(1:LOPEN)
         OPEN(LUN,STATUS='UNKNOWN',ACCESS='SEQUENTIAL',
     +        FORM='FORMATTED',
     +        FILE=CHOPEN(1:LOPEN),
     +        IOSTAT=IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. iostat ',IRC,
     +         ' from open of temporary file (option Y)'
            GOTO 80
         ENDIF
#endif
#if defined(CERNLIB_APOLLO)
         LENCOM = INDEX(CHCOMM,'>') - 1
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOLLO)
         WRITE(LUN,'(A)',IOSTAT=IRC) CHCOMM(1:LENCOM)
         IF(IRC.NE.0) GOTO 80
         CLOSE(LUN)
         GOTO 80
      ENDIF
      IF(IDEBFA.GE.0) PRINT *,'FMTAPE. executing ',CHCOMM(1:LENCOM)
#endif
#if defined(CERNLIB_APOLLO)
      ISTAT = 0
   60 CONTINUE
      IC =  SYSTEMF(CHCOMM(1:LENCOM))
*
*     Check output of stage command
*
      OPEN(LUN,FILE='/tmp/'//CHFILE(1:LCHF),STATUS='OLD',
     +     FORM='FORMATTED')
   70 READ(LUN,'(A)',END=30 ) CHLINE
      LCHL = LENOCC(CHLINE)
      IF(IDEBFA.GE.0) PRINT *,'FMTAPE. ',CHLINE(1:LCHL)
      IF(INDEX(CHLINE(1:LCHL),'path : ').NE.0) THEN
         ISTART = INDEX(CHLINE(1:LCHL),'/')
         STGPTH = CHLINE(ISTART:)
         CHFNFA = STGPTH
         LNFNFA = LENOCC(CHFNFA)
      ELSEIF(INDEX(CHLINE(1:LCHL),'stat : ').NE.0) THEN
         ISTAT = 1
         IF(INDEX(CHLINE(1:LCHL),'ABORTED').NE.0) THEN
            IRC = -1
            GOTO 80
         ELSEIF(INDEX(CHLINE(1:LCHL),'ENDED_OK').NE.0) THEN
            GOTO 30
         ENDIF
      ENDIF
      GOTO 70
   30 CLOSE(LUN)
      IF(ISTAT.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMTAPE. no status return ',
     +      'from stage. Will retry in 60 seconds'
         CALL SLEEPF(60)
         GOTO 60
      ENDIF
#endif
#if defined(CERNLIB_SHIFT)
      IRC = SYSTEMF(CHCOMM(1:LENCOM))
      IF(IRC.NE.0) THEN
         PRINT *,'FMTAPE. return code ',IRC,' from stage command'
         GOTO 80
      ENDIF
 
      IF(.NOT.IWAIT) GOTO 80
 
      IF(IOPTU.EQ.0) THEN
*
*     New stager?
*
         INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST)
 
         IF(IEXIST) THEN
            IF(IDEBFA.GE.1) WRITE(LPRTFA,9005)
 9005 FORMAT(' FMTAPE. new stager detected')
 
            SHFNAM = CHFILE(1:LFILE)
            ISTAT  = READLNF(CHFILE(1:LFILE),CHFNFA)
            LNFNFA = IS(1)
         ELSE
            IF(IDEBFA.GE.1) WRITE(LPRTFA,9007)
 9007 FORMAT(' FMTAPE. old stager detected')
 
            IF(ISFGET.NE.0) THEN
*
*     Issue SFGET to resolve link and hence activate RFIO
*     (not necessary with latest SHIFT s/w)
*
               CALL FMFGET(CHPOOL,CHUSER,CHFILE,SHFNAM,IMODE,'T',IRC)
               IF(IRC.NE.0) THEN
                  PRINT *,'FMTAPE. return code ',IRC,' from sfget'
                  GOTO 80
               ELSE
*
*     Fully qualified file name
*
                  CHFNFA = SHFNAM(1:IS(1))
                  LNFNFA = IS(1)
               ENDIF
            ELSE
               SHFNAM = CHFILE(1:LFILE)
               ISTAT  = READLNF(CHFILE(1:LFILE),CHFNFA)
               LNFNFA = IS(1)
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_CRAY)
      IRC = SYSTEMF(CHCOMM(1:LENCOM))
      CALL FMASSN(FORLUN,CHFNFA,'G',ICODE)
      LNFNFA = LENOCC(CHFNFA)
#endif
   80 CONTINUE
      IF(IDEBFA.GE.1) THEN
         CALL DATIME(ID,IT)
         PRINT 9008,ID,IT,IS(6)
 9008 FORMAT(' FMTAPE. exit at ',I6.6,1X,I4.4,I2.2)
      ENDIF
 
      END
