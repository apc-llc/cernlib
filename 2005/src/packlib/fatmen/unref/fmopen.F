*
* $Id: fmopen.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmopen.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMOPEN(GENAME,CHLUN,LENTRY,CHOPT,IRC)
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/tmsdef.inc"
#include "fatmen/fatstg.inc"
#include "fatmen/fattyp.inc"
#include "fatmen/fatinfo.inc"
#include "zebra/zmach.inc"
#include "fatmen/fatvidp.inc"
#include "fatmen/slate.inc"
#include "fatmen/fabalq.inc"
      PARAMETER     (MEGA=1024*1024)
      PARAMETER     (LKEYFA=10)
#if defined(CERNLIB_CERNVM)
      PARAMETER     (MAXSTG=210)
#endif
#if !defined(CERNLIB_CERNVM)
      PARAMETER     (MAXSTG=200)
#endif
#if defined(CERNLIB_IBMMVS)
      PARAMETER     (MODEFT=1)
#endif
      DIMENSION     KEYS(LKEYFA)
#if defined(CERNLIB_IBMVM)
      CHARACTER*16  CHSFS
      CHARACTER*80  CHGIME
#endif
#if defined(CERNLIB_SHIFT)
      CHARACTER*255 SHFNAM,SHUNAM
      CHARACTER*16  SHPOOL,SHUSER
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*8   CHSERV
      CHARACTER*255 EQUNAM
      CHARACTER*155 CHGRP,CHSTFL
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
      CHARACTER*12  CHNREC,CHNRC2,CHRECL,CHBLF
      CHARACTER*9   CHACT
      CHARACTER*8   CHUSER
      CHARACTER*8   ROUTIN,STATE
      CHARACTER*12  FORMT
      CHARACTER*255 COMAND,SETUP,LABELDEF,FILEDEF,CHFILE,L3PATH,CWD
      CHARACTER*255 CHNFS,CHDSN
      CHARACTER*4   DEVTYP
      CHARACTER*6   VSN,VID,FSEQ
      CHARACTER*15  XVID
      CHARACTER*8   VIP
*     CHARACTER*2   LABEL
      CHARACTER*6   CHREC,CHBLK
      CHARACTER*6   VAXLAB(3)
      CHARACTER*2   IBMLAB(3)
      CHARACTER*(*) GENAME
      CHARACTER*8   HNAME,HTYPE,HSYS,HHOST
      CHARACTER*8   USER,ADDR
      CHARACTER*256 DSN
      CHARACTER*2   MODE
      CHARACTER*4   CFMODE
      CHARACTER*80  CHLINE
      CHARACTER*8   FORLUN
      INTEGER       FMHOST,FMUSER,FMNODE
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
      LOGICAL       IWAIT,ILINK,IEXIST,IVMIO,FPACK
      INTEGER       SYSTEMF
#if defined(CERNLIB_IBMVM)
      EXTERNAL      FMVMIO
      CHARACTER*8   CHACC
      CHARACTER*2   CHUNIT
#endif
#if defined(CERNLIB_SHIFT)
      EXTERNAL      FMFZIO
#endif
#if defined(CERNLIB_VAXVMS)
      EXTERNAL      FMBALQ
#endif
      DIMENSION     LENTRY(1)
      DATA          NENTRY/0/
      DATA          IBMLAB(1)/'SL'/,IBMLAB(2)/'NL'/IBMLAB(3)/'AL'/
      DATA          VAXLAB(1)/'EBCDIC'/,
     +              VAXLAB(2)/'NONE  '/,
     +              VAXLAB(3)/'ASCII '/
#if defined(CERNLIB_IBMMVS)
      DATA          HDISP(2)/4HKEEP/,HDISP(3)/4HKEEP/
#endif
*
*     CHOPT: (lowercase = reserved but not implemented)
*        reserved: acm
*        free    : bghijqy
*
*           a - append
*           b - *free*
*           c - concatenate (for M)
*           D - make duplicate into robot
*           E - when used with T, issue SETUP END
*           F - issue FZFILE or FPARM as appropriate
*           g - *free*
*           h - *free*
*           i - *free*
*           j - *free*
*           K - KEEP option on STAGE OUT
*           L - override tape label information with DCB from catalogue
*           m - multi-file?
*           N - don't use DSN on STAGE
*           O - override size with IQUEST(11)
*           P - autoput on STAGE OUT
*           q - *free*
*           R - read
*           S - update catalogue with size returned from STAGE IN
*           T - use tape directly (i.e. not stage)
*           U - user will issue open
*           V - as S, but even if file size is non-zero
*           W - write
*           y - *free*
*           X - D/A
*           Z - issue RZFILE
#include "fatmen/fatopts.inc"
      NCH = LENOCC(GENAME)
#if defined(CERNLIB_IBMVM)
      CALL FMONIT('FMOPEN. '//GENAME(1:NCH)//' CHOPT '//CHOPT)
#endif
      IF (LENTRY(1) .EQ. 0) THEN
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9001) GENAME(1:NCH),CHOPT
 9001 FORMAT(' FMOPEN. enter for ',A,1X,A)
         CALL FMGET(GENAME,LENTRY,KEYS,IRC)
         IF (IRC.NE.0) RETURN
         ELSE
         IF(IDEBFA.GE.1) WRITE(LPRTFA,9002) GENAME(1:NCH)
 9002 FORMAT(' FMOPEN. enter for ',A,' using user supplied bank')
      ENDIF
      L = LENTRY(1)
*
*     Set routine name
*
      ROUTIN = 'FMOPEN. '
*
*     FATMEN file format (for call to FZFILE,RZFILE)
*
      CALL UHTOC(IQ(L+KOFUFA+MFLFFA),4,FFORM,4)
      ISIZE  = 0
      ICFOP  = 0
      IVMIO  = .FALSE.
*
*     Check options
*
      CALL FMCHOP(ROUTIN,CHOPT,'CDEFKLMONPRSTUVWXZ',IC)
 
      IF((IOPTR.EQ.0).AND.(IOPTW.EQ.0)) IOPTR = 1
 
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
*
*     Take file size from IQUEST vector, if option O is specified
*
      IF(IOPTO.NE.0) ISIZE = IQUEST(11)
*
*     I/O options for FZ:
*         IQUEST(10) = 1 - Use C I/O in FZ
*         IQUEST(10) = 2 - Use FORTRAN D/A I/O
*         IQUEST(10) = 3 - Use 'package' I/O in FZ (e.g. IOPACK)
*
      FPACK = .FALSE.
      IF(INDEX(FFORM,'FP').NE.0)    FPACK = .TRUE.
 
      IF(IOPTF.NE.0.AND..NOT.FPACK) ICFOP = IQUEST(10)
      IF(ICFOP.EQ.2) IOPTX = 1
 
      IF((IOPTX.NE.0).AND.(IOPTT.NE.0)) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. FORTRAN direct-access ',
     +      'not valid for tape files - ignored'
         IOPTX = 0
      ENDIF
 
      IF(ICFOP.EQ.1) THEN
         IF(IOPTU.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. user open not allowed',
     +         ' for C I/O with Zebra FZ'
          ENDIF
          IOPTU = 0
       ENDIF
*
*     Set mode (read/write)
*
      IMODE  = IOPTW
      IOMODE  = '/IN '
      IF(IMODE.NE.0) IOMODE  = '/OUT '
 
*
*     CHLUN can have the following formats:
*
*                                          nn
*                                          FTnnFlll
*                                          VMnnFlll
*                                          IOFILEnn
*                                          FORnnn
*                                          fort.nn
*     Other formats may be used with FPACK, for example BOSINPUT
*
      LUN  = 0
      LCHLUN = LENOCC(CHLUN)
*     Dirty trick to satisfy Unix machines
      IF (LCHLUN .EQ. 1) THEN
         READ(CHLUN,9003) LUN
      ELSEIF(LCHLUN .EQ. 2) THEN
         READ(CHLUN,9004) LUN
      ENDIF
 9003 FORMAT(I1)
 9004 FORMAT(I2)
*
*     Get LUN from CHLUN (DDNAME) if necessary
*
      IF(LUN.EQ.0.AND..NOT.FPACK) THEN
         CALL FMDD2L(CHLUN(1:LCHLUN),LUN,IRC)
      ENDIF
 
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
      LDSN   = LENOCC(DSN)
      LBLANK = INDEX(DSN,' ')
      IF(LBLANK.NE.0) LDSN = LBLANK - 1
#if defined(CERNLIB_UNIX)
*
*     If DSN starts with a $, assume that it is an environmental
*     variable.
*
      IF(DSN(1:1).EQ.'$') THEN
         LENV = INDEX(DSN(1:LDSN),'/')
         CALL GETENVF(DSN(2:LENV-1),CHNFS)
         IF(IS(1).GT.0) THEN
            CHDSN = CHNFS(1:IS(1)) // DSN(LENV:LDSN)
            LDSN  = LENOCC(CHDSN)
            DSN   = CHDSN(1:LDSN)
         ELSE
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. cannot translate ',
     +         ' environmental variable ',DSN(1:LENV-1)
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
*
*     If DSN starts with a $, assume that it is an environmental
*     variable.
*
      IF(DSN(1:1).EQ.'$') THEN
         LENV = INDEX(DSN(1:LDSN),'/')
         CALL FMGTLG(DSN(2:LENV-1),CHNFS,'LNM$SYSTEM',IRC)
         LCHNFS = IS(1)
         IF(LCHNFS.GT.0) THEN
*
*     If there is more than one slash in file name
*     assume that the intervening elements are directory names
*
            JSLASH              = INDEXB(DSN(1:LDSN),'/')
            IF(JSLASH.EQ.LENV) THEN
               CHNFS(LCHNFS+1:) = DSN(LENV+1:LDSN)
               LCHNFS           = LCHNFS + LDSN - LENV
            ELSE
               CHNFS(LCHNFS+1:) = '[' // DSN(LENV+1:JSLASH-1)
     +                            // ']' // DSN(JSLASH+1:LDSN)
               LCHNFS           = LCHNFS + LDSN - LENV + 1
               CALL CTRANS('/','.',CHNFS,1,LCHNFS)
            ENDIF
         DSN  = CHNFS(1:LCHFNS)
         LDSN = LCHNFS
 
         ENDIF
      ENDIF
#endif
*
*     Get DCB information
*
      CALL UHTOC(IQ(L+KOFUFA+MRFMFA),4,RECFM,4)
      LRECL  = IQ(L+KOFUFA+MRLNFA)*4
      LBLOCK = IQ(L+KOFUFA+MBLNFA)*4
#if defined(CERNLIB_IBMVM)
*
*     Suppress user open for FX, FXN files
*
      IF(ICFOP.EQ.2) THEN
         IVMIO = .TRUE.
         IF(IOPTU.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. user open not allowed',
     +         ' for direct-access I/O with Zebra FZ'
          ENDIF
          IOPTU = 0
       ENDIF
#endif
*
*     New Zebra uses FORTRAN I/O as default...
*
      LFORM = LENOCC(FFORM)
#if !defined(CERNLIB_IBMVM)
      IF((FFORM(1:2).EQ.'FX').AND.(ICFOP.LE.1)) THEN
#endif
#if defined(CERNLIB_IBMVM)
      IF((FFORM(1:2).EQ.'FX').AND.(ICFOP.NE.1).AND.(ICFOP.NE.3)) THEN
#endif
         FTEMP = FFORM
         FFORM = 'F'//FTEMP(1:LFORM)
      ENDIF
 
      IC = FMHOST(HNAME,HTYPE,HSYS)
      CALL UHTOC(IQ(L+KOFUFA+MHSNFA),4,HHOST,8)
      LHOST  = LENOCC(HHOST)
*
*     Find file and STAGE if necessary
*
#if defined(CERNLIB_FPACK)
*=======================================================================
*     FPACK files : machine independant interface
*=======================================================================
      IF(FPACK) THEN
*
*     build comand string for FPACK interpreter
*        OPEN symbolic-name FILE=filename HOST=hostname [options...]
*           options: RECL, BLFACTOR, NREC, NREC2, ACTION, ACCESS,
*                    STATUS, FORM, WORDFMT, RECSEP, NOOPEN
*
*           ACCESS   = sequential (FPT, FPS), direct (FPD), keyed (FPK),
*                      ordered (FPO)
*           FORM     = FPT = text, binary otherwise
*           NOOPEN   = IOPTU
*           WORDFMT  = MCPLFA
*           ACTION   = IOPTR & IOPTW (modify not supported)
*           STATUS   = OLD, unless action=write
*           NREC     = number of records, primary allocation
*           NREC2    = number of records, secondary allocation
*           RECSEP   = (not yet implemented)
*           RECL     = MRLNFA*4
*           BLFACTOR = MBLNFA/MRLNFA
*
         CHFILE = CHLUN
         COMAND = 'OPEN '//CHFILE(1:LCHLUN)//' FILE="'//DSN(1:LDSN)//'"'
     +            //' HOST='//HHOST(1:LHOST)//' ACTION='
     +            //CHACT(1:LCHACT)
         LCOM   = LENOCC(COMAND)
*
*     RECL BLFACTOR
*
         IF(IQ(L+KOFUFA+MRLNFA).GT.0) THEN
            CALL FMITOC(IQ(L+KOFUFA+MRLNFA)*4,CHRECL,JS)
            COMAND(LCOM+1:LCOM+JS+6) = ' RECL='//CHRECL(1:JS)
            LCOM = LCOM + JS + 6
            IF(IQ(L+KOFUFA+MBLNFA).GT.0) THEN
               CALL FMITOC(IQ(L+KOFUFA+MBLNFA)/IQ(L+KOFUFA+MRLNFA),
     +            CHBLF,JS)
               COMAND(LCOM+1:LCOM+JS+10) = ' BLFACTOR='//CHBLF(1:JS)
               LCOM = LCOM + JS + 10
            ENDIF
         ENDIF
*
*     Status: NEW enforced for ACTION=WRITE
*
         IF(IOPTW.NE.1.AND.IOPTR.EQ.0) THEN
            COMAND(LCOM+1:LCOM+11) = ' STATUS=NEW'
*
*      Allocation
*
            IF(IQUEST(12).GT.0) THEN
*
*      Primary...
*
               CALL FMITOC(IQUEST(12),CHNREC,JS)
               COMAND(LCOM+1:LCOM+JS+6) = ' NREC='//CHNREC(1:JS)
               LCOM = LCOM + JS + 6
               IF(IQUEST(13).GT.0) THEN
*
*      Secondary...
*
                  CALL FMITOC(IQUEST(12),CHNREC,JS)
                  COMAND(LCOM+1:LCOM+JS+7) = ' NREC2='//CHNRC2(1:JS)
                  LCOM = LCOM + JS + 7
               ENDIF
            ENDIF
         ELSE
            COMAND(LCOM+1:LCOM+11) = ' STATUS=OLD'
         ENDIF
         LCOM = LCOM + 11
*
*     Space: in case of new files, primary/secondary allocations
*     are taken from IQUEST(12-13), if non-zero
*
 
         IF(IOPTU.NE.0) THEN
            COMAND(LCOM+1:LCOM+7) = ' NOOPEN'
            LCOM = LCOM + 7
         ENDIF
*
*     WORDFMT...
*
      IF(IQ(L+KOFUFA+MCPLFA).EQ.0) THEN
*
*        'local' i.e. native
*
         COMAND(LCOM+1:LCOM+16) = ' WORDFMT=WFLOCAL'
         LCOM = LCOM + 16
 
      ELSEIF(IQ(L+KOFUFA+MCPLFA).EQ.1) THEN
*
*        IEEE big endian
*
         COMAND(LCOM+1:LCOM+15) = ' WORDFMT=WFIEEE'
         LCOM = LCOM + 15
 
      ELSEIF(IQ(L+KOFUFA+MCPLFA).EQ.2) THEN
*
*        IBM
*
         COMAND(LCOM+1:LCOM+14) = ' WORDFMT=WFIBM'
         LCOM = LCOM + 14
 
      ELSEIF(IQ(L+KOFUFA+MCPLFA).EQ.3) THEN
*
*        VAX
*
         COMAND(LCOM+1:LCOM+14) = ' WORDFMT=WFVAX'
         LCOM = LCOM + 14
 
      ELSEIF(IQ(L+KOFUFA+MCPLFA).EQ.4) THEN
*
*        DECstation (IEEE little endian)
*
         COMAND(LCOM+1:LCOM+14) = ' WORDFMT=WFDEC'
         LCOM = LCOM + 14
 
      ELSEIF(IQ(L+KOFUFA+MCPLFA).EQ.5) THEN
*
*        CRAY
*
         COMAND(LCOM+1:LCOM+15) = ' WORDFMT=WFCRAY'
         LCOM = LCOM + 15
 
      ENDIF
 
*
*     FPACK FORM and ACCESS parameters...
*
         IF(FFORM(1:3).EQ.'FPT') THEN
*
*     text files
*
            COMAND(LCOM+1:LCOM+28) = ' ACCESS=SEQUENTIAL FORM=TEXT'
            LCOM = LCOM + 28
 
         ELSEIF(FFORM(1:3).EQ.'FPS') THEN
*
*     binary sequential files
*
            COMAND(LCOM+1:LCOM+30) = ' ACCESS=SEQUENTIAL FORM=BINARY'
            LCOM = LCOM + 30
 
         ELSEIF(FFORM(1:3).EQ.'FPD') THEN
*
*     binary direct access files
*
            COMAND(LCOM+1:LCOM+26) = ' ACCESS=DIRECT FORM=BINARY'
            LCOM = LCOM + 26
 
         ELSEIF(FFORM(1:3).EQ.'FPK') THEN
*
*     binary keyed access files
*
            COMAND(LCOM+1:LCOM+25) = ' ACCESS=KEYED FORM=BINARY'
            LCOM = LCOM + 25
 
         ELSEIF(FFORM(1:3).EQ.'FPO') THEN
*
*     binary ordered access files
*
            COMAND(LCOM+1:LCOM+27) = ' ACCESS=ORDERED FORM=BINARY'
            LCOM = LCOM + 27
 
         ENDIF
 
         IF(IDEBFA.GE.0) PRINT *,'FMOPEN. call FPARM for ',
     +      COMAND(1:LCOM)
         CALL FPARM(COMAND(1:LCOM))
         CALL FERMES(COMAND,1)
         IRC    = LENOCC(COMAND)
         IF(IRC.NE.0.AND.IDEBFA.GE.-3) PRINT *,'FMOPEN. error ',
     +      'from FPARM = ',COMAND(1:IRC)
         RETURN
      ENDIF
*=======================================================================
*     FPACK files : end
*=======================================================================
#endif
*=======================================================================
*
*     Disk files ...
*
*=======================================================================
      IF(IQ(L+KOFUFA+MMTPFA).EQ.1) THEN
*
*     File is on disk. Check on Node etc. has been done in FMRZIN
*
#if defined(CERNLIB_IBMVM)
         COMAND = 'FILEDEF       FTnnF001 DISK '
         DDNAME = 'FT00F001'
 
         IF((INDEX(FFORM,'FX').NE.0).AND.(IOPTX.NE.0)) THEN
            COMAND = 'FILEDEF       VMnnF001 DISK '
            DDNAME = 'VM00F001'
         ENDIF
 
         WRITE(COMAND(17:18),9009) LUN
         WRITE(DDNAME(3:4),'(I2.2)') LUN
 
         IF(((FFORM(1:2).EQ.'FX').AND.(IOPTX.EQ.0))
     +     .OR.(FFORM(1:2).EQ.'EP')) THEN
            COMAND = 'FILEDEF       IOFILEnn     DISK '
            WRITE(COMAND(21:22),9009) LUN
            DDNAME = 'IOFILE00'
            WRITE(DDNAME(7:8),'(I2.2)') LUN
         ENDIF
 
*        IF (LUN .EQ. 0) COMAND(15:22) = CHLUN
         IF(LCHLUN.GT.2) COMAND(15:22) = CHLUN
*
*     Get disk name and link to it
*
 
         LSTA = INDEX(DSN,'<')
         IF (LSTA .NE. 0) THEN
*
*     Format of DSN is <user.address>filename.filetype on VM
*     address defaults to 191. If field <> missing, defaults to
*     current userid.
*
*     Valid filenames:
*                     FN.FT
*                     <JAMIE>FN.FT
*                     <JAMIE.191>FN.FT
* SFS                 POOL:<JAMIE.A191>FN.FT
*
            CALL CTRANS('[','<',DSN,1,LDSN)
            CALL CTRANS(']','>',DSN,1,LDSN)
            LDOT = INDEX(DSN,'.')
            LBRA = INDEX(DSN,'>')
 
            IF ((LDOT .NE. 0) .AND. (LDOT .LE. LBRA)) THEN
               LEND = LDOT
            ELSE
               LEND = LBRA
            ENDIF
 
            USER = DSN(LSTA+1:LEND-1)
            LUSR = LEND - LSTA - 1
            ADDR = '    '
 
            IF ((LDOT .NE. 0) .AND. (LDOT .LE. LBRA)) THEN
               ADDR= DSN(LDOT+1:LBRA-1)
            ENDIF
 
            LCHSFS = INDEX(DSN(1:LDSN),':')
            IF(LCHSFS.NE.0) THEN
               CHSFS = DSN(1:LCHSFS)
               IF(IDEBFA.GE.2) PRINT *,'FMOPEN. SFS pool = ',
     +            CHSFS(1:LCHSFS)
            ENDIF
 
            IF(IOPTW.NE.0) THEN
               CHACC = ' ( MR ) '
            ELSE
               CHACC = ' ( RR ) '
            ENDIF
*
*     Check if user name is numeric
*
            IC = ICNUM(USER(1:LUSR),1,LUSR)
 
            IF(IC.GT.LUSR) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. username is numeric.',
     +            ' Cannot link to this userid using GIME'
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. executing ',
     +                    'EXEC FATGIME '//USER(1:LUSR)//ADDR//CHACC
               CALL VMCMS('EXEC FATGIME '//USER(1:LUSR)//ADDR//
     +         CHACC,IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.0)
     +            PRINT *,ROUTIN//' return code from FATGIME = ',IRC
                  RETURN
               ENDIF
 
            ELSE
 
               IF(LCHSFS.EQ.0) THEN
 
                  CHGIME = 'EXEC GIME '//USER(1:LUSR)//ADDR//
     +            '(QUIET NONOTICE STACK)'
               ELSE
                  CHGIME = 'EXEC GIME '//
     +            CHSFS(1:LCHSFS)//USER(1:LUSR)//'.'//ADDR//
     +            '(QUIET NONOTICE STACK)'
               ENDIF
 
               CALL CSQMBL(CHGIME,1,80)
               LCHG   = LENOCC(CHGIME)
 
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. executing ',
     +                    CHGIME(1:LCHG)
               CALL VMCMS(CHGIME(1:LCHG),IRC)
 
               IF(IRC.GT.4) THEN
                  IF(IDEBFA.GE.0)
     +            PRINT *,ROUTIN//' return code from GIME = ',IRC
                  RETURN
               ENDIF
 
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
            IF(IDEBFA.GE.0) WRITE(LPRTFA,9005) ROUTIN,USER,ADDR,MODE
 9005  FORMAT(1X,A8,'linked to ',A8,' address ',A3,' mode ',A4)
         ELSE
            MODE = '*'
         ENDIF
 
         LDOT = INDEXB(DSN,'.')
         DSN(LDOT:LDOT) = ' '
         COMAND = COMAND(1:30) // DSN(LBRA+1:LDSN) // ' ' // MODE
         LENCOM = LENOCC(COMAND)
*           F - issue FZFILE
*
*     Don't add DCB if it is missing...
*
         WRITE(DCB,9010) RECFM,LRECL,LBLOCK
         IF((LENOCC(RECFM).GT.0).AND.
     +      (LRECL.NE.0.OR.LBLOCK.NE.0)) THEN
*        IF(IMODE.NE.0) THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // ' ( ' // DCB
         ENDIF
 
         LENCOM = LENOCC(COMAND)
         IF(IDEBFA.GE.0)
     +   PRINT *,ROUTIN//'running ',COMAND(1:LENCOM)
 
         CALL VMCMS(COMAND(1:LENCOM),IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',IRC,
     +         ' from FILEDEF'
            RETURN
         ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Find disk with most space
*
      IF(IMODE.NE.0) THEN
         CALL FMXDSK(DSN,IRC)
         LDSN = LENOCC(DSN)
      ENDIF
*
*     Just assign the relevant logical name...
*
      FORLUN = 'FOR00N'
      WRITE(FORLUN(4:6),9006) LUN
 9006 FORMAT(I3)
      IF (FORLUN(4:4) .EQ. ' ') FORLUN(4:4) = '0'
      IF (FORLUN(5:5) .EQ. ' ') FORLUN(5:5) = '0'
      IF (LUN .EQ. 0) FORLUN = CHLUN
      IC = LIB$SET_LOGICAL(FORLUN(1:LENOCC(FORLUN)),
     +                     DSN(1:LDSN))
      IF (.NOT. IC) CALL LIB$SIGNAL(%VAL(IC))
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
*
*     Check if link already exists...
*
      INQUIRE(FILE=FORLUN(1:LFLUN),EXIST=ILINK)
      IF(ILINK) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FMOPEN. removing existing symbolic link...'
         IC = SYSTEMF('rm '//FORLUN(1:LFLUN))
      ENDIF
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
     +   PRINT *,'FMOPEN. removing existing symbolic link...'
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
 
         IF(IDEBFA.GE.0) PRINT *,'FMOPEN. SHIFT POOL file...'
 
         IDOT   = INDEX(DSN(1:IRSQB),'.')
         SHPOOL = DSN(2:IDOT-1)
         SHUSER = DSN(IDOT+1:IRSQB-1)
         ISTART = IRSQB+1
         IEND   = LDSN
      IF (IDEBFA.GE.0)    WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' pool = ',SHPOOL,
     +   ' user = ',SHUSER,' dsn = ',DSN(ISTART:IEND)
*
*     Get temporary file name
*
   10    CONTINUE
         CALL FMFNME(SHUNAM)
         LUNAM = LENOCC(SHUNAM)
         INQUIRE(FILE=SHUNAM(1:LUNAM),EXIST=IEXIST)
         IF(IEXIST) THEN
            IC = SLEEPF(1)
            GO TO 10
         ENDIF
*
*     Issue SFGET to obtain full shift pathname
*
         IF(IMODE.EQ.0) THEN
 
            IRC = SYSTEMF('sfget -k -p '//SHPOOL//
     +          ' -u '//SHUSER//' '//DSN(ISTART:IEND)//' > '
     +          //SHUNAM(1:LUNAM))
 
*           IRC = SYSTEMF('assign ` sfget -k -p '//SHPOOL//
*    +                  ' -u '//SHUSER// ' '//DSN(ISTART:IEND)//'  `
*    +                  '//FORLUN(1:LFLUN)//'  ')
         ELSE
 
            IRC = SYSTEMF('sfget -p '//SHPOOL//
     +          ' -u '//SHUSER//' '//DSN(ISTART:IEND)//' > '
     +          //SHUNAM(1:LUNAM))
 
*           IRC = SYSTEMF('assign ` sfget -p '//SHPOOL//
*    +                  ' -u '//SHUSER// ' '//DSN(ISTART:IEND)//'  `
*    +                  '//FORLUN(1:LFLUN)//'  ')
         ENDIF
         IF(IRC.NE.0) THEN
            PRINT *,'FMOPEN. return code ',IRC,' from SFGET'
            RETURN
          ENDIF
*
*     Now check if sfget was successful...
*
          CALL CFOPEN(LUNPTR,0,0,'r',0,
     +                SHUNAM(1:LUNAM),IRC)
          NWREC = LEN(SHFNAM)/4
          NWTAK = NWREC
          SHFNAM = ' '
          CALL CFGET(LUNPTR,0,NWREC,NWTAK,SHFNAM,IRC)
          CALL CFCLOS(LUNPTR,0)
          LFNAM = LENOCC(SHFNAM)
          IF(INDEX(SHFNAM,'/shift').NE.1) THEN
             IF(IDEBFA.GE.0) PRINT *,'FMOPEN. error from ',
     +          'sfget - ',SHFNAM(1:LFNAM)
             IRC = -1
             RETURN
*
*     Delete temporary file only if sfget worked
*
          ELSE
             IRC  = SYSTEMF('rm '//SHUNAM(1:LUNAM))
          ENDIF
*
*     Perform assign
*
          IRC = SYSTEMF('assign '//SHFNAM(1:LSHF)//' '
     +                  //FORLUN(1:LFLUN)//'  ')
 
      ELSE
 
      IF(IDEBFA.GE.0) PRINT *,'FMOPEN. SHIFT private file...'
      IF (IDEBFA.GE.0)    WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' dsn = ',DSN(1:LDSN)
         IC = SYSTEMF('assign '//DSN(1:LDSN)//' '//
     +               FORLUN(1:LFLUN))
         SHFNAM = DSN(1:LDSN)
         LFNAM  = LDSN
         IF(IC.NE.0) THEN
            PRINT *,'FMOPEN. return code ',IC,' from SFGET'
            RETURN
          ENDIF
       ENDIF
 
#endif
*=======================================================================
*
*     Tape files ...
*
*=======================================================================
         ELSEIF(IQ(L+KOFUFA+MMTPFA).GT.1) THEN
 
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
 
 
         WRITE(FSEQ,9007) IQ(L+KOFUFA+MFSQFA)
 9007    FORMAT(I6)
 
         JFSEQ = INDEXB(FSEQ,' ') + 1
*
*     File size, if zero take default size for current medium
*
         IF(IOPTO.EQ.0) ISIZE = IQ(L+KOFUFA+MFSZFA)
         IF(ISIZE.NE.0) THEN
            IF(ISIZE.GT.MAXSTG) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. Warning - ',
     +            'staging disks are limited to ',MAXSTG,
     +            ' MB on this system'
            ENDIF
#if defined(CERNLIB_IBMVM)
*
*     May need slightly more space on disk, due to VBS format!
*
            IFUDGE = MAX(2,ISIZE/15)
#endif
#if !defined(CERNLIB_IBMVM)
            IFUDGE = 0
#endif
            WRITE(CSIZE,9008) MIN(ISIZE+IFUDGE,MAXSTG,
     +                            MEDSIZ(IQ(L+KOFUFA+MMTPFA)))
         ELSE
            WRITE(CSIZE,9008) MIN(MEDSIZ(IQ(L+KOFUFA+MMTPFA)),MAXSTG)
         ENDIF
 9008       FORMAT(I4)
 
         IF(CHLUN(1:LCHLUN) .EQ. 'NOWAIT') THEN
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
      CALL UCTOH(DSN(1:LDSN)//' ',HDSN(1),4,LDSN+1)
*
*      Check if file is catalogued
*
      INQUIRE(FILE='/'//DSN(1:LDSN),EXIST=IEXIST)
      IF(.NOT.IEXIST) THEN
*
*      Get media details
*
         CALL FMQVOL(GENAME(1:NCH),L,KEYS,
     +               LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
         CALL UHTOC(IQ(L+KOFUFA+MVSNFA),4,VSN,6)
         LVSN = LENOCC(VSN)
         CALL CLTOU(VSN)
 
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMOPEN. return from FMQVOL with ',
     +              LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
         LLAB = LENOCC(LABTYP)
         LMOD = LENOCC(MODEL)
         CALL UCTOH('NEW ',HDISP(1),4,4)
         CALL UCTOH(VSN(1:LVSN),HVOL,4,LVSN)
         CALL UCTOH(MODEL,HUNIT(1),4,LMOD)
         HLAB(1) = IQ(L+KOFUFA+MFSQFA)
         CALL UCTOH(LABTYP,HLAB(2),4,LLAB)
         IF(IOPTW.NE.0) THEN
            CALL UCTOH('OUT ',HLAB(3),4,4)
         ELSE
            CALL UCTOH('IN  ',HLAB(3),4,4)
         ENDIF
         HDCB(2) = IQ(L+KOFUFA+MRLNFA)*4
         HDCB(3) = IQ(L+KOFUFA+MBLNFA)*4
         HDCB(4) = MEDDEN(IQ(L+KOFUFA+MMTPFA))
         CALL FTDD(LUN,MODEFT,HDSN,HDISP,HVOL,HLAB,HDCB,HUNIT,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',IRC,
     +         ' from FTDD'
            RETURN
         ENDIF
      ELSE
*
*     Just set DISP=OLD
*
         CALL UCTOH('OLD ',HDISP(1),4,4)
         CALL FTDD(LUN,MODEFT,HDSN,HDISP,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',IRC,
     +         ' from FTDD'
            RETURN
         ENDIF
      ENDIF
*=======================================================================
*     end I B M   M V S
*=======================================================================
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_NEEDFILE))
*
*     Interface to FNAL NEEDFILE exec
*
         CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
         IF(IDEBFA.GE.3) THEN
            PRINT *,'FMOPEN. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
*
         IF(CHLUN(1:LCHLUN) .EQ. 'NOWAIT') THEN
            STGOPT = 'NOREPLY'
            IWAIT  = .FALSE.
         ELSE
            STGOPT = 'WAIT'
            IWAIT  = .TRUE.
            WRITE(CHUNIT,'(I2.2)') LUN
         ENDIF
 
         COMAND = 'EXEC NEEDFILE '//VSN(1:LVSN) //
     +            ' ( UNIT '//CHUNIT//' '//MODEL//' '//STGOPT
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_NEEDFILE))
C======================== Modified by C. Onions =================
         IF(((FFORM(1:2).EQ.'FX').AND.(IOPTX.EQ.0))
     +     .OR.(FFORM(1:2).EQ.'EP')) THEN
            COMAND = 'EXEC STAGE IN IOFILEnn   '
            WRITE(COMAND(21:22),9009) LUN
         ELSEIF((FFORM(1:2).EQ.'FX').AND.(IOPTX.NE.0))THEN
            COMAND = 'EXEC STAGE IN VMnnF001   '
            WRITE(COMAND(17:18),9009) LUN
 9009       FORMAT(I2.2)
         ELSE
            COMAND = 'EXEC STAGE IN FTnnF001   '
            WRITE(COMAND(17:18),9009) LUN
         ENDIF
 
         IF(LCHLUN.GT.2) COMAND(15:22) = CHLUN
*        IF (LUN .EQ. 0) COMAND(15:22) = CHLUN
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
            PRINT *,'FMOPEN. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
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
 
           COMAND = COMAND(1:LENOCC(COMAND)) // ' DEVTYPE '//MODEL
*
*     Output STAGing only - add DCB information (also NL tapes)
*
         IF((IMODE.NE.0).OR.(LABTYP(1:2).EQ.'NL')
     +      .OR.(IOPTL.NE.0))                    THEN
            WRITE(DCB,9010) RECFM,LRECL,LBLOCK
 9010       FORMAT(' RECFM ',A4,' LRECL ',I5,' BLOCK ',I5)
            COMAND = COMAND(1:LENOCC(COMAND)) // DCB
            ENDIF
*
*     Output STAGing only - options Keep, autoPut
*
         IF((IMODE.NE.0).AND.(IOPTK.NE.0)) THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // ' KEEP'
         ENDIF
 
         IF((IMODE.NE.0).AND.(IOPTP.NE.0)) THEN
            COMAND = COMAND(1:LENOCC(COMAND)) // ' AUTOPUT'
         ENDIF
 
         LENCOM = LENOCC(COMAND)
 
         IF(IOPTT.EQ.0) THEN
*
*        Use STAGE
*
   20       CONTINUE
            IF(IDEBFA.GE.0) PRINT *,ROUTIN//'running ',COMAND(1:LENCOM)
 
            CALL VMCMS(COMAND(1:LENCOM),IRC)
*
*     NOWAIT specified - just return
*
            IF(.NOT.IWAIT) RETURN
 
            IF(IRC.EQ.0) THEN
*
*     If option S specified and file size currently zero OR
*        option V                                        AND
*        read mode and data base opened for write...
*
      IF(((IOPTS.NE.0.AND.IQ(L+KOFUFA+MFSZFA).EQ.0).OR.IOPTV.NE.0)
     +   .AND.(LUFZFA.GT.0.AND.IMODE.EQ.0)) THEN
*
*     Build STAGE Query command
*
         COMAND = 'EXEC STAGE QUERY ' // VSN(1:LVSN) // '.'
     +   // FSEQ(JFSEQ:LEN(FSEQ))
     +   // '.' // LABTYP(1:LLAB) // '.' // VID(1:LVID)
     +   // ' (LIFO'
         LC     = LENOCC(COMAND)
         IF(IDEBFA.GE.2) PRINT *,'FMOPEN. running ',
     +      COMAND(1:LC)
         CALL VMCMS(COMAND(1:LC),IRC)
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
         ISIZE  = ISIZE + 1
 
         IF(IOPTV.NE.0.AND.
     +      IABS(IQ(L+KOFUFA+MFSZFA)-ISIZE).GT.1) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMOPEN. file size in catalogue ',
     +         '(',IQ(L+KOFUFA+MFSZFA),
     +         ') disagress with that returned by VMSTAGE (',ISIZE,')'
         ENDIF
 
         IQ(L+KOFUFA+MFSZFA) = ISIZE
 
         IF(IDEBFA.GE.0) THEN
         PRINT *,ROUTIN//'- updating file size from STAGE information'
         PRINT *,ROUTIN//CHLINE(1:LENGTH)
         ENDIF
      ENDIF
*
*     Option D - make a duplicate copy into the robot
*
               IF((IOPTD.NE.0).AND.(IMODE.EQ.0)) THEN
                     CALL FMSMCF(GENAME,L,IC)
                     IF(IC.NE.0) THEN
                        PRINT *,'FMOPEN - return code ',
     +                     IC,' from FMSMCF'
                  ENDIF
 
               ENDIF
 
            ELSEIF(IRC.EQ.400) THEN
*
*     STAGE failed - cannot allocate disk size of size requested
*
                  READ(CSIZE,9008) ISIZE
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
     +                      MAXSTG,
     +                      MEDSIZ(IQ(L+KOFUFA+MMTPFA)))
                    WRITE(CSIZE,9008) ISIZE
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
            IF(IOPTW.EQ.0) THEN
              RING = ' NORING '
              ELSE
              RING = ' RING   '
              ENDIF
 
            CALL FMWORD(DDNAME,3,' ',COMAND,IRC)
            LDD = LENOCC(DDNAME)
            IF(LDD.LE.2) THEN
              READ(DDNAME(1:LDD),*) LUN
              DDNAME = 'FT00F001'
              WRITE(DDNAME(3:4),9009) LUN
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
            SETUP = 'SETUP '//MODEL//' '//DEVNUM//' '
     +      //VSN(1:LVSN)//' VID '//XVID//' '
     +      //LABTYP//CDEN//RING
 
            IF(IOPTE.NE.0) THEN
               LENS  = LENOCC(SETUP)
               SETUP = SETUP(1:LENS)//' (END'
               ENDIF
 
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
 
            IF(IOPTW.EQ.0) THEN
               RING = ' READ   '
            ELSE
               RING = ' WRITE  '
            ENDIF
 
            SETUP = 'VMTAPE MOUNT '//VSN(1:LVSN)//DEVNAM
     +              //' DSN ? (DEN '//CDEN//' LABEL '//LABTYP
     +              //RING//' WAIT UNIT '//MODEL
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
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',IRC,
     +            ' from FILEDEF'
               RETURN
            ENDIF
 
            IF((LDSN.GT.0).AND.(IOPTN.EQ.0)) THEN
               LABELDEF = 'LABELDEF '//DDNAME//' FID ?'
               LENL = LENOCC(LABELDEF)
 
               IF(IDEBFA.GE.0)
     +         PRINT *,ROUTIN//'running ',LABELDEF(1:LENL)
*
*     Put DSN on program STACK for LABELDEF
*
               IF(IDEBFA.GE.0)
     +         PRINT *,ROUTIN//'DSN is ',DSN(1:LDSN)
               CALL VMSTAK(DSN(1:LDSN),'L',IRC)
               CALL VMCMS(LABELDEF(1:LENL),IRC)
 
               ENDIF
 
            ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Build the relevant logical name for STAGE
*
         FORLUN = 'FOR00N'
         WRITE(FORLUN(4:6),9006) LUN
         IF (FORLUN(4:4) .EQ. ' ') FORLUN(4:4) = '0'
         IF (FORLUN(5:5) .EQ. ' ') FORLUN(5:5) = '0'
         IF (LUN .EQ. 0) FORLUN = CHLUN
         LFLUN = LENOCC(FORLUN)
 
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
            PRINT *,'FMOPEN. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
         ENDIF
 
*
*     Translate IBM to VAX labels (SL->EBCDIC etc.)
*
         JL = ICNTH(LABTYP,IBMLAB,3)
         COMAND = '$STAGE ' // VSN(1:LVSN) // ' ' // VID(1:LVID) // ' '
     +            // FORLUN(1:LFLUN) // IOMODE
     +            // '/NAME=' // DSN(1:LDSN)
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
            WRITE(CHREC,'(I6.6)')  LRECL
            WRITE(CHBLK, '(I6.6)') LBLOCK
            IF(LRECL.GT.0) COMAND = COMAND(1:LENOCC(COMAND)) //
     +        '/RECORDSIZE='//CHREC
            IF(LBLOCK.GT.0) COMAND = COMAND(1:LENOCC(COMAND)) //
     +        '/BLOCKSIZE='//CHBLK
         ENDIF
*
         LENCOM = LENOCC(COMAND)
*
*        RMS format
*
         IF(RECFM(1:3).EQ.'RMS') THEN
            COMAND(LENCOM+1:LENCOM+4) = '/RMS'
            LENCOM = LENCOM + 4
         ENDIF
*
*        'T' option - read directly from tape
*
         IF(IOPTT.NE.0) THEN
            COMAND = COMAND(1:LENCOM) // '/DIRECT'
            LENCOM = LENCOM + 7
         ENDIF
*
*     Check that we can use STAGE, before doing LIB$SPAWN...
*
         CALL FMSTGP(CHGRP,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. error checking STAGE',
     +         ' IRC = ',IRC
            RETURN
         ENDIF
 
         ISTAGE = .TRUE.
         IF(IMODE.EQ.0.AND.IOPTT.EQ.0) THEN
*
*     Check that file is not already on disk
*
            LCHGRP = LENOCC(CHGRP)
            CHSTFL = CHGRP(1:LCHGRP)//VSN(1:LVSN)//'_'//VID(1:LVID)
     +         //'.'//FSEQ(JFSEQ:LEN(FSEQ))//'_'//VAXLAB(JL)
            LCHST  = LENOCC(CHSTFL)
 
            INQUIRE(FILE=CHSTFL(1:LCHST),EXIST=IEXIST)
            IF(IEXIST) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. requested file ',
     +            'already on disk - checking file size...'
               OPEN(LUN,FILE=CHSTFL(1:LCHST),STATUS='OLD',
     +              FORM='UNFORMATTED',READONLY,SHARED,
     +              USEROPEN=FMBALQ,IOSTAT=ISTAT)
               ISIZE = (NBLOKS*512)/MEGA + 1
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. size allocated = ',
     +            NBLOKS,' disk blocks = ',ISIZE,' MB'
*
*     If file already on disk, accept and set logical name
*
               IF(IABS(IQ(L+KOFUFA+MFSZFA)-ISIZE).LT.1) THEN
                  IRC = LIB$SET_LOGICAL(FORLUN(1:LFLUN),
     +                                  CHSTFL(1:LCHST),'LNM$JOB',,)
                  IF(.NOT.IRC) CALL LIB$SIGNAL(%VAL(IRC))
                  GOTO 70
               ENDIF
            ENDIF
         ENDIF
*
*     Check if we should issue a local or remote stage...
*
         LM    = LENOCC(MODEL)
         CALL FMGTLG('SETUP_'//MODEL(1:LM)//'S',EQUNAM,
     +               'LNM$SYSTEM',ILOCAL)
         INQUIRE(FILE='SETUP_EXE:TPSERV.CONF',EXIST=IEXIST)
         IF(ILOCAL.NE.0.AND.IEXIST) THEN
 
            IF(IDEBFA.GE.0) PRINT *,'FMOPEN. generic device type ',
     +         MODEL(1:LM),' not found on this node - checking ',
     +         'served devices'
            ISTAT = LIB$GET_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
            OPEN(LUNTAP,FILE='SETUP_EXE:TPSERV.CONF',
     +           FORM='FORMATTED',STATUS='OLD',
     +           READONLY,SHARED,IOSTAT=ISTAT)
            IF(ISTAT.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMRZIN. cannot open TPSERV ',
     +            'configuration file (SETUP_EXE:TPSERV.CONF)'
            ELSE
   50          CONTINUE
               READ(LUNTAP,'(A)',END=60) CHLINE
               LLINE = LENOCC(CHLINE)
               IF(IDEBFA.GE.2) PRINT *,'FMOPEN. tpserv line : ',
     +            CHLINE(1:LLINE)
               IF(INDEX(CHLINE(1:LLINE),'TPSERV').NE.0.AND.
     +            INDEX(CHLINE(1:LLINE),MODEL(1:LM)).NE.0) THEN
*
*     Get remote host name
*
                  LBLNK  = INDEXB(CHLINE(1:LLINE),' ')
                  CHSERV = CHLINE(LBLNK+1:LLINE)
                  LSERV  = LLINE - LBLNK
                  IF(IDEBFA.GE.0) PRINT *,'FMOPEN. served ',MODEL(1:LM),
     +               ' found on node ',CHSERV(1:LSERV)
                  GOTO 60
               ENDIF
               GOTO 50
   60          CONTINUE
               CLOSE(LUNTAP)
               ISTAT = LIB$FREE_LUN(LUNTAP)
#include "fatmen/fatvaxrc.inc"
            ENDIF
*
*     Now submit remote job and wait for completion
*
            LSTA = INDEX(CHSTFL,']') + 1
*
*     Is remote node in the same cluster?
*     (Can talk to job controller directly)
*
            IF(FMNODE(CHSERV(1:LSERV)).EQ.0) THEN
 
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. node ',CHSERV(1:LSERV),
     +            ' is in this VAXcluster - can talk to job controller'
               CALL FMCSTG(CHSTFL(LSTA:LCHST),
     +            MODEL(1:LM),COMAND(1:LENCOM),IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',
     +               IRC,' from FMCSTG'
                  RETURN
               ENDIF
 
            ELSE
 
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. node ',CHSERV(1:LSERV),
     +            ' is not in this VAXcluster - submit job via DECnet'
               CALL FMRSTG(CHSERV(1:LSERV),CHSTFL(LSTA:LCHST),
     +            MODEL(1:LM),COMAND(1:LENCOM),IRC)
               IF(IRC.NE.0) THEN
                  IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',
     +               IRC,' from FMRSTG'
                  RETURN
               ENDIF
            ENDIF
*
*     Set logical name
*
            IF(IRC.EQ.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMOPEN. defining logical name ',
     +            FORLUN(1:LFLUN),' to point to ',CHSTFL(1:LCHST)
               IRC = LIB$SET_LOGICAL(FORLUN(1:LFLUN),
     +                               CHSTFL(1:LCHST),'LNM$JOB',,)
               IF(.NOT.IRC) CALL LIB$SIGNAL(%VAL(IRC))
            ENDIF
 
         ELSE
 
         IF(ISTAGE) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMOPEN. running ',COMAND(1:LENCOM)
 
            IRC = LIB$SPAWN(COMAND(1:LENCOM))
            IF (.NOT. IRC) CALL LIB$SIGNAL(%VAL(IRC))
 
         ENDIF
 
*
*     Check file size on disk
*
            IF(((IOPTS.NE.0.AND.IQ(L+KOFUFA+MFSZFA).EQ.0)
     +         .OR.IOPTV.NE.0)
     +         .AND.(LUFZFA.GT.0.AND.IMODE.EQ.0)) THEN
               OPEN(LUN,FILE=CHSTFL(1:LCHST),STATUS='OLD',
     +              FORM='UNFORMATTED',READONLY,SHARED,
     +              USEROPEN=FMBALQ,IOSTAT=ISTAT)
 
               ISIZE = (NBLOKS*512)/MEGA + 1
               IF(IDEBFA.GE.1) PRINT *,'FMOPEN. size allocated = ',
     +            NBLOKS,' disk blocks = ',ISIZE,' MB'
 
               IF(IOPTV.NE.0.AND.
     +            IABS(IQ(L+KOFUFA+MFSZFA)-ISIZE).GT.1) THEN
                  IF(IDEBFA.GE.0) PRINT *,'FMOPEN. file size in ',
     +               'catalogue (',IQ(L+KOFUFA+MFSZFA),
     +               ') disagress with that returned by STAGE (',
     +               ISIZE,')'
               ENDIF
 
            IQ(L+KOFUFA+MFSZFA) = ISIZE
 
            ENDIF
 
         ENDIF
 
70       CONTINUE
 
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_HEPVM))&&(!defined(CERNLIB_VMTAPE))&&(!defined(CERNLIB_NEEDFILE))
         PRINT *,'FMOPEN. Tape support is not available for this ',
     +           'system'
         IRC = 999
         RETURN
 
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_VAXTAP))
         PRINT *,'FMOPEN. Tape support is not available for this ',
     +           'system'
         IRC = 999
         RETURN
 
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_APOL3))
         PRINT *,'FMOPEN. Tape support is not available for this ',
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
            PRINT *,'FMOPEN. return from FMQTMS with ',
     +              VID,'/',LIB,'/',MODEL,'/',DENS,'/',MNTTYP,'/',
     +              LABTYP,'/',IC
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
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
 
      WRITE(FSEQ,9007) IQ(L+KOFUFA+MFSQFA)
 
      IF (LUN .EQ. 0) FORLUN = CHLUN
*        "stagein fort.lun -v vsn -V vid -l sl|nl|al|blp
*                          -g TAPE|CART|SMCF -d 6250|1600"
#endif
#if defined(CERNLIB_APOL3)
        IF(IMODE.EQ.0) THEN
           COMAND = 'stage -i '
        ELSEIF(IMODE.EQ.1) THEN
*
*     Output staging on Apollo:
*        Use temporary file in current directory
*        or in directory specified by L3STAGE
*
           CALL GETENVF('L3STAGE',L3PATH)
           IF(IS(1).EQ.0) THEN
              STGPTH = DSN(1:LDSN)
           ELSE
              STGPTH = L3PATH(1:IS(1)) // DSN(1:LDSN)
              LSTG   = IS(1) + LDSN
           ENDIF
           LSTG   = IS(1) + LDSN
           IF(IDEBFA.GE.0) PRINT *,'FMOPEN. stage out file is ',
     +        STGPTH(1:LSTG)
           GOTO 60
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
        LFLUN = LENOCC(FORLUN)
        COMAND = 'stagein  -G -U '//FORLUN(1:LFLUN)
     +           // ' T'//VID(1:LVID)//'.FSEQ'//FSEQ(JFSEQ:LEN(FSEQ))
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)
     +           // ' -v '//VSN(1:LVSN)// ' -V '
     +           //VID(1:LVID)//' -l '//LABTYP//' -g '//MODEL
     +           // ' -q ' //FSEQ
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
         WRITE(DCB,9011) RECFM(1:1),LRECL,LBLOCK
 9011    FORMAT(' -F ',A1,' -L ',I5,' -b ',I5)
         COMAND = COMAND(1:LENOCC(COMAND)) // DCB
         LENCOM = LENOCC(COMAND)
 
#endif
#if defined(CERNLIB_APOL3)
*
*     Add DCB information, direct output to temporary file
*
         CALL FMFNME(CHFILE)
         LCHF = LENOCC(CHFILE)
         IF(IDEBFA.GE.3) PRINT *,'FMOPEN. output of STAGE command ',
     +      'will be sent to /tmp/'//CHFILE(1:LCHF)
         WRITE(DCB,9011) RECFM,LRECL,LBLOCK
 9011    FORMAT(' -r ',A,' -c ',I5,' -b ',I5)
         COMAND = COMAND(1:LENOCC(COMAND)) // DCB
     +             // ' ' // VSN(1:LVSN)
     +            // ' > /tmp/'//CHFILE(1:LCHF)
         LENCOM = LENOCC(COMAND)
 
#endif
#if defined(CERNLIB_CRAY)||defined(CERNLIB_SHIFT)||defined(CERNLIB_APOL3)
 
      CALL CSQMBL(COMAND,1,LENCOM)
      LENCOM = LENOCC(COMAND)
      IF(IDEBFA.GE.0) PRINT *,'FMOPEN. executing ',COMAND(1:LENCOM)
#endif
#if defined(CERNLIB_APOL3)
      ISTAT = 0
   30 CONTINUE
      IC =  SYSTEMF(COMAND(1:LENCOM))
*
*     Check output of stage command
*
      OPEN(LUN,FILE='/tmp/'//CHFILE(1:LCHF),STATUS='OLD',
     +     FORM='FORMATTED')
   40 READ(LUN,'(A)',END=50 ) CHLINE
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
            GOTO 50
         ENDIF
      ENDIF
      GOTO 40
   50 CLOSE(LUN)
      IF(ISTAT.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. no status return ',
     +      'from stage. Will retry in 60 seconds'
         CALL SLEEPF(60)
         GOTO 30
      ENDIF
   60 CONTINUE
#endif
#if defined(CERNLIB_SHIFT)
      IC = SYSTEMF(COMAND(1:LENCOM))
      IF(IC.NE.0) THEN
         PRINT *,'FMOPEN. return code ',IC,' from stage command'
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_CRAY)
      IC = SYSTEMF(COMAND(1:LENCOM))
#endif
 
         ENDIF
 
*=======================================================================
*
* End of media dependant code
*
*=======================================================================
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
#if defined(CERNLIB_IBMVM)
*
*     Set vaddr used for tape
*
      IF(IOPTT.NE.0) THEN
         IVADDR(LUN) = ITAPE
         IDEV(ITAPE) = IVADDR(LUN)
         ENDIF
#endif
*
*     Set the disk and access modes (for FMCLOS)
*
      IF (IQ(L+KOFUFA+MMTPFA) .EQ. 1) THEN
#if defined(CERNLIB_IBMVM)
         CHMODE(LUN) = MODE(1:1)
#endif
         LFMODE(LUN) = 1
         ELSE
         CHMODE(LUN) = ' '
         IF(IOPTT.EQ.0) THEN
            LFMODE(LUN) = 2
            ELSE
            LFMODE(LUN) = 3
            ENDIF
         ENDIF
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
*
*     Direction...
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
         IF(INDEX(FFORM,'A').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'A'
         ELSEIF(INDEX(FFORM,'X').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'X'
         ENDIF
*
*     FORTRAN I/O...
*
         IF((INDEX(FFORM,'FFX').NE.0).AND.(ICFOP.EQ.0)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'F'
         ENDIF
*
*     Package I/O... (e.g. IOPACK, MAGTAP)
*
         IF((INDEX(FFORM,'FX').NE.0).AND.(ICFOP.EQ.3)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'Y'
         ENDIF
*
*     File format X, but native data...
*
         IF(INDEX(FFORM,'FXN').NE.0) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'N'
         ENDIF
#if defined(CERNLIB_IBMVM)
*
*     File format X, direct access
*
         IF((INDEX(FFORM,'FX').NE.0).AND.(IOPTX.NE.0)) THEN
            LFZOPT = LFZOPT + 1
            FZOPT(LFZOPT:LFZOPT) = 'C'
         ENDIF
#endif
 
         LRECL  = IQ(L+KOFUFA+MRLNFA)
 
#if defined(CERNLIB_SHIFT)
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
 
            CALL CFOPEN(LUNPTR,MEDIUM,LRECL,CFMODE,NBUF,
     +                  SHFNAM(1:LFNAM),IRC)
*    +                  'fort.'//FORLUN(1:LFLUN),IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.-3) PRINT *,'FMOPEN. return code ',IRC,
     +            ' from CFOPEN'
               RETURN
            ENDIF
            IQUEST(1) = LUNPTR
         ENDIF
#endif
         IF(IDEBFA.GE.2) PRINT *,'FMOPEN. call FZFILE with ',
     +      'LRECL/CHOPT = ',LRECL,'/',FZOPT(1:LFZOPT)
         CALL FZFILE(LUN,LRECL,FZOPT(1:LFZOPT))
         CALL FZLOGL(LUN,IDEBFA)
#if defined(CERNLIB_IBMVM)
         IF((IOPTF.NE.0).AND.(IOPTX.NE.0).AND.
     +      (INDEX(FFORM,'FX').NE.0)) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMOPEN. call FZHOOK for ',
     +         'LUN = ',LUN
            CALL FZHOOK(LUN,FMVMIO,DBUF)
         ENDIF
#endif
 
 
         ENDIF
 
      IF(IOPTU.EQ.0) THEN
*
*     Decide on file format
*
         IF((INDEX(FFORM,'AS').NE.0).OR.(INDEX(FFORM,'FA').NE.0)) THEN
            FORMT = 'FORMATTED'
         ELSE
            FORMT = 'UNFORMATTED'
         ENDIF
*
*     Decide on file status
*
        IF(IOPTW.NE.0) THEN
           STATE = 'NEW'
        ELSE
           STATE = 'OLD'
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
         IF(IEXIST) STATE = 'OLD'
      ELSE
#if defined(CERNLIB_VAXVMS)
         INQUIRE(FILE=FORLUN,EXIST=IEXIST)
         IF(IEXIST) STATE = 'OLD'
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
            IF(IDEBFA.GE.-1) PRINT *,'FMOPEN. invalid value (',
     +         CHSPAC,') given for SPACE parameter. TRK will be used'
            CHSPAC = 'TRK '
         ENDIF
 
         MODEL = CHMGEN(1)
         LMOD  = LENOCC(MODEL)
 
         CALL FILEINF(IRC,'DEVICE',MODEL(1:LMOD),CHSPAC(1:3),
     +      ISPACE(2),'SECOND',ISPACE(3),'DIR',ISPACE(4),
     +      'RECFM',RECFM,'LRECL',IQ(L+KOFUFA+MRLNFA)*4,
     +      'BLKSIZE',IQ(L+KOFUFA+MBLNFA)*4)
      ENDIF
      ENDIF
 
           IF((FFORM(1:2).EQ.'FX').OR.(FFORM(1:2).EQ.'EP').OR.
     +        (INDEX(CHLUN,'IOFILE').NE.0)) THEN
*
*       IOPACK (implied or explicit)
*
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open supressed'
*
*       FORTRAN direct access
*
              ELSEIF((FFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)
     +               .AND.(CHLUN(1:2).NE.'VM')) THEN
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN D/A open...'
              OPEN(UNIT=LUN,
     +             FILE='/'//DSN(1:LDSN),ACTION=CHACT(1:LCHACT),
     +             ACCESS='DIRECT',STATUS=STATE,RECL=LRECL*4)
              ELSE
*
*       FORTRAN sequential I/O
*
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open...'
              OPEN(UNIT=LUN,
     +             FILE='/'//DSN(1:LDSN),ACTION=CHACT(1:LCHACT),
     +             FORM=FORMT,STATUS=STATE)
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
*        IF((IOPTX.NE.0).AND.(IOPTW.NE.0)) VMOPT = 'U'
         IF(IDEBFA.GE.2) PRINT *,
     +   'FMOPEN. call VMOPEN for input  dataset on unit ',LUN,
     +   ' with DCB ',RECFM1,LREC1,LBLK1,' VMOPT ',VMOPT
         CALL VMOPEN(LUN,FNAME,VMOPT,RECFM1,LREC1,LBLK1,IRC,INFO)
         IF(IDEBFA.GE.2) PRINT *,
     +   'FMOPEN. return from VMOPEN ',
     +   ' with DCB ',RECFM1,LREC1,LBLK1
         IF(IABS(IRC).GT.1) THEN
            IF(IDEBFA.GT.-3)
     +      PRINT *,'FMOPEN. return code ',IRC,
     +              ' from VMOPEN for input file, INFO = ',INFO
         ELSE
            IRC = 0
         ENDIF
#endif
#if defined(CERNLIB_IBMVM)
           ELSEIF((FFORM(1:2).EQ.'FX').OR.(FFORM(1:2).EQ.'EP').OR.
     +        (INDEX(CHLUN,'IOFILE').NE.0)) THEN
*
*       IOPACK (implied or explicit)
*
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open supressed'
*
*       FORTRAN direct access
*
              ELSEIF((FFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)
     +               .AND.(CHLUN(1:2).NE.'VM')) THEN
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN D/A open...'
              OPEN (UNIT=LUN,ACCESS='DIRECT',STATUS=STATE,RECL=LRECL*4,
     +              ACTION=CHACT(1:LCHACT))
              ELSE
*
*       FORTRAN sequential I/O
*
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open...'
              OPEN (UNIT=LUN,FORM=FORMT,STATUS=STATE,
     +              ACTION=CHACT(1:LCHACT))
              ENDIF
#endif
#if defined(CERNLIB_APOL3)
         IF((IQ(L+KOFUFA+MMTPFA).EQ.1).OR.(IMODE.NE.0)) THEN
            CHFILE = DSN(1:LDSN)
            LCHF   = LDSN
         ELSEIF((IQ(L+KOFUFA+MMTPFA).GT.1).AND.(IMODE.EQ.0)) THEN
            CHFILE = STGPTH
            LCHF   = LENOCC(CHFILE)
         ENDIF
         IF(IDEBFA.GE.0) PRINT *,'FMOPEN. open file ',
     +           CHFILE(1:LCHF)
#endif
#if defined(CERNLIB_UNIX)
           IF((FFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)) THEN
#endif
#if (defined(CERNLIB_APOLLO)||defined(CERNLIB_NORD)||defined(CERNLIB_SUN)||defined(CERNLIB_IBMRT)||defined(CERNLIB_MACMPW)||defined(CERNLIB_AIX370))&&(defined(CERNLIB_UNIX))
      LREC=LRECL*4
#endif
#if (defined(CERNLIB_DECS)||defined(CERNLIB_SGI))&&(defined(CERNLIB_UNIX))
      LREC=LRECL
#endif
#if (defined(CERNLIB_CRAY)||defined(CERNLIB_CONVEX))&&(defined(CERNLIB_UNIX))
      LREC=LRECL*8
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_APOL3))
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN D/A open...'
              OPEN (UNIT=LUN,ACCESS='DIRECT',STATUS=STATE,RECL=LREC)
              ELSE
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open...'
              OPEN (UNIT=LUN,FORM=FORMT,STATUS=STATE)
              ENDIF
#endif
#if defined(CERNLIB_APOL3)
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN D/A open...'
              OPEN (UNIT=LUN,FILE=CHFILE(1:LCHF),
     +              ACCESS='DIRECT',STATUS=STATE,RECL=LREC)
              ELSE
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open...'
              OPEN (UNIT=LUN,FILE=CHFILE(1:LCHF),
     +              FORM=FORMT,STATUS=STATE)
              ENDIF
 
#endif
#if defined(CERNLIB_SHIFT)
*
*     LRECL in bytes for SHIFT...
*
              LREC  = LRECL*4
              SHOPT = ' '
              ELSE
              LREC  = LRECL*4
              SHOPT = 'D'
              ENDIF
 
       IF(ICFOP.EQ.0) THEN
           IF(IDEBFA.GE.0) PRINT *,'FMOPEN. calling XYOPEN with ',
     +        'LUN,LRECL,CHOPT = ',LUN,',',LREC,',',SHOPT
           CALL XYOPEN(LUN,LREC,SHOPT,IRC)
           IF(IRC.NE.0) THEN
              PRINT *,'FMOPEN. return code ',IRC,' from XYOPEN '
              RETURN
           ENDIF
 
        IF(IOPTF.NE.0) THEN
           IF(IDEBFA.GE.0) PRINT *,'FMOPEN. call FZHOOK for ',
     +        'LUN = ',LUN
           CALL FZHOOK(LUN,FMFZIO,DBUF)
        ENDIF
 
        ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Always open VAX files SHARED
*
        IF(IOPTR.NE.0) THEN
           IF((FFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)) THEN
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN D/A open...'
             OPEN (UNIT=LUN,STATUS=STATE,
     +             ACCESS='DIRECT',RECL=LRECL,
     +             SHARED, READONLY)
             ELSE
              IF(IDEBFA.GE.2) PRINT *,'FMOPEN. FORTRAN open...'
             OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE,
     +             SHARED, READONLY)
             ENDIF
 
           ELSE
           IF((FFORM(1:2).EQ.'DA').OR.(IOPTX.NE.0)) THEN
             OPEN (UNIT=LUN,STATUS=STATE,
     +             ACCESS='DIRECT',RECL=LRECL,
     +             SHARED)
             ELSE
             OPEN (UNIT=LUN, FORM=FORMT, STATUS=STATE,
     +             SHARED)
             ENDIF
 
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
         IF(IDEBFA.GE.2) PRINT *,'FMOPEN. call RZOPEN with ',
     +      'LUN/CHDIR/DSN/RZOPT/LRECL = ',
     +      LUN,'/',CHDIR,'/',DSN(1:LDSN),'/',RZOPT,'/',LRECL
         CALL RZOPEN(LUN,CHDIR,DSN(1:LDSN),RZOPT,LRECL,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMOPEN. return code ',IRC,
     +                      ' from RZOPEN'
            RETURN
            ENDIF
 
         RZOPT = ' '
         IF(IOPTW.NE.0) RZOPT = 'ULD'
         IF(IOPT1.NE.0) RZOPT = '1ULD'
         IF(IDEBFA.GE.2) PRINT *,'FMOPEN. call RZFILE for ',
     +      ' LUN/CHDIR/RZOPT = ',LUN,'/',CHDIR,'/',RZOPT
         CALL RZFILE(LUN,CHDIR,RZOPT)
         IRC = IQUEST(1)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMOPEN. return code ',IRC,
     +                      ' from RZFILE'
            RETURN
            ENDIF
         ENDIF
 
      END
