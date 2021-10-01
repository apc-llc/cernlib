*
* $Id: fmdisk.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmdisk.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDISK(GENAM,LBANK,KEYS,CHLUN,CHOPT,IRC)
 
      CHARACTER*(*) GENAM,CHLUN
#include "fatmen/fatmon.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatinfo.inc"
#include "fatmen/slate.inc"
#if defined(CERNLIB_IBMVM)
      CHARACTER*2   CHLETT
      CHARACTER*4   CHFORM,RECFM
      CHARACTER*8   CHACC,DDNAME,USER,ADDR
      CHARACTER*16  CHSFS
      CHARACTER*40  CHDCB
      CHARACTER*80  CHGIME,CHLINE
      CHARACTER*255 CHCOMM
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*255 CHFILE
      CHARACTER*255 EQUNAM
      CHARACTER*8   CHHOST
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      CHARACTER*8   FORLUN
#endif
#if defined(CERNLIB_UNIX)
      LOGICAL       IEXIST
      INTEGER       UNLINKF
#endif
#if defined(CERNLIB_SHIFT)
#include "fatmen/fmshft.inc"
#endif
#include "fatmen/fmnkeys.inc"
      INTEGER       SYSTEMF
      DIMENSION KEYS(LKEYFA)
      CHARACTER*256 CHDSN
 
#include "fatmen/fatopts.inc"
 
      IF(IDEBFA.GE.1) THEN
         CALL DATIME(ID,IT)
         PRINT 9001,ID,IT,IS(6)
 9001 FORMAT(' FMDISK. enter at ',I6.6,1X,I4.4,I2.2)
      ENDIF
 
      IRC    = 0
      LUN    = 0
      IMODE  = IOPTW
      LCHLUN = LENOCC(CHLUN)
 
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
*     Get CHDSN
*
      CALL FMGDSN(LBANK,CHDSN,LDSN,IRC)
      CHFNFA = CHDSN(1:LDSN)
*
*     URL
*
      IURL   = INDEX(CHDSN(1:LDSN),'://')
      ISLASH = INDEX(CHDSN(1:LDSN),'/')
      IF(IURL.NE.0.AND.IURL.EQ.ISLASH-1) THEN
#if defined(CERNLIB_UNIX)
         CALL FMMSS(GENAM,CHDSN(1:LDSN),' ',IRC)
         IF(IRC.NE.0) GOTO 20
*
*     ... call mssget here ...
*
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9006) CHDSN(1:LDSN)
 9006 FORMAT(' FMDISK. invoking "mssget" for ',A)
         GOTO 20
#endif
#if !defined(CERNLIB_UNIX)
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9007) CHDSN(1:LDSN)
 9007 FORMAT(' FMDISK. URL support only available on Unix systems')
         IRC = 99
         GOTO 20
#endif
      ENDIF
 
#if defined(CERNLIB_IBMVM)
*
*     FATMEN file format (for call to FZFILE,RZFILE)
*
      CALL UHTOC(IQ(LBANK+KOFUFA+MFLFFA),4,CHFORM,4)
 
      ICFOP = 0
      IF(INDEX(CHFORM,'FP').EQ.0.AND.IOPTF.NE.0) THEN
         ICFOP = IQUEST(10)
         IF(ICFOP.EQ.2) IOPTX = 1
      ENDIF
 
      CHCOMM = 'FILEDEF       FTnnF001 DISK '
      DDNAME = 'FT00F001'
 
      IF((INDEX(CHFORM,'FX').NE.0).AND.(IOPTX.NE.0)) THEN
         CHCOMM = 'FILEDEF       VMnnF001 DISK '
         DDNAME = 'VM00F001'
      ENDIF
 
      WRITE(CHCOMM(17:18),9008) LUN
 9008       FORMAT(I2.2)
      WRITE(DDNAME(3:4),'(I2.2)') LUN
 
      IF((CHFORM(1:2).EQ.'FX'.AND.IOPTX.EQ.0.AND.ICFOP.EQ.3)
     +   .OR.(CHFORM(1:2).EQ.'EP')) THEN
         CHCOMM = 'FILEDEF       IOFILEnn     DISK '
         WRITE(CHCOMM(21:22),9008) LUN
         DDNAME = 'IOFILE00'
         WRITE(DDNAME(7:8),'(I2.2)') LUN
      ENDIF
 
      IF(LCHLUN.GT.2) CHCOMM(15:22) = CHLUN
*
*     Get disk name and link to it
*
      LSTA = INDEX(CHDSN,'<')
      IF(LSTA.NE.0) THEN
*
*     Format of CHDSN is <user.address>filename.filetype on VM
*     address defaults to 191. If field <> missing, defaults to
*     current userid.
*
*     Valid filenames:
*                     FN.FT
*                     <JAMIE>FN.FT
*                     <JAMIE.191>FN.FT
* SFS                 POOL:<JAMIE.A191>FN.FT
*
         CALL CTRANS('[','<',CHDSN,1,LDSN)
         CALL CTRANS(']','>',CHDSN,1,LDSN)
         LDOT = INDEX(CHDSN,'.')
         LBRA = INDEX(CHDSN,'>')
 
         IF((LDOT.NE.0).AND.(LDOT.LE.LBRA)) THEN
            LEND = LDOT
         ELSE
            LEND = LBRA
         ENDIF
 
         USER = CHDSN(LSTA+1:LEND-1)
         LUSR = LEND - LSTA - 1
         ADDR = '    '
 
         IF ((LDOT.NE.0).AND.(LDOT.LE.LBRA)) THEN
            ADDR= CHDSN(LDOT+1:LBRA-1)
         ENDIF
 
         LCHSFS = INDEX(CHDSN(1:LDSN),':')
         IF(LCHSFS.NE.0) THEN
            CHSFS = CHDSN(1:LCHSFS)
            IF(IDEBFA.GE.2) PRINT *,'FMDISK. SFS pool = ', CHSFS(1:
     +      LCHSFS)
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
            IF(IDEBFA.GE.-3) PRINT *,'FMDISK. username is numeric.',
     +      ' Cannot link to this userid using GIME'
            IF(IDEBFA.GE.0) PRINT *,'FMDISK. executing ', 'EXEC '
     +      //'FATGIME '//USER(1:LUSR)//' '//ADDR//CHACC
            CALL VMCMS('EXEC FATGIME '//USER(1:LUSR)//' '
     +      //ADDR// CHACC,IRC)
            IF(IRC.NE.0) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMDISK. return code from '
     +         //'FATGIME = ',IRC
               GOTO 20
            ENDIF
 
         ELSE
 
            IF(LCHSFS.EQ.0) THEN
 
               CHGIME = 'EXEC GIME '//USER(1:LUSR)//' '
     +         //ADDR// '(QUIET NONOTICE STACK)'
               CALL SBIT1(IHOWFA,JLOCFA)
            ELSE
               CHGIME = 'EXEC GIME '// CHSFS(1:LCHSFS)//USER(1:LUSR)//
     +         '.'//ADDR// '(QUIET NONOTICE STACK)'
               CALL SBIT1(IHOWFA,JSFSFA)
            ENDIF
 
            CALL CSQMBL(CHGIME,1,80)
            LCHG = LENOCC(CHGIME)
 
            IF(IDEBFA.GE.0) PRINT *,'FMDISK. executing ', CHGIME(1:
     +      LCHG)
            CALL VMCMS(CHGIME(1:LCHG),IRC)
 
            IF(IRC.GT.4) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMDISK. return code from GIME ='
     +         //' ',IRC
               GOTO 20
            ENDIF
 
         ENDIF
 
         CALL VMRTRM(CHLINE,LENGTH)
         CHLETT = CHLINE(1:1)
*
*     Use mode 4 for all CMS files, except RECFM F
*     N.B. files in CMS format V will be incorrectly handled!
*     To be read correctly, RECFM=U
*
         IF(CHFORM(1:2).EQ.'RZ') THEN
            CHLETT(2:2) = '6'
         ELSE
            CHLETT(2:2) = '4'
         ENDIF
 
         IF(RECFM(1:1).EQ.'U') CHLETT(2:2) = '1'
         IF(IDEBFA.GE.0) WRITE(LPRTFA,9009) USER,ADDR,CHLETT
 9009 FORMAT('FMDISK. linked to ',A8,' address ',A3,' mode ',A4)
      ELSE
         CHLETT = '*'
      ENDIF
 
      LDOT = INDEXB(CHDSN,'.')
      CHDSN(LDOT:LDOT) = ' '
      CHCOMM = CHCOMM(1:30) // CHDSN(LBRA+1:LDSN) // ' ' // CHLETT
      LENCOM = LENOCC(CHCOMM)
*
*     Don't add DCB if it is missing...
*
      WRITE(CHDCB,9010) RECFM,LRECL,LBLOCK
 9010       FORMAT(' RECFM ',A4,' LRECL ',I5,' BLOCK ',I5)
      IF((LENOCC(RECFM).GT.0).AND. (LRECL.NE.0.OR.LBLOCK.NE.0)) THEN
*        IF(IMODE.NE.0) THEN
         CHCOMM = CHCOMM(1:LENOCC(CHCOMM)) // ' ( ' // CHDCB
      ENDIF
 
      LENCOM = LENOCC(CHCOMM)
      IF(IDEBFA.GE.0) PRINT *,'FMDISK. running ',CHCOMM(1:LENCOM)
 
      CALL VMCMS(CHCOMM(1:LENCOM),IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMDISK. return code ',IRC, ' from '
     +   //'FILEDEF'
         GOTO 20
      ENDIF
*
*     Set the disk and access modes (for FMCLOS)
*
      CHMODE(LUN) = CHLETT(1:1)
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Find disk with most space
*
      IF(IMODE.NE.0) THEN
         CALL FMXDSK(CHFNFA,CHDSN,IRC)
         LDSN = LENOCC(CHDSN)
      ELSE
         CALL UHTOC(IQ(LBANK+KOFUFA+MHSNFA),4,CHHOST,NHSNFA)
         LHOST = LENOCC(CHHOST)
*
*        Call FAINQR to build full DSN
*
         CALL FAINQR(CHDSN,CHHOST(1:LHOST),CHFILE,IRC)
         IF(IRC.EQ.0) THEN
            CHDSN  = CHFILE
            LDSN   = LENOCC(CHFILE)
            CHFNFA = ' '
            CHFNFA = CHDSN(1:LDSN)
*
*     Monitoring
*
            IF(IQUEST(1).EQ.1) THEN
               CALL SBIT1(IHOWFA,JLOCFA)
            ELSEIF(IQUEST(1).EQ.12) THEN
               CALL SBIT1(IHOWFA,JMSCFA)
            ELSEIF(IQUEST(1).EQ.13) THEN
               CALL SBIT1(IHOWFA,JDFSFA)
            ELSEIF(IQUEST(1).EQ.14) THEN
               CALL SBIT1(IHOWFA,JDECFA)
            ELSEIF(IQUEST(1).EQ.15) THEN
               CALL SBIT1(IHOWFA,JCSPFA)
            ELSEIF(IQUEST(1).EQ.16) THEN
               CALL SBIT1(IHOWFA,JFPKFA)
            ELSEIF(IQUEST(1).EQ.22) THEN
               CALL SBIT1(IHOWFA,JNFSFA)
            ENDIF
         ENDIF
      ENDIF
*
*     Just assign the relevant logical name...
*
      FORLUN = 'FOR00N'
      WRITE(FORLUN(4:6),'(I3.3)') LUN
      IF (LUN .EQ. 0) FORLUN = CHLUN
      LFLUN = LENOCC(FORLUN)
      ISTAT = LIB$SET_LOGICAL(FORLUN(1:LFLUN),
     +                     CHDSN(1:LDSN),'LNM$JOB',,)
#include "fatmen/fatvaxrc.inc"
      IF(IDEBFA.GE.2) WRITE(LPRTFA,*) 'Assign ',
     +   CHDSN(1:LDSN),' ',FORLUN(1:LFLUN)
*
*     Protect against logical names in the process table
*
      CALL FMGTLG(FORLUN(1:LFLUN),EQUNAM,'LNM$PROCESS',IC)
      IF(IC.EQ.0) THEN
         IF(IDEBFA.GE.0) PRINT 9011,FORLUN(1:LFLUN),EQUNAM(1:IS(1))
 9011 FORMAT(' FMDISK. warning - conflicting logical name for ',A,/,
     +       ' = ',A,/,
     +       ' - deleted from process table')
         ISTAT = LIB$DELETE_LOGICAL(FORLUN(1:LFLUN),'LNM$PROCESS')
      ENDIF
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
*     Fold to lower case
*
      CALL CUTOL(FORLUN(1:LFLUN))
*
*     Remove existing link, if any
*
      ILINK = UNLINKF(FORLUN(1:LFLUN))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
      CALL CUTOL(CHDSN)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_SHIFT))&&(!defined(CERNLIB_APOLLO))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'ln for logical unit ',
     +   FORLUN(1:LFLUN),' CHDSN = ',CHDSN(1:LDSN)
      IRC = SYSTEMF('ln -sff '//CHDSN(1:LDSN)//' '
     +            //FORLUN(1:LFLUN))
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.-3) WRITE(LPRTFA,9012) IRC
 9012 FORMAT(' FMDISK. return code ',I6,' from ln')
         GOTO 20
      ENDIF
      IF(CHDSN(1:4).EQ.'/afs') CALL SBIT1(IHOWFA,JAFSFA)
#endif
#if defined(CERNLIB_APOLLO)
      IC = SYSTEMF('ln -sff '//CHDSN(1:LDSN)//' '
     +            //FORLUN(1:LFLUN))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'ln for logical unit ',
     +   FORLUN(1:LFLUN),' CHDSN = ',CHDSN(1:LDSN)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CRAY))
      IC = SYSTEMF('assign -a '//CHDSN(1:LDSN)//' '
     +            //FORLUN(1:LFLUN))
      IF (IDEBFA .GE. 2)    WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' CHDSN = ',CHDSN(1:LDSN)
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_SHIFT))
*
*     Remove existing link, if any
*
      ILINK = UNLINKF(FORLUN(1:LFLUN))
 
      IF(LUN.LT.10) THEN
         WRITE(FORLUN,'(I1)') LUN
      ELSE
         WRITE(FORLUN,'(I2)') LUN
      ENDIF
 
      LFLUN = LENOCC(FORLUN)
 
      CALL CTRANS('<','[',CHDSN,1,LDSN)
      CALL CTRANS('>',']',CHDSN,1,LDSN)
      ILSQB = INDEX(CHDSN(1:LDSN),'[')
      IRSQB = INDEX(CHDSN(1:LDSN),']')
 
      IF(ILSQB.NE.0) THEN
 
         IF(IDEBFA.GE.0) PRINT *,'FMDISK. SHIFT POOL file...'
 
         IDOT   = INDEX(CHDSN(1:IRSQB),'.')
         SHPOOL = CHDSN(2:IDOT-1)
         SHUSER = CHDSN(IDOT+1:IRSQB-1)
         ISTART = IRSQB+1
         IEND   = LDSN
         IF (IDEBFA.GE.0) WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' pool = ',SHPOOL, ' user = ',SHUSER,' CHDSN ='
     +   //' ',CHDSN(ISTART:IEND)
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
 
         IF(IDEBFA.GE.1) PRINT *,'FMDISK. using temporary file ',
     +      SHUNAM(1:LUNAM)
*
*     Issue SFGET to obtain full shift pathname
*
         CALL FMFGET(SHPOOL,SHUSER,CHDSN(ISTART:IEND),SHFNAM,IMODE,'D',
     +               IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMDISK. return code ',IRC,
     +         ' from FMFGET'
            GOTO 20
         ENDIF
         LFNAM = IS(1)
*
*     Perform assign
*
         IF(IDEBFA.GE.0) PRINT *,'FMDISK. issuing ',
     +      'assign '//SHFNAM(1:LFNAM)//' '//FORLUN(1:LFLUN)//'  '
         IRC = SYSTEMF('assign '//SHFNAM(1:LFNAM)//' '
     +         //FORLUN(1:LFLUN)//'  ')
 
      ELSE
 
         IF(IDEBFA.GE.0) PRINT *,'FMDISK. SHIFT private file...'
         IF (IDEBFA.GE.0) WRITE(LPRTFA,*) 'Assign for logical unit ',
     +   FORLUN(1:LFLUN),' CHDSN = ',CHDSN(1:LDSN)
         IC = SYSTEMF('assign '//CHDSN(1:LDSN)//' '//
     +               FORLUN(1:LFLUN))
         SHFNAM = CHDSN(1:LDSN)
         LFNAM  = LDSN
         IF(IC.NE.0) THEN
            PRINT *,'FMDISK. return code ',IC,' from SFGET'
            GOTO 20
         ENDIF
      ENDIF
#endif
   20 CONTINUE
 
      IF(IDEBFA.GE.1) THEN
         CALL DATIME(ID,IT)
         PRINT 9013,ID,IT,IS(6)
 9013 FORMAT(' FMDISK. exit at ',I6.6,1X,I4.4,I2.2)
      ENDIF
 
      END
