*
* $Id: fainqr.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: fainqr.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAINQR(DSNAME,CHHOST,CHNAME,IRC)
*CMZ :          17/10/91  15.00.35  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/10/91
      CHARACTER*(*) DSNAME,CHHOST,CHNAME
      CHARACTER*255 DSN,CHFILE
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#include "zebra/quest.inc"
 
      PARAMETER     (NPOSS=4)
      CHARACTER*8    CHPOSS(NPOSS)
#if defined(CERNLIB_VAXVMS)
      CHARACTER*255 CHDFS,DFSNAM
      INCLUDE       '($RMSDEF)'
      INTEGER       FAFNDF
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CHARACTER*255 CHNFS
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*8   USER,ADDR
      CHARACTER*16  CHSFS
      CHARACTER*80  CHGIME,CHLINE
      CHARACTER*1   MODE
#endif
#if defined(CERNLIB_SHIFT)
      CHARACTER*16  SHPOOL,SHUSER
      CHARACTER*11  SHLINK
      CHARACTER*255 CHDSN
#endif
      LOGICAL       IEXIST
      SAVE          CHPOSS
 
      DATA          CHPOSS(1)/'OSM'/,
     +              CHPOSS(2)/'UNITREE'/,
     +              CHPOSS(3)/'ADSM'/,
     +              CHPOSS(4)/'EMASS'/
*
*     Return access method in IQUEST(1)
*     IQUEST(1) = 0 : unknown
*                 1 : VM/CMS mini-disk
*                 2 : VM/CMS SFS
*                11 : VAX/VMS disk
*                12 : VAXcluster disk
*                13 : DFS (DECnet)
*                14 : DECnet
*                15 : CSPACK (zserv)
*                16 : FPACK
*                21 : Unix disk
*                22 : NFS (e.g. $VARIABLE/file)
*                23 : AFS
*                24 : Shift pool file
*                25 : Shift private file
*                31 : Lachman OSM file
*                32 : Unitree file
*                33 : ADSM file
*                34 : E-MASS file
*
*     CHFILE contains on exit the fully qualified filename (VMS, Unix)
*
      IRC    = 0
      IACC   = 0
 
      LDSN   = LENOCC(DSNAME)
      DSN    = DSNAME(1:LDSN)
      LHOST  = LENOCC(CHHOST)
*
*     URL?
*
      IURL   = INDEX(DSN(1:LDSN),'://')
      ISLASH = INDEX(DSN(1:LDSN),'/')
      IF(IURL.NE.0.AND.IURL.EQ.ISLASH-1) THEN
         IT  = ICNTHU(DSN(1:IURL-1),CHPOSS,NPOSS)
         IF(IT.NE.0) IACC=30+IT
#if defined(CERNLIB_UNIX)
*
*     ... issue mssquery ...
*
#endif
#if !defined(CERNLIB_UNIX)
         IRC = -1
#endif
         GOTO 99
      ENDIF
 
#if defined(CERNLIB_IBMVM)
*
*     Get disk name and link to it
*
      MODE  = ' '
      LSTA = INDEX(DSN,'<')
      IF(LSTA.NE.0) THEN
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
 
         IF((LDOT.NE.0).AND.(LDOT.LE.LBRA)) THEN
            LEND = LDOT
         ELSE
            LEND = LBRA
         ENDIF
 
         USER = DSN(LSTA+1:LEND-1)
         LUSR = LEND - LSTA - 1
         ADDR = '    '
 
         IF((LDOT.NE.0).AND.(LDOT.LE.LBRA)) THEN
            ADDR= DSN(LDOT+1:LBRA-1)
         ENDIF
 
         LCHSFS = INDEX(DSN(1:LDSN),':')
         IF(LCHSFS.NE.0) THEN
            CHSFS = DSN(1:LCHSFS)
            IF(IDEBFA.GE.2) PRINT *,'FAINQR. SFS pool = ',
     +         CHSFS(1:LCHSFS)
         ENDIF
 
         IF(LCHSFS.EQ.0) THEN
            IACC = 1
            CHGIME = 'EXEC GIME '//USER(1:LUSR)//' '//ADDR//
     +      '(QUIET NONOTICE STACK)'
         ELSE
            IACC = 2
            CHGIME = 'EXEC GIME '//
     +      CHSFS(1:LCHSFS)//USER(1:LUSR)//'.'//ADDR//
     +      '(QUIET NONOTICE STACK)'
         ENDIF
 
         CALL CSQMBL(CHGIME,1,80)
         LCHG   = LENOCC(CHGIME)
 
         IF(IDEBFA.GE.1) PRINT *,'FAINQR. executing ',
     +              CHGIME(1:LCHG)
         CALL VMCMS(CHGIME(1:LCHG),IGIME)
 
         IF(IGIME.GT.4) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FAINQR return code from GIME = ',IGIME
            GOTO 99
         ENDIF
 
         CALL VMRTRM(CHLINE,LENGTH)
         MODE = CHLINE(1:1)
 
         CHFILE = '/' // DSN(LBRA+1:LDSN) // ' ' // MODE
         LDSN = LDSN + 3 - LBRA
 
      ENDIF
 
      CALL CTRANS('.',' ',CHFILE,1,LDSN)
 
#endif
#if defined(CERNLIB_IBMMVS)
      CHFILE = '/'//DSN(1:LDSN)
      LDSN = LDSN + 1
#endif
#if defined(CERNLIB_VAXVMS)
*
*     Check hostname
*
      IACC = 11
      IF(LHOST.GT.0) THEN
         CALL FMVAXC(CHHOST(1:LHOST),ICODE)
         IACC = 11 + ICODE
      ENDIF
*
*     Check DFS access
*
      IF(ICODE.EQ.2) THEN
*
*     First, get full file name
*
         ICONT = 0
*        IFIND = LIB$FIND_FILE(DSN(1:LDSN),CHDFS,ICONT)
         IFIND = FAFNDF(DSN(1:LDSN),CHDFS,ICONT)
         IEND  = LIB$FIND_FILE_END(ICONT)
         IF(IFIND.EQ.RMS$_SUC) THEN
            LCHDSN = INDEX(CHDFS,':') - 1
            IF(LCHDSN.GT.0) THEN
               CALL FMGTLG(CHDFS(1:LCHDSN),DFSNAM,'LNM$SYSTEM',JRC)
               IF((JRC.EQ.0).AND.(INDEX(DFSNAM,'DFSC').NE.0)) IACC = 13
            ENDIF
         ELSE
            INQUIRE(FILE=CHHOST(1:LHOST)//'::'//DSN(1:LDSN),
     +              EXIST=IEXIST)
            IF(IEXIST) THEN
               IACC = 14
               CHFILE = CHHOST(1:LHOST)//'::'//DSN(1:LDSN)
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      IACC = 21
      IF(DSN(1:5).EQ.'/afs/') IACC = 23
#endif
#if defined(CERNLIB_SHIFT)
*
*     Check if link already exists...
*
      CHDSN = DSN(1:LDSN)
      SHLINK = 'FATMEN_LINK'
      INQUIRE(FILE=SHLINK,EXIST=IEXIST)
      IF(IEXIST) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FAINQR. removing existing symbolic link...'
         IRC = SYSTEMF('rm '//SHLINK)
      ENDIF
 
      CALL CTRANS('<','[',DSN,1,LDSN)
      CALL CTRANS('>',']',DSN,1,LDSN)
      ILSQB = INDEX(DSN(1:LDSN),'[')
      IRSQB = INDEX(DSN(1:LDSN),']')
 
      IF(ILSQB.NE.0) THEN
 
         IF(IDEBFA.GE.1) PRINT *,'FAINQR. SHIFT POOL file...'
 
         IACC   = 24
         IDOT   = INDEX(DSN(1:IRSQB),'.')
         SHPOOL = DSN(2:IDOT-1)
         SHUSER = DSN(IDOT+1:IRSQB-1)
         ISTART = IRSQB+1
         IEND   = LDSN
      IF (IDEBFA.GE.0)    PRINT *,'FAINQR. Assign for logical unit ',
     +   SHLINK,' pool = ',SHPOOL,
     +   ' user = ',SHUSER,' dsn = ',DSN(ISTART:IEND)
         IRC = SYSTEMF('assign ` sfget -k -p '//SHPOOL//
     +                  ' -u '//SHUSER// ' '//CHDSN(ISTART:IEND)//'  `
     +                  '//SHLINK//'  ')
         IF(IRC.NE.0) THEN
            PRINT *,'FAINQR. return code ',IRC,' from SFGET'
            GOTO 99
          ENDIF
      ELSE
 
         IF(IDEBFA.GE.1) PRINT *,'FAINQR. SHIFT private file...'
         IACC   = 25
         IRC = SYSTEMF('assign '//CHDSN(1:LDSN)//' '//
     +               SHLINK)
         IF(IRC.NE.0) THEN
            PRINT *,'FAINQR. return code ',IRC,' from SFGET'
            GOTO 99
         ENDIF
      ENDIF
 
      DSN  = SHLINK
      LDSN = 11
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_SHIFT))
*
*     Expand any environmental variable
*
      IF(DSN(1:1).EQ.'$') THEN
         IACC = 22
         LEND = INDEX(DSN,'/')
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
         CALL GETENVF(DSN(2:LEND-1),CHNFS)
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_SHIFT))
         CALL FMGTLG(DSN(2:LEND-1),CHNFS,'LNM$SYSTEM',IRC)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_SHIFT))
         IF(IS(1).EQ.0) THEN
            IF(IDEBFA.GE.3) PRINT *,'FAINQR. cannot expand ',
     +         'environmental variable/logical name ',DSN(2:LEND-1)
            IRC = 1
            GOTO 99
         ENDIF
         LCHNFS = IS(1)
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
         CHFILE = CHNFS(1:IS(1)) // DSN(LEND:LDSN)
         LDSN   = IS(1) + LDSN - LEND + 1
#endif
#if (defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_SHIFT))
*
*     If there is more than one slash in file name
*     assume that the intervening elements are directory names
*
         JSLASH              = INDEXB(DSN(1:LDSN),'/')
         IF(JSLASH.EQ.LEND) THEN
            CHNFS(LCHNFS+1:) = DSN(LEND+1:LDSN)
            LCHNFS           = LCHNFS + LDSN - LEND
         ELSE
            CHNFS(LCHNFS+1:) = '[' // DSN(LEND+1:JSLASH-1)
     +                         // ']' // DSN(JSLASH+1:LDSN)
            LCHNFS           = LCHNFS + LDSN - LEND + 1
            CALL CTRANS('/','.',CHNFS,1,LCHNFS)
         ENDIF
         CHFILE = CHNFS(1:LCHNFS)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_SHIFT))
      ELSE
         IF(IACC.NE.14) CHFILE = DSN
      ENDIF
 
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
      CALL CUTOL(CHFILE)
#endif
      LFILE = LENOCC(CHFILE)
      IF(IDEBFA.GE.2) PRINT *,'FAINQR. issuing inquire for ',
     +   CHFILE(1:LFILE)
      INQUIRE(FILE=CHFILE(1:LFILE),EXIST=IEXIST,NAME=CHNAME)
      IF(.NOT.IEXIST) IRC = 1
 
#if defined(CERNLIB_IBMVM)
*
*     Don't drop disks that were already accessed, our A disk etc.
*
      IF(IGIME.NE.4.AND.MODE.NE.'A'.AND.MODE.NE.' ')
     +   CALL VMCMS('EXEC DROP '//MODE//' (QUIET',IRT)
#endif
99    CONTINUE
      IQUEST(1) = IACC
      END
