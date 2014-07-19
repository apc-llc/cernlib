*
* $Id: faeras.F,v 1.1.1.1 1996/03/07 15:18:07 mclareni Exp $
*
* $Log: faeras.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:07  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAERAS(DSNAME,IRC)
*CMZ :          17/10/91  15.00.35  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/10/91
      CHARACTER*(*) DSNAME
      CHARACTER*255 DSN,CHFILE
      INTEGER       SYSTEMF
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
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
 
      IRC = 0
 
      LDSN = LENOCC(DSNAME)
      DSN  = DSNAME(1:LDSN)
 
#if defined(CERNLIB_IBMVM)
*
*     Get disk name and link to it
*
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
            IF(IDEBFA.GE.2) PRINT *,'FAERAS. SFS pool = ',
     +         CHSFS(1:LCHSFS)
         ENDIF
 
         IF(LCHSFS.EQ.0) THEN
 
            CHGIME = 'EXEC GIME '//USER(1:LUSR)//' '//ADDR//
     +      '(QUIET NONOTICE STACK)'
         ELSE
            CHGIME = 'EXEC GIME '//
     +      CHSFS(1:LCHSFS)//USER(1:LUSR)//'.'//ADDR//
     +      '(QUIET NONOTICE STACK)'
         ENDIF
 
         CALL CSQMBL(CHGIME,1,80)
         LCHG   = LENOCC(CHGIME)
 
         IF(IDEBFA.GE.0) PRINT *,'FAERAS. executing ',
     +              CHGIME(1:LCHG)
         CALL VMCMS(CHGIME(1:LCHG),IRC)
 
         IF(IRC.GT.4) THEN
            IF(IDEBFA.GE.0)
     +      PRINT *,'FAERAS return code from GIME = ',IRC
            RETURN
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
#if defined(CERNLIB_SHIFT)
*
*     Check if link already exists...
*
      CHDSN = DSN(1:LDSN)
      SHLINK = 'FATMEN_LINK'
      INQUIRE(FILE=SHLINK,EXIST=IEXIST)
      IF(IEXIST) THEN
         IF(IDEBFA.GE.0)
     +   PRINT *,'FAERAS. removing existing symbolic link...'
         IRC = SYSTEMF('rm '//SHLINK)
      ENDIF
 
      CALL CTRANS('<','[',DSN,1,LDSN)
      CALL CTRANS('>',']',DSN,1,LDSN)
      ILSQB = INDEX(DSN(1:LDSN),'[')
      IRSQB = INDEX(DSN(1:LDSN),']')
 
      IF(ILSQB.NE.0) THEN
 
         IF(IDEBFA.GE.1) PRINT *,'FAERAS. SHIFT POOL file...'
 
         IDOT   = INDEX(DSN(1:IRSQB),'.')
         SHPOOL = DSN(2:IDOT-1)
         SHUSER = DSN(IDOT+1:IRSQB-1)
         ISTART = IRSQB+1
         IEND   = LDSN
      IF (IDEBFA.GE.0)    PRINT *,'FAERAS. Assign for logical unit ',
     +   SHLINK,' pool = ',SHPOOL,
     +   ' user = ',SHUSER,' dsn = ',DSN(ISTART:IEND)
         IRC = SYSTEMF('assign ` sfget -k -p '//SHPOOL//
     +                  ' -u '//SHUSER// ' '//CHDSN(ISTART:IEND)//'  `
     +                  '//SHLINK//'  ')
         IF(IRC.NE.0) THEN
            PRINT *,'FAERAS. return code ',IRC,' from SFGET'
            RETURN
          ENDIF
      ELSE
 
         IF(IDEBFA.GE.1) PRINT *,'FAERAS. SHIFT private file...'
         IRC = SYSTEMF('assign '//CHDSN(1:LDSN)//' '//
     +               SHLINK)
         IF(IRC.NE.0) THEN
            PRINT *,'FAERAS. return code ',IRC,' from SFGET'
            RETURN
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
            IF(IDEBFA.GE.3) PRINT *,'FAERAS. cannot expand ',
     +         'environmental variable/logical name ',DSN(2:LEND-1)
            IRC = 1
            RETURN
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
         CHFILE = DSN
      ENDIF
 
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SHIFT))
      CALL CUTOL(CHFILE)
#endif
      INQUIRE(FILE=CHFILE(1:LDSN),EXIST=IEXIST)
      IF(.NOT.IEXIST) THEN
         IRC = 1
         IF(IDEBFA.GE.-3) PRINT *,'FAERAS. file ',
     +      CHFILE(1:LDSN),' does not exist'
      ELSE
         IF(IDEBFA.GE.1) PRINT *,'FAERAS. removing disk file ',
     +      CHFILE(1:LDSN)
#if defined(CERNLIB_IBMVM)
         CALL VMCMS('ERASE '//CHFILE(2:LDSN),IRC)
         IF(IRC.NE.0) PRINT *,'FAERAS. return code ',IRC,
     +      ' from ERASE command'
#endif
#if defined(CERNLIB_IBMMVS)
#endif
#if defined(CERNLIB_VAXVMS)
         ISTAT = LIB$DELETE_FILE(CHFILE(1:LDSN),,,,,,,,)
#include "fatmen/fatvaxrc.inc"
#endif
#if defined(CERNLIB_UNIX)
         IRC   = SYSTEMF('rm '//CHFILE(1:LDSN))
         IF(IRC.NE.0) PRINT *,'FAERAS. return code ',IRC,
     +      ' from rm command'
#endif
      ENDIF
 
#if defined(CERNLIB_IBMVM)
      IF(MODE.NE.'A') CALL VMCMS('EXEC DROP '//MODE,IRET)
#endif
      END
