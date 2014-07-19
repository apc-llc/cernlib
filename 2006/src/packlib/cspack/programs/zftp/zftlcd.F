*
* $Id: zftlcd.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zftlcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTLCD
      CHARACTER*80 PATH,CHPATH
      CHARACTER*20 CHOPT
      CHARACTER*80 CHPASS
#if defined(CERNLIB_IBMVM)
      DATA NENTRY/0/
      CHARACTER*80  LINE,CHGIME
      CHARACTER*8   CHUSER
      INTEGER FMUSER
      CALL KUGETS(PATH,LPATH)
      IF(LPATH.EQ.0) GOTO 10
*
*     For remote connections to VM systems, cater for write links
*
      CALL KUGETC(CHPASS,LPASS)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      IOPTR = INDEX(CHOPT(1:LCHOPT),'R')
      IOPTW = INDEX(CHOPT(1:LCHOPT),'W')
 
      IF((IOPTR.NE.0).AND.(IOPTW.NE.0)) THEN
         PRINT *,'ZFTLCD. only one of R(ead) and W(rite)',
     +           'maybe specified'
         RETURN
      ENDIF
 
      IF((IOPTR+IOPTW.NE.0).AND.(LPASS.EQ.0)) THEN
         PRINT *,'ZFTLCD. password must be given with ',
     +           'R or W options'
         RETURN
      ENDIF
 
      NENTRY = 1
      IDOT   = INDEX(PATH(1:LPATH),'.')
      IF(IDOT.NE.0) PATH(IDOT:IDOT) = ' '
 
      IF(IOPTW.NE.0) THEN
         CHGIME = ' * '//CHPASS(1:LPASS)//' (MR STACK'
      ELSEIF(IOPTR.NE.0) THEN
         CHGIME = ' * '//CHPASS(1:LPASS)//' (RR STACK'
      ELSE
         CHGIME = ' (STACK'
      ENDIF
 
      CALL VMCMS('EXEC GIME '//PATH(1:LPATH)//
     +           CHGIME,IRC)
*
*     Return code 4 is only a warning - existing link exists
*
      IF(IRC.GT.4) THEN
         RETURN
      ELSE
         CALL VMRTRM(LINE,IRC)
         CALL VMCMS('EXEC SWAPMODE A '//LINE(1:1),IRC)
         PRINT *,'Local directory changed to ',PATH(1:LPATH)
         CHPATH = PATH
      ENDIF
      RETURN
 
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      INTEGER SETDEF
      INTEGER CHDIR
      INTEGER CHDIRF
      COMMON/SLATE/IS(40)
      CALL KUGETS(PATH,LPATH)
      IF(LPATH.EQ.0) GOTO 10
      CALL KUGETC(CHPASS,LPASS)
      CALL KUGETC(CHOPT,LCHOPT)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
#endif
#if defined(CERNLIB_UNIX)
      IF(INDEX(CHOPT,'C').EQ.0) CALL CUTOL(PATH)
#endif
#if defined(CERNLIB_MIPS)
      LPATH = LPATH + 1
      PATH(LPATH:LPATH) = CHAR(0)
#endif
#if defined(CERNLIB_UNIX)
      IRC    = CHDIRF(PATH(1:LPATH))
#endif
#if defined(CERNLIB_VAXVMS)
      IRC    = SETDEF(PATH(1:LPATH))
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      IF(IRC.EQ.0) THEN
         PRINT *,'Local directory changed to ',PATH(1:LPATH)
         ELSE
         PRINT *,'Error setting local directory'
         ENDIF
      RETURN
#endif
#if defined(CERNLIB_IBMMVS)
      IF(NENTRY.EQ.0) THEN
         CHPATH = ' '
      ENDIF
      CALL XZRPRE(CHPATH,LPATH)
#endif
10    CONTINUE
#if defined(CERNLIB_IBM)
      IF(NENTRY.EQ.0) THEN
         CALL CFILL(' ',CHPATH,1,80)
         ISTAT  = FMUSER(CHPATH)
         LPATH  = LENOCC(CHPATH)
         CHPATH(LPATH+2:LPATH+4) = '191'
         LPATH  = LPATH + 4
      ENDIF
      PATH  = CHPATH
      LPATH = LENOCC(PATH)
#endif
#if defined(CERNLIB_UNIX)
      CALL GETWDF(PATH)
      LPATH = IS(1)
#endif
#if defined(CERNLIB_VAXVMS)
      PATH = ' '
      CALL GETDEF(PATH)
      LPATH = LENOCC(PATH)
#endif
      PRINT *,'Local directory is ',PATH(1:LPATH)
*
  99  END
 
