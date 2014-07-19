*
* $Id: xzlcd.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzlcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZLCD(PATH,IRC)
      CHARACTER*(*) PATH
      CHARACTER*80  CHPATH
#if defined(CERNLIB_IBMMVS)
      LPATH = LENOCC(PATH)
      CALL XZPREF(PATH(1:LPATH),IRC)
#endif
#if defined(CERNLIB_IBMVM)
      DATA NENTRY/0/
      CHARACTER*80  LINE
      CHARACTER*8   CHUSER
      INTEGER FMUSER
      NENTRY = 1
      LPATH  = LENOCC(PATH)
      IF(LPATH.EQ.0) THEN
         IRC = 1
         RETURN
      ENDIF
      IDOT   = INDEX(PATH(1:LPATH),'.')
      IF(IDOT.NE.0) PATH(IDOT:IDOT) = ' '
      CALL VMCMS('EXEC GIME '//PATH(1:LPATH)//' (STACK',IRC)
*
*     Return code 4 is only a warning - existing link exists
*
      IF(IRC.GT.4) THEN
         RETURN
      ELSE
         CALL VMRTRM(LINE,IRC)
         CALL VMCMS('EXEC SWAPMODE A '//LINE(1:1),IRC)
         PRINT *,'Local directory changed to ',PATH(1:LPATH)
         CHPATH = PATH(1:LPATH)
      ENDIF
      RETURN
 
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      INTEGER SETDEF
      INTEGER CHDIR
      INTEGER CHDIRF
      COMMON/SLATE/IS(40)
      LPATH  = LENOCC(PATH)
      CHPATH = PATH(1:LPATH)
#endif
#if defined(CERNLIB_UNIX)
      CALL CUTOL(CHPATH)
#endif
#if defined(CERNLIB_MIPS)
      LPATH = LPATH + 1
      CHPATH(LPATH:LPATH) = CHAR(0)
#endif
#if defined(CERNLIB_UNIX)
      IRC    = CHDIRF(CHPATH(1:LPATH))
#endif
#if defined(CERNLIB_VAXVMS)
      IRC    = SETDEF(CHPATH(1:LPATH))
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)||defined(CERNLIB_IBMMVS)
      IF(IRC.EQ.0) THEN
         PRINT *,'Local directory changed to ',PATH(1:LPATH)
      ELSE
         Print *,'Error setting remote directory'
         ENDIF
      RETURN
#endif
 
      ENTRY XZLPWD(PATH,IRC)
#if defined(CERNLIB_IBMMVS)
      IF(NENTRY.EQ.0) PATH = ' '
      CALL XZRPRE(PATH,IRC)
#endif
#if defined(CERNLIB_IBMVM)
      IF(NENTRY.EQ.0) THEN
         CALL CFILL(' ',CHPATH,1,80)
         ISTAT  = FMUSER(CHUSER)
         CHPATH = CHUSER
         LPATH  = LENOCC(CHPATH)
         CHPATH(LPATH+2:LPATH+4) = '191'
         LPATH  = LPATH + 4
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      CALL GETWDF(CHPATH)
      LPATH = IS(1)
#endif
#if defined(CERNLIB_VAXVMS)
      CHPATH = ' '
      CALL GETDEF(CHPATH)
      LPATH = LENOCC(CHPATH)
#endif
      Print *,'Current working directory is ',CHPATH(1:LPATH)
      PATH  = CHPATH(1:LPATH)
      END
 
