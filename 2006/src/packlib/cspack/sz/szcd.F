*
* $Id: szcd.F,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
*
* $Log: szcd.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZCD(PATH,IRC)
      CHARACTER*(*) PATH
      CHARACTER*80  CHPATH,CHGIME
      CHARACTER*20  CHOPT,CHPASS
#if defined(CERNLIB_IBMMVS)
      LPATH = LENOCC(PATH)
      CALL XZPREF(PATH(1:LPATH),IRC)
      CALL XZRPRE(CHPATH,LPATH)
#endif
#if defined(CERNLIB_IBMVM)
      DATA NENTRY/0/
      CHARACTER*80  LINE
      CHARACTER*8   CHUSER
      INTEGER FMUSER
      NENTRY = 1
      LPATH  = LENOCC(PATH)
*
*     Look for a blank in the path name - this indicates that there
*     is a password and option following
*
      LBLANK = INDEX(PATH(1:LPATH),' ')
*
*     Translate user.address to user address
*
      IDOT   = INDEX(PATH(1:LPATH),'.')
      IF(IDOT.NE.0) PATH(IDOT:IDOT) = ' '
 
      IF(LBLANK.EQ.0) THEN
         CALL VMCMS('EXEC GIME '//PATH(1:LPATH)//' (STACK',IRC)
      ELSE
*
*     Strip off password and options
*
         JBLANK = INDEXB(PATH(1:LPATH),' ')
         CHPASS = PATH(LBLANK+1:JBLANK-1)
         CHOPT  = PATH(JBLANK+1:LPATH)
         LPATH  = LBLANK - 1
         LPASS  = JBLANK - LBLANK - 1
         IF(INDEX(CHOPT,'W').NE.0) THEN
            CHGIME = ' * '//CHPASS(1:LPASS)//' (MR STACK'
         ELSE
            CHGIME = ' * '//CHPASS(1:LPASS)//' (RR STACK'
         ENDIF
 
         CALL VMCMS('EXEC GIME '//PATH(1:LPATH)//
     +              CHGIME,IRC)
 
      ENDIF
*
*     Return code 4 is only a warning - existing link exists
*
      IF(IRC.GT.4) THEN
         RETURN
         ELSE
         CALL VMRTRM(LINE,IRC)
         CALL VMCMS('EXEC SWAPMODE A '//LINE(1:1),IRC)
         CALL CZPUTA('2Remote directory changed to '//PATH(1:LPATH),IRC)
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
*
*     Options?
*
      CHOPT  = ' '
      LBLANK = INDEX(CHPATH(1:LPATH),' ')
      IF(LBLANK.NE.0) THEN
         CHOPT  = CHPATH(LBLANK+1:LPATH)
         CHPATH = PATH(1:LBLANK-1)
         LPATH  = LBLANK - 1
      ENDIF
      IF(INDEX(CHOPT,'C').EQ.0) CALL CUTOL(CHPATH)
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
         CALL CZPUTA('2Remote directory changed to '//CHPATH(1:LPATH),
     +               IRC)
         ELSE
         CALL CZPUTA('2Error setting remote directory',IRC)
         IRC = 1
         ENDIF
      RETURN
#endif
 
      ENTRY SZPWD(IRC)
#if defined(CERNLIB_IBMMVS)
      CALL XZRPRE(CHPATH,LPATH)
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
      CALL CZPUTA('2Current working directory is '//CHPATH(1:LPATH),IRC)
      END
