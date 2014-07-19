*
* $Id: cdacl.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdacl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDACL(LUNACL,CHPREF,IOPTRR,IOPTWW,CHOPT,IRC)
*     =======================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDACL (LUNACL, CHPREF, IOPTRR*, IOPTWW*, CHOPT, IRC*)   *
*                                                                      *
*   Check if user has read or write access to database                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUNACL   Unit on which names file is to be read                  *
*     CHPREF   Two character database prefix                           *
*     IOPTRR   0 if no read access                                     *
*     IOPTWW   0 if no write access                                    *
*     CHOPT    Options                                                 *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by CDOPEN                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  ? : Other errors as for NAMEFD                      *
*                                                                      *
*     :nick.ch                                                         *
*       :file./hepdb/cdchorus/chorus.dbs                               *
*       :desc.CHORUS (geometry) database                               *
*       :servers.cernvm vxcrna xantia                                  *
*       :read.*                                                        *
*       :write.brunner meinhard                                        *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   CHPREF
      CHARACTER*2     PREFIX
      CHARACTER*80    CHLINE
      CHARACTER*12    CHUSER
      CHARACTER*255   CHFILE,CHDIR
      CHARACTER*20    CHIN(2,1)
      CHARACTER*255   CHOUT(2,1)
      INTEGER         LUNACL,IRC
#if defined(CERNLIB_IBMVM)
      CHARACTER*1     CDMODE
#endif
#include "hepdb/cduscm.inc"
#include "hepdb/slate.inc"
#include "hepdb/hdbopts.inc"
 
      IRC    = 0
 
      IOPTRR = 0
      IOPTWW = 0
*
*     Get username
*
      CHUSER = ' '
      CALL CDUSER(CHUSER,IRC)
      IF(IRC.NE.0) GOTO 999
 
      IF(LEN(CHPREF).NE.2) THEN
         IF(IDEBCD.GE.0) WRITE(LPRTCD,9001) CHPREF
 9001 FORMAT(' CDACL. illegal database prefix ',A)
         IRC = 301
         GOTO 999
      ENDIF
 
      PREFIX = CHPREF
      CALL CLTOU(PREFIX)
*
*     Find the location of the names file and process
*
      CALL GETENVF('CDSERV',CHDIR)
      LDIR = IS(1)
      IF(LDIR.EQ.0) THEN
         IF(IDEBCD.GE.0) WRITE(LPRTCD,9002)
 9002 FORMAT(' CDACL. variable CDSERV not defined')
         IRC = 311
         GOTO 999
      ENDIF
#if defined(CERNLIB_IBMVM)
*
*     Link to server disk
*
      LDOT = INDEX(CHDIR(1:LDIR),'.')
      IF(LDOT.NE.0) CHDIR(LDOT:LDOT) = ' '
*
*     Translate <> & [] characters
*
      IBRA = INDEX(CHDIR(1:LDIR),'<')
      IF(IBRA.NE.0) CHDIR(IBRA:IBRA) = ' '
      IBRA = INDEX(CHDIR(1:LDIR),'>')
      IF(IBRA.NE.0) CHDIR(IBRA:IBRA) = ' '
      IBRA = INDEX(CHDIR(1:LDIR),'[')
      IF(IBRA.NE.0) CHDIR(IBRA:IBRA) = ' '
      IBRA = INDEX(CHDIR(1:LDIR),']')
      IF(IBRA.NE.0) CHDIR(IBRA:IBRA) = ' '
*
*     Remove leading and trailing blanks
*
      JX = ICFNBL(CHDIR,1,LDIR)
      CHFILE = CHDIR(JX:LDIR)
      LDIR = LENOCC(CHFILE)
      CHDIR = CHFILE(1:LDIR)
      CALL VMCMS('EXEC GIME '//CHDIR(1:LDIR) //' (QUIET NONOTICE '
     +//'STACK)',ICODE)
      IF(ICODE.GT.4) THEN
         IF(IDEBCD.GE.0) WRITE(LPRTCD,9003) ICODE,CHDIR(1:LDIR)
 9003 FORMAT(' CDACL. return code ',I10,' from EXEC GIME ',A)
         GOTO 999
      ENDIF
      CALL VMRTRM(CHLINE,LLINE)
      CDMODE = CHLINE(1:1)
      CHFILE = 'HEPDB NAMES '//CDMODE
      LFILE = 14
#endif
#if defined(CERNLIB_WINNT)
      CHFILE = CHDIR(1:LDIR) // '\\hepdb.names'
      LFILE = LDIR + 12
      CALL CUTOL(CHFILE(1:LFILE))
#endif
#if defined(CERNLIB_MSDOS)
      CHFILE = CHDIR(1:LDIR) // '\\hepdb.nam'
      LFILE = LDIR + 10
      CALL CUTOL(CHFILE(1:LFILE))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))&&(!defined(CERNLIB_MSDOS))
      CHFILE = CHDIR(1:LDIR) // '/hepdb.names'
      LFILE = LDIR + 12
      CALL CUTOL(CHFILE(1:LFILE))
#endif
#if defined(CERNLIB_VAXVMS)
      CHFILE = CHDIR(1:LDIR) // 'hepdb.names'
      LFILE = LDIR + 11
#endif
*
*     Check the :read tag
*
      NIN  = 1
      NOUT = 1
      CHIN(1,1) = ':nick'
      CHIN(2,1) = PREFIX
 
      CHOUT(1,1) = ':read'
      CHOUT(2,1) = ' '
 
      CALL NAMEFD(LUNACL,CHFILE(1:LFILE),CHIN,NIN,CHOUT,NOUT,IRC)
      IF(IRC.EQ.0) THEN
         CALL CDACLC(CHUSER,CHOUT(2,1),IOPTRR)
      ELSEIF(IRC.EQ.4) THEN
         IOPTRR = 1
         IRC    = 0
      ELSE
         IF(IDEBCD.GE.0)
     +      WRITE(LPRTCD,9004) IRC,CHFILE(1:LFILE),CHPREF
 9004 FORMAT(' CDCONF. error ',I10,' processing names file ',A,
     +       ' for entry ',A)
         GOTO 999
      ENDIF
*
*     Check the :write tag
*
      CHOUT(1,1) = ':write'
      CHOUT(2,1) = ' '
 
      CALL NAMEFD(LUNACL,CHFILE(1:LFILE),CHIN,NIN,CHOUT,NOUT,IRC)
      IF(IRC.EQ.0) THEN
         CALL CDACLC(CHUSER,CHOUT(2,1),IOPTWW)
      ELSEIF(IRC.EQ.4) THEN
         IOPTWW = 1
         IRC    = 0
      ELSE
         IF(IDEBCD.GE.0)
     +      WRITE(LPRTCD,9004) IRC,CHFILE(1:LFILE),CHPREF
         GOTO 999
      ENDIF
 
      IF(IOPTWW.NE.0) IOPTRR = 1
 
  999 END
