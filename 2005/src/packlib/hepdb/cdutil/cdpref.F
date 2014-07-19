*
* $Id: cdpref.F,v 1.1.1.1 1996/02/28 16:24:40 mclareni Exp $
*
* $Log: cdpref.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPREF(LUN,CHPREF,CHTOP,CHDBFL,IRC)
*     =============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPREF (LUN, CHPREF, CHTOP*, CHDBFL*, IRC*)             *
*                                                                      *
*   Find top directory and file name from database prefix              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Unit on which names file is to be read                  *
*     CHPREF   Two character database prefix                           *
*     CHTOP    4 character top directory name ('CD' // CHPREF)         *
*     CHDBFL   File name of database file (for CDOPEN)                 *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by <USER>                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =  4 : Prefix not found in NAMES file                  *
*               =  ? : Other errors as for NAMEFD                      *
*               =-12 : Error translating filename to VAX format        *
*                                                                      *
************************************************************************
*
      CHARACTER*(*)   CHPREF,CHTOP,CHDBFL
      CHARACTER*2     PREFIX
#include "hepdb/slate.inc"
#include "hepdb/cduscm.inc"
#include "hepdb/cdnamc.inc"
      CHARACTER*80    CHLINE
      CHARACTER*255   CHQUED,CHFILE,CHDIR
      CHARACTER*20    CHIN(2,1)
      CHARACTER*255   CHOUT(2,1)
 
      DATA            NENTRY/0/
      SAVE            NENTRY,CHDIR,LDIR,CHFILE,LFILE
 
      IRC    = 0
      CHTOP  = ' '
 
      IF(LEN(CHPREF).NE.2) THEN
         IF(IDEBCD.GE.0) PRINT *,'CDPREF. illegal database prefix ',
     +      CHPREF
         IRC = 301
         RETURN
      ENDIF
 
      PREFIX = CHPREF
      CALL CLTOU(PREFIX)
*
*     Reserved prefix
*
      IF(PREFIX.EQ.'ZZ') THEN
         IF(IDEBCD.GT.-3) WRITE(LPRTCD,9001) CHPREF
 9001 FORMAT(' CDPREF. ',A,' is a reserved database prefix.')
         IRC = 302
         RETURN
      ENDIF
 
      IF(NENTRY.EQ.0) THEN
         NENTRY = 1
         CHFILE = ' '
*
*     Find the location of the names file and process
*
         CALL GETENVF('CDSERV',CHDIR)
         LDIR = IS(1)
         IF(LDIR.EQ.0) THEN
            IF(IDEBCD.GE.0) PRINT *,'CDPREF. variable CDSERV ',
     +         'not defined'
            IRC = 311
            RETURN
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
         JX     = ICFNBL(CHDIR,1,LDIR)
         CHFILE = CHDIR(JX:LDIR)
         LDIR   = LENOCC(CHFILE)
         CHDIR  = CHFILE(1:LDIR)
         CALL VMCMS('EXEC GIME '//CHDIR(1:LDIR) //' (QUIET NONOTICE '
     +   //'STACK)',ICODE)
         IF(ICODE.GT.4) THEN
            IF(IDEBCD.GE.0) PRINT *,'CDPREF. return code ',ICODE,
     +         ' from EXEC GIME '//CHDIR(1:LDIR)
            RETURN
         ENDIF
         CALL VMRTRM(CHLINE,LLINE)
         CDMODE = CHLINE(1:1)
         CHFILE = 'HEPDB NAMES '//CDMODE
         LFILE  =  14
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
      ENDIF
*
*     Get the file name and list of servers for this database
*
      NIN  = 1
      NOUT = 1
      CHIN(1,1) = ':nick'
      CHIN(2,1) = PREFIX
 
      CHOUT(1,1) = ':file'
      CHOUT(2,1) = ' '
 
      CALL NAMEFD(LUN,CHFILE(1:LFILE),CHIN,NIN,CHOUT,NOUT,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBCD.GE.0)
     +      PRINT *,'CDCONF. error ',IRC,' processing names file ',
     +      CHFILE(1:LFILE),' for entry ',CHPREF
         RETURN
      ENDIF
 
      CHTOP      = 'CD'
      CHTOP(3:4) = CHPREF(1:2)
#if !defined(CERNLIB_IBMVM)
      CHDBFL     = CHOUT(2,1)
#endif
#if defined(CERNLIB_VAXVMS)
      LDBFL      = LENOCC(CHDBFL)
      IF(INDEX(CHDBFL(1:LDBFL),'/').NE.0) THEN
         CALL FTOVAX(CHDBFL(1:LDBFL),LDBFL)
         IF(IS(1).NE.1) THEN
            IF(IDEBCD.GE.-3) WRITE(LPRTCD,7001)
     +         CHOUT(2,1)(1:LENOCC(CHOUT(2,1)))
7001  FORMAT(' CDPREF. error converting ',A,' to VMS format')
            IRC = -12
            GOTO 999
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
*
*     File syntax is <user.addr>filename.filetype
*
      LOUT = LENOCC(CHOUT(2,1))
      LGT  = INDEX(CHOUT(2,1),'>')
      IF(LGT.EQ.0) THEN
         CHDBFL = CHOUT(2,1)
      ELSE
         CHLINE = ' '
         CHLINE = CHOUT(2,1)(2:LGT-1)
         LDOT   = INDEX(CHLINE,'.')
         IF(LDOT.NE.0) CHLINE(LDOT:LDOT) = ' '
         LLINE  = LENOCC(CHLINE)
         CALL VMCMS('EXEC GIME '//CHLINE(1:LLINE) //' (QUIET NONOTICE '
     +   //'STACK)',ICODE)
         IF(ICODE.GT.4) THEN
            IF(IDEBCD.GE.0) PRINT *,'CDPREF. return code ',ICODE,
     +         ' from EXEC GIME '//CHLINE(1:LLINE)
            RETURN
         ENDIF
         CALL VMRTRM(CHLINE,LLINE)
         CHDBFL = CHOUT(2,1)(LGT+1:LOUT) // '.' // CHLINE(1:1)
      ENDIF
#endif
999   END
