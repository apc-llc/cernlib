*
* $Id: cdfsnd.F,v 1.4 1997/03/14 17:07:32 mclareni Exp $
*
* $Log: cdfsnd.F,v $
* Revision 1.4  1997/03/14 17:07:32  mclareni
* WNT mods
*
* Revision 1.3.2.1  1997/01/21 11:28:42  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.3  1996/04/12 10:36:11  jamie
* booboo
*
* Revision 1.2  1996/04/12 07:39:58  jamie
* check write acl in cdfsnd
*
* Revision 1.1.1.1  1996/02/28 16:24:17  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFSND (LUN, CTOP, IRC)
*     ===================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFSND (LUN, CTOP, IRC*)                                *
*                                                                      *
*   Close current update file on the specified unit and mv/rename      *
*   to the input queue directory of the server.                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit number where the file sits                 *
*     CTOP     First 2 characters of the file after being moved        *
*     IRC     Error code (0 if no error)                               *
*                                                                      *
*   Requires :                                                         *
*                                                                      *
*     CDSERV variable must be set                                      *
*                                                                      *
*   Called by CDSTSV                                                   *
*                                                                      *
************************************************************************
*
      LOGICAL         OPEN
      CHARACTER*2     CTOP,CH2
      CHARACTER*255   NAMEO,NAMEN,ARGS
#if defined(CERNLIB_VAXVMS)
      CHARACTER*255   CHTMP
#endif
#include "hepdb/slate.inc"
#include "hepdb/cduscm.inc"
#include "hepdb/cdnamc.inc"
      CHARACTER*255   CHQUED,CHFILE,CHDIR
      CHARACTER*20    CHIN(2,1)
      CHARACTER*255   CHOUT(2,1)
#if defined(CERNLIB_IBMVM)
      CHARACTER*8     FNAME,FTYPE
#endif
      INTEGER         SYSTEMF
 
      DATA            NENTRY/0/
      SAVE            NENTRY,CHDIR,LDIR
 
*     ------------------------------------------------------------------
*
* ** Get the name of the file and close it
*
      IRC = 0
      INQUIRE (LUN, OPENED=OPEN, NAME=NAMEO)
      IF (.NOT.OPEN) GO TO 999
      CLOSE (LUN)
 
      IF(NENTRY.EQ.0) THEN
         NENTRY = 1
*
*     Find the location of the names file and process
*
         CALL GETENVF('CDSERV',CHDIR)
         LDIR = IS(1)
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
#if defined(CERNLIB_IBMVM)
*     LDOT = INDEX(CHDIR(1:LDIR),'.')
*     IF(LDOT.NE.0) LDIR = LDOT - 1
         CHFILE = 'HEPDB NAMES '//CDMODE
         LFILE = 13
#endif
      ENDIF
 
      NCH  = LENOCC (NAMEO)
      CH2  = CTOP
      CALL CUTOL (CH2)
      NCH2 = LENOCC (CH2)
 
#if defined(CERNLIB_VAXVMS)
      I1     = INDEX(NAMEO(1:NCH),']')  + 1
*
*     Strip off version number
*
      ICOLON = INDEX(NAMEO(1:NCH),';')
      IF(ICOLON.NE.0) NCH = ICOLON - 1
#endif
#if (!defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))
      DO 10 I = NCH, 1, -1
         IF (NAMEO(I:I).EQ.'/') THEN
            I1 = I + 1
            GO TO 20
         ENDIF
   10 CONTINUE
      I1   = 1
   20 CONTINUE
#endif
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT)
      DO 10 I = NCH, 1, -1
#ifdef CERNLIB_BSLASH
         IF (NAMEO(I:I).EQ.'\\' .OR. 
#else
         IF (NAMEO(I:I).EQ. '\' .OR. 
#endif
     *       NAMEO(I:I).EQ. ':' .OR. 
     *       NAMEO(I:I).EQ. '/') THEN
            I1 = I + 1
            GO TO 20
         ENDIF
   10 CONTINUE
      I1   = 1
   20 CONTINUE
#endif
*     IF (NCH2.GT.0.AND.NCH.GT.I1+1) THEN
*        NAMEN = CH2(1:NCH2)//NAMEO(I1+2:NCH)
*     ELSE IF (NCH2.GT.0) THEN
*        NAMEN = CH2(1:NCH2)//NAMEO(I1:NCH)
*     ELSE
*        NAMEN = NAMEO(I1:NCH)
*     ENDIF
*
*     Ensure mv-ed filename is unique
*
      CALL CDUNIQ(NAMEN,IRC)
      NCHN = LENOCC (NAMEN)
      NAMEN(1:2) = CH2
*
*     Get the file name and list of servers for this database
*
      NIN  = 1
      NOUT = 1
      CHIN(1,1) = ':nick'
      CHIN(2,1) = 'config'
 
      CHOUT(1,1) = ':queue'
      CHOUT(2,1) = ' '
 
      CALL NAMEFD(LUN,CHFILE(1:LFILE),CHIN,NIN,CHOUT,NOUT,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBCD.GE.0)
     +      PRINT *,'CDCONF. error ',IRC,' processing names file ',
     +      CHFILE(1:LFILE),' for entry ',CH2
         RETURN
      ENDIF
 
      CHQUED = CHOUT(2,1)
      LQUED  = LENOCC(CHQUED)
*
* *** check to see if the user has write privileges to the database file
*
       CALL CDACL(LUN,CH2,IOPTRR,IOPTWW,' ',IRC)
       IF (IOPTWW .EQ. 0) THEN
          IF(IDEBCD.GE.0)
     +       PRINT *,'CDFSND. Write access denied to database file ',
     +       'with prefix ',CH2,'.'
          IRC = -13
          RETURN
       ENDIF
 
#if defined(CERNLIB_VAXVMS)
*
*        map UNIX path to VMS path
*
*     /a/b/c/d/e/f/g maps to a:[b.c.d.e.f.g]
*
      ISLASH = INDEX(CHQUED(1:LQUED),'/')
 
      IF(ISLASH.NE.0) THEN
 
         JSLASH = INDEX(CHQUED(ISLASH+1:LQUED),'/')
         IF(JSLASH.EQ.0) THEN
            IF(IDEBCD.GE.-3) WRITE(LPRTCD,7001) CHQUED(1:LQUED)
 7001 FORMAT(' CDFSND. error translating ',A,' to VMS format')
            IRC = -12
            GOTO 999
         ENDIF
 
         CHTMP  = CHQUED(ISLASH+1:ISLASH+JSLASH-1) // ':[' //
     +            CHQUED(ISLASH+JSLASH:LQUED)      // ']'
 
         LOLD   = LQUED
         LQUED  = LENOCC(CHTMP)
 
         CALL CTRANS(CHTMP,'/','.',1,LQUED)
 
         WRITE(6,9001) CHQUED(1:LOLD),CHTMP(1:LQUED)
 9001 FORMAT(' CDFSND. queue directory ',A,' mapped to ',A)
         CHQUED = CHTMP
 
      ENDIF
 
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_WINNT))&&(!defined(CERNLIB_MSDOS))
*
* *** mv the file
*
      ARGS = 'mv '//NAMEO(I1:NCH)//' '//CHQUED(1:LQUED)//'/'
     +            //NAMEN(1:NCHN)
#endif
#if defined(CERNLIB_WINNT)||defined(CERNLIB_MSDOS)
*
* *** move the file  (since MsDOS 6.0 or Windows/NT)
*
      ARGS = 'move '//NAMEO(I1:NCH)//' '//CHQUED(1:LQUED)//'\\'
     +            //NAMEN(1:NCHN)
#endif
#if defined(CERNLIB_UNIX)
      IRC = SYSTEMF(ARGS)
#endif
#if defined(CERNLIB_VAXVMS)
*
*     move the file
*
      CALL CDMOVF(NAMEO(I1:NCH),
     +            CHQUED(1:LQUED)//'ZZ'//NAMEN(3:NCHN),' ',IRC)
      IF(IRC.NE.0) GOTO 999
*
* *** rename the file
*
      IERR = LIB$RENAME_FILE(CHQUED(1:LQUED)//'ZZ'//NAMEN(3:NCHN),
     +                       CHQUED(1:LQUED)//NAMEN(1:NCHN),,,,,,,,,,)
      IF(.NOT.IERR) THEN
         IRC = -1
      ELSE
         IRC = 0
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
*
*     New file name is prefix//random_number $CDSERV
*
      LEND = LDIR
      LBLA = INDEXB(CHDIR(1:LDIR),' ')
      IF(LBLA.NE.0) LEND = LBLA - 1
      FTYPE = CHDIR(1:LEND)
      CALL CDRAND(FNAME,IRC)
      NAMEN = CH2(1:NCH2) // FNAME(3:) // ' ' // FTYPE // ' ' //
     +   NAMEO(NCH-1:NCH)
      NCHN  = LENOCC(NAMEN)
      CALL CLTOU(NAMEN(1:NCHN))
      CALL VMCMS('RENAME '//NAMEO(I1:NCH)//' '//NAMEN(1:NCHN),IRC)
*
*     Now send the file to the server pointed to be the :queue tag
*
      CALL VMCMS('EXEC SENDFILE '//NAMEN(1:NCHN)
     +           //' TO '//CHQUED(1:LQUED),IRC)
      CALL VMCMS('ERASE '//NAMEN(1:NCHN),IRC)
#endif
*                                                             END CDFSND
  999 END
