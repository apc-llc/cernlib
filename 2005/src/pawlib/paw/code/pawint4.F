*
* $Id: pawint4.F,v 1.4 1999/05/20 07:37:18 couet Exp $
*
* $Log: pawint4.F,v $
* Revision 1.4  1999/05/20 07:37:18  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.3  1998/02/11 15:48:06  couet
* - KUIP/UNITS has been replaced by FORTRAN/UNITS and is now able to list the
*   HBOOK files again.
*
* Revision 1.2  1997/10/23 13:00:27  mclareni
* NT mods
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/19 21/11/95  15.39.13  by  Julian Bunn
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWINT4(PAWLOGF)
*
*          Initialization routine
*
#include "paw/pcmode.inc"
#include "paw/pawfil.inc"
*
      CHARACTER*(*) PAWLOGF
      LOGICAL       EXIST1, EXIST2
*.______________________________________
*
      LPAW = LENOCC(PAWLOGF)
*
*        Execute SYSTEM logon macro containing messages
*        about new PAW features etc.
*        Should find a standard place for this file!
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      PAWSYS=PAWSY1
#endif
      EXIST1=.FALSE.
#if !defined(CERNLIB_NEWLIB)
      INQUIRE(FILE=PAWSYS,EXIST=EXIST1)
#endif
#if defined(CERNLIB_NEWLIB)
      EXIST1=.TRUE.
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      PAWSYS=PAWSY2
#endif
      IF (EXIST1) CALL KUEXEC('EXEC '//PAWSYS)
#if defined(CERNLIB_VAX)
      IF(.NOT.EXIST1) THEN
         IF (LIB$SYS_TRNLOG('PAW$SYS',LPAW,PAWSYS,,,%VAL(0)) .EQ. 1)
     +      CALL KUEXEC('EXEC '//PAWSYS(1:LPAW))
      ENDIF
#endif
#if defined(CERNLIB_UNIX)
      IF(.NOT.EXIST1) THEN
         PAWSYS = ' '
#ifdef CERNLIB_WINNT
         CALL GETENVF('CERN_ROOT',PAWSYS)
         LPAW = LENOCC(PAWSYS)
         IF (LPAW .GT. 0) THEN
           IF (PAWSYS(LPAW:LPAW) .NE. '/' .AND.
     *         PAWSYS(LPAW:LPAW) .NE. '\')       THEN
              PAWSYS = PAWSYS(:LPAW) // '/'
              LPAW = LPAW + 1
           ENDIF
           PAWSYS = PAWSYS(:LPAW) // 'bin/pawsys.kumac'
           INQUIRE(FILE=PAWSYS,EXIST=EXIST1)
         ENDIF
         IF (.NOT. EXIST1) CALL GETENVF('PAWSYS',PAWSYS)
#else
         CALL GETENVF('PAWSYS',PAWSYS)
#endif
         LPAW = LENOCC(PAWSYS)
         IF (LPAW.GT.0) CALL KUEXEC('EXEC '//PAWSYS(1:LPAW))
      ENDIF
#endif
*
*           Disable the command KUIP/UNITS. FORTRAN/UNITS is used instead
*
      CALL KUEXEC('VISIBILITY KUIP/UNITS OFF')
*
      IF (.NOT.NOLOG) THEN
*
*           Execute LOGON macro
*
#if !defined(CERNLIB_IBM)||defined(CERNLIB_IBMMVS)
         PAWLOG=PAWLOGF
#endif
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
         PAWLOG=PAWLO1
#endif
#if !defined(CERNLIB_IBMMVS)
         LPAW = LENOCC(PAWLOG)
         INQUIRE(FILE=PAWLOG(1:LPAW),EXIST=EXIST1)
#endif
#if defined(CERNLIB_IBMMVS)
         EXIST1 = .TRUE.
#endif
         IF (.NOT. EXIST1) THEN
#if defined(CERNLIB_IBM)
            EXIST2 = .FALSE.
#endif
#if defined(CERNLIB_VAX)
            EXIST2 = .FALSE.
            IF (LIB$SYS_TRNLOG('PAW$LOGON',LPAW,PAWLOG,,,%VAL(0))
     +          .EQ. 1) EXIST2 = .TRUE.
#endif
#if defined(CERNLIB_APOLLO)
            PAWLOG = '~/user_data/pawlogon.kumac'
            LPAW   = 26
            INQUIRE(FILE=PAWLOG(1:LPAW), EXIST=EXIST2)
#endif
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY))&&(!defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))
            PAWLOG = '~/.pawlogon.kumac'
            LPAW=LENOCC(PAWLOG)
            CALL KUHOME(PAWLOG,LPAW)
            INQUIRE(FILE=PAWLOG(1:LPAW), EXIST=EXIST2)
#endif
#if defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT)
            PAWLOG = '~\pawlogon.kumac'
            LPAW=LENOCC(PAWLOG)
            CALL KUHOME(PAWLOG,LPAW)
            INQUIRE(FILE=PAWLOG(1:LPAW), EXIST=EXIST2)
#endif
            IF (.NOT. EXIST2) THEN
               NOLOG = .TRUE.
#if !defined(CERNLIB_VAX)
               PRINT 10100, PAWLOG(1:LPAW)
#endif
#if defined(CERNLIB_VAX)
               PRINT 10200, 'PAW$LOGON'
#endif
            ELSE
               PRINT 10300, PAWLOG(1:LPAW)
            ENDIF
         ENDIF
      ENDIF
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      PAWLOG=PAWLO2
#endif
      IF (.NOT.NOLOG) CALL KUEXEC('EXEC '//PAWLOG(1:LPAW))
#if !defined(CERNLIB_NEWLIB)
*
*            Reset history
*
#endif
      CALL KUEXEC('/KUIP/LAST 0')
#if defined(CERNLIB_NEWLIB)
      CALL KUEXEC('ALIAS/CREATE NEWLIB SHELL C')
#endif
*
10100 FORMAT(' *** No default PAWLOGON file "',A,'" found',/)
10200 FORMAT(' *** No default PAWLOGON file defined via "',A,'"',/)
10300 FORMAT(' *** Using default PAWLOGON file "',A,'"',/)
*
      END
