*
* $Id: cscrsl.F,v 1.5 1998/01/21 10:25:12 couet Exp $
*
* $Log: cscrsl.F,v $
* Revision 1.5  1998/01/21 10:25:12  couet
* - LINUX mods (it should work now ...)
*
* Revision 1.4  1998/01/15 14:41:49  mclareni
* Mods for shared libraries for Linux
*
* Revision 1.3  1998/01/13 12:45:25  couet
* - Mods for LINUX
*
* Revision 1.2  1998/01/12 09:55:45  couet
* - Mods for LINUX (shared libraries)
*
* Revision 1.1.1.1  1996/02/26 17:16:30  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/02 07/04/94  15.26.09  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   07/01/94
      SUBROUTINE CSCRSL(NAME,CPID,FEXT,IERR)
      CHARACTER *(*) NAME,CPID,FEXT
      COMMON /CSTBCS/ ITBC,ITBS
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      CHARACTER *72 CHLINE,FEXEC*72
      INTEGER  SYSTEMF,CS_SHL_LOAD
      LN=LENOCC(NAME)
      LP=LENOCC(CPID)
*c      IF(ITBS.NE.0)THEN
*c        CALL CSRMSL(NAME)
*c      ENDIF
      IF(FEXT.EQ.'.sl' .OR. FEXT.EQ.'.csl')THEN
#if defined(CERNLIB_LINUX)
        CHLINE=NAME(:LN)
        CHLINE(LENOCC(CHLINE)+1:)='.sl'
        IERR=CS_SHL_LOAD(CHLINE(1:LENOCC(CHLINE)))
#else
        IERR=CS_SHL_LOAD(NAME(1:LN)//'.sl')
#endif
        ITBS=ITBS+1
        IF(IERR.NE.0)THEN
#if defined(CERNLIB_LINUX)
          CHLINE=NAME(:LN)
          CHLINE(LENOCC(CHLINE)+1:)='.sl'
          CALL CS_SHL_UNLOAD(CHLINE(1:LENOCC(CHLINE)))
#else
          CALL CS_SHL_UNLOAD(NAME(1:LN)//'.sl')
#endif
          ITBS=ITBS-1
        ENDIF
        RETURN
      ENDIF
      LPATH=LENOCC(CHPATH)
      FEXEC=CHPATH(:LPATH)//'exec_'//CPID//'.exec'
      LEXEC=LENOCC(FEXEC)
      CALL CSCREXEC(NAME,CPID,FEXT,FEXEC,IERR)
      CHLINE='/bin/sh '//FEXEC(:LEXEC)
      L=LENOCC(CHLINE)
      IERR=SYSTEMF(CHLINE(:L))
      IF (IERR .NE. 0) RETURN
      CHLINE='/bin/rm -f '//FEXEC(:LEXEC)
      L=LENOCC(CHLINE)
      IERR=SYSTEMF(CHLINE(:L))
      IF (IERR .NE. 0) RETURN
#if defined(CERNLIB_LINUX)
      CHLINE=NAME(1:LN)
      CHLINE(LENOCC(CHLINE)+1:)='.sl'
      IERR=CS_SHL_LOAD(CHLINE(1:LENOCC(CHLINE)))
#else
      IERR=CS_SHL_LOAD(NAME(1:LN)//'.sl')
#endif
      ITBS=ITBS+1
      IF(IERR.NE.0)THEN
#if defined(CERNLIB_LINUX)
        CHLINE=NAME(1:LN)
        CHLINE(LENOCC(CHLINE)+1:)='.sl'
        CALL CS_SHL_UNLOAD(CHLINE(1:LENOCC(CHLINE)))
#else
        CALL CS_SHL_UNLOAD(NAME(1:LN)//'.sl')
#endif
        ITBS=ITBS-1
      ENDIF
      RETURN
#endif
      END
