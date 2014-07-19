*
* $Id: fmcd.F,v 1.3 1997/07/16 13:17:58 mclareni Exp $
*
* $Log: fmcd.F,v $
* Revision 1.3  1997/07/16 13:17:58  mclareni
* Add Linux to the normal Unix backslash case
*
* Revision 1.2  1996/03/11 14:23:49  cernlib
* Add builing/installing programs for fatmen.
*
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMCD
#include "fatmen/fmpath.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatbug.inc"
      CHARACTER*255 CHPATH
      CHARACTER*80  CHOPT
      CHARACTER*80  LINE
      COMMON/QUEST/IQUEST(100)
#include "fatmen/fatoptd.inc"
#include "fatmen/fatinit.inc"
      CALL KUGETC(PATH,LPATH)
      CALL KUGETC(CHOPT,LCH)
      CALL FMFIXF(PATH,PATH)
      LPATH=LENOCC(PATH)
 
      IF(LCH.EQ.0) THEN
         CHOPT = ' '
         LCH   = 1
      ENDIF
 
      CALL FMOPTC(CHOPT,ALFNUM,IOPT)
      IF(IOPTA.NE.0) THEN
         DO 36 J=1,36
            IOPT(J) = 1
36       CONTINUE
      ENDIF
 
      IF(PATH(1:LPATH).EQ.'PWD') THEN
        CALL FMPWD
        RETURN
      ENDIF
 
      IF(IDEBFA.GE.3)
     +   PRINT *,'FMCD. enter for path = ',PATH(1:LPATH)
 
      ISLASH = INDEX(PATH(1:LPATH),'/')
      IF(ISLASH.EQ.0) THEN
         LEND = LPATH
      ELSE
         LEND = ISLASH - 1
      ENDIF
      CHPATH = PATH
 
      IF(PATH(1:LEND).EQ.'..') THEN
#if defined(CERNLIB_SUN)||defined(CERNLIB_SGI)||defined(CERNLIB_DECS)\
  ||defined(CERNLIB_IBMRT)||defined(CERNLIB_APOF77)||defined(CERNLIB_QMALPH)\
  ||defined(CERNLIB_HPUX)||defined(CERNLIB_LINUX)
        PATH = '\\'
#else
        PATH = '\'
#endif
        IF(ISLASH.GT.0) THEN
           PATH  = PATH(1:1) // CHPATH(ISLASH:LPATH)
           LPATH = LENOCC(PATH)
        ELSE
           LPATH = 1
        ENDIF
      ENDIF
 
      IF((PATH(1:LEND).EQ.'~').OR.(PATH(1:LEND).EQ.'$HOME')) THEN
        PATH  = FATTOP
        LTOP  = LENOCC(PATH)
        IF(ISLASH.GT.0) THEN
           PATH  = PATH(1:LTOP) // CHPATH(ISLASH:LPATH)
        ENDIF
      LPATH = LENOCC(PATH)
      IF(IDEBFA.GE.3) PRINT *,'FMCD. set path to ',PATH(1:LPATH)
      ENDIF
 
#if !defined(CERNLIB_CZ)
      CALL RZCDIR(PATH(1:LPATH),'U')
      IF (IQUEST(1).EQ.0) THEN
         CALL RZCDIR(PATH(1:LPATH),'P')
         CALL RZCDIR(PATH,'R')
*
*     Quota
*
         IF(IOPTQ.NE.0) THEN
            PRINT *,'Quota = ',IQUEST(10)
         ENDIF
*
*
*     Subdirectories
*
         IF(IOPTS.NE.0) THEN
            PRINT *,'Number of subdirectories = ',IQUEST(9)
         ENDIF
*
*     Dates & Times
*
         IF(IOPTT.NE.0) THEN
            PRINT *,'Created on ',IQUEST(14),' at ',IQUEST(15),
     +              'Modified on ',IQUEST(16),' at ',IQUEST(17)
         ENDIF
*
*     Usage
*
         IF(IOPTU.NE.0) THEN
            PRINT *,'Number of records used = ',IQUEST(18)
            PRINT *,IQUEST(19),' megawords + ',IQUEST(20),' words'
         ENDIF
*
#endif
#if defined(CERNLIB_CZ)
      CALL CZPUTA('MESS :CD '//PATH(3:LPATH),ISTAT)
1     CONTINUE
      CALL CZGETA(LINE,ISTAT)
      PRINT *,LINE(3:80)
      IF(INDEX('Cannot',LINE) .EQ. 0) THEN
#endif
         CDIR  = PATH
         LCDIR = LENOCC(CDIR)
      ELSE
         GOTO 99
      ENDIF
      RETURN
 
99    CONTINUE
      PRINT *,'FMCD. Error setting directory, IRC = ',IQUEST(1)
      END
