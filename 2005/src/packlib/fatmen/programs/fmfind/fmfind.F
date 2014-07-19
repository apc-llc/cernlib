*
* $Id: fmfind.F,v 1.1.1.1 1996/03/07 15:17:42 mclareni Exp $
*
* $Log: fmfind.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:42  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATFIND
*
*     Taken command lines arguments and issue FMFIND
*
      CHARACTER*255 GENAM,CHUNIT
      CHARACTER*36  CHOPT
      CHARACTER*26  KOPT
      CHARACTER*8   CHCODE
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatoptd.inc"
 
      NARGS = IARGC(DUMMY)
 
      IF(NARGS.LT.2) THEN
         PRINT 9001
9001  FORMAT(' FMFIND. usage: FMFIND GENAM CHUNIT [CHOPT] ')
         GOTO 99
      ENDIF
 
      CALL GETARG(1,GENAM)
      LGN = LENOCC(GENAM)
      CALL CLTOU(GENAM(1:LGN))
 
      IF(NARGS.GE.2) THEN
         CALL GETARG(2,CHUNIT)
         LUNIT = LENOCC(CHUNIT)
      ELSE
         CHUNIT = ' '
         LUNIT  = 0
      ENDIF
 
      IF(NARGS.GE.3) THEN
         CALL GETARG(3,CHOPT)
         CALL FMOPTC(CHOPT,ALFNUM,IOPT)
      ELSE
         CHOPT = ' '
         CALL VZERO(IOPT,36)
      ENDIF
 
*
*     Get FATMEN root and initialise
*
      LUNRZ = 1
      LUNFZ = 2
 
      LEND  = INDEX(GENAM(3:LGN),'/') + 3
      LEND  = LEND + INDEX(GENAM(LEND:LGN),'/') - 2
 
      CALL FMLOGL(-3)
      CALL FMSTRT(LUNRZ,LUNFZ,GENAM(1:LEND),IRC)
      CALL FMLOGL(0)
*
*     Build options string
*
      IOPTU = 1
      IF(IOPTW.EQ.0) IOPTR = 1
 
      KOPT  = 'U'
      LKOPT = 1
 
      IF(IOPTD.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'D'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTE.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'E'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTH.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'H'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTK.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'K'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTL.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'L'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTN.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'N'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTQ.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'Q'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTR.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'R'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTS.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'S'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTU.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'U'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTV.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'V'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTW.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'W'
         LKOPT             = LKOPT + 1
      ENDIF
 
      IF(IOPTY.NE.0) THEN
         KOPT(LKOPT:LKOPT) = 'Y'
         LKOPT             = LKOPT + 1
      ENDIF
 
      LFAT  = 0
      CALL FMOPEN(GENAM(1:LGN),CHUNIT(1:LUNIT),LFAT,KOPT,IRC)
 
      CALL FMEND(IC)
 
      CALL FMITOC(IRC,CHCODE,LCODE)
#if defined(CERNLIB_UNIX)
      CALL FAEXIT(IRC)
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('S','FATCODE',CHCODE(1:LCODE),ISTAT)
      IF(IRC.EQ.0) THEN
         STOP
      ELSE
         STOP 16
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      ISTAT = LIB$SET_SYMBOL('FATCODE',CHCODE(1:LCODE),
     +                       LIB$K_CLI_GLOBAL_SYM)
      IF(IRC.EQ.0) THEN
         STOP
         CALL SYS$EXIT(%VAL(1))
      ELSE
         CALL SYS$EXIT(%VAL(44))
      ENDIF
#endif
99    END
 
