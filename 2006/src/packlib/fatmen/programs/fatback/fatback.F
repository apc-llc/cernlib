*
* $Id: fatback.F,v 1.3 2000/01/04 16:41:32 mclareni Exp $
*
* $Log: fatback.F,v $
* Revision 1.3  2000/01/04 16:41:32  mclareni
* Remove dummy argument for IARGC
*
* Revision 1.2  2000/01/04 15:55:38  jamie
* change I6 to I6.6 (blank in filename)
*
* Revision 1.1.1.1  1996/03/07 15:17:37  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      PROGRAM FATBACK
*
*     Convert FATMEN RZ file to exchange format
*
      CHARACTER*255 CHFILE,CHBACK,CHLINK
      PARAMETER (IPRINT=6)
      PARAMETER (IDEBUG=0)
      PARAMETER (LUNI=1)
      PARAMETER (LUNO=2)
      PARAMETER (NWPAW=1000000)
      COMMON/PAWC/PAW(NWPAW)
      INTEGER   SYSTEMF
 
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_TCPSOCK))
      IDUMMY = CINIT(DUMMY)
#endif
      CALL HLIMIT(NWPAW)
 
      NARGS = IARGC()
 
      IF(NARGS.LT.2) GOTO 99
 
      CALL GETARG(1,CHFILE)
      LFILE = LENOCC(CHFILE)
 
      CALL GETARG(2,CHBACK)
      LBACK = LENOCC(CHBACK)
 
#if defined(CERNLIB_UNIX)
      LBACK = LBACK + 1
      CHBACK(LBACK:LBACK) = '/'
#endif
 
      CALL DATIME(ID,IT)
 
      WRITE(CHBACK(LBACK+1:),9001) ID
 9001 FORMAT(I6.6,'.fatfx')
      LBACK = LBACK + 12
 
      CALL XZINIT(IPRINT,IDEBUG,LUNI,LUNO)
 
      IRECL = 3600
      PRINT 9002,CHFILE(1:LFILE),CHBACK(1:LBACK)
 9002 FORMAT(' FATBACK. copying ',A,' to ',A)
      CALL XZRTOF(CHFILE(1:LFILE),CHBACK(1:LBACK),IRECL,' ',IRC)
      IF(IRC.NE.0) THEN
#if defined(CERNLIB_IBMVM)
         STOP 4
#endif
#if defined(CERNLIB_VAXVMS)
         CALL SYS$EXIT(%VAL(4))
#endif
#if defined(CERNLIB_UNIX)
         CALL FAEXIT(4)
#endif
      ELSE
#if defined(CERNLIB_UNIX)
*
*     Create link to latest 'good' backup
*
         CHLINK = CHBACK(1:LBACK-12)//'good.fatfx'
         LLINK  = LBACK - 2
         CALL UNLINKF(CHLINK(1:LLINK))
         PRINT 9003,CHBACK(1:LBACK),CHLINK(1:LLINK)
 9003 FORMAT(' FATBACK. linking ',A,' to ',A)
         ISTAT = SYSTEMF('ln -sf '//CHBACK(1:LBACK)//
     +                   ' '//CHLINK(1:LLINK))
#endif
      ENDIF
 
   99 END
