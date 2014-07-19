*
* $Id: fmlogn.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmlogn.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLOGN(IRC)
#if defined(CERNLIB_IBMMVS)
      CHARACTER*8   CHUSER
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*256 MATCH
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*80  CHFILE
      CHARACTER*1   CHMODE
#endif
      LOGICAL IMAC,IEXIST
 
      IEXIST = .FALSE.
 
      IRC    = 0
 
#if defined(CERNLIB_IBMMVS)
*
      IC = FMUSER(CHUSER)
      INQUIRE(FILE='/'//CHUSER(1:LENOCC(CHUSER))//
     +        '.FATMEN.KUMAC',EXIST=IMAC)
#endif
#if defined(CERNLIB_IBMVM)
*
*     Look for a file on the A-disk or G-disk, including extensions
*
      INQUIRE(FILE='/FATMEN KUMAC *',NAME=CHFILE,EXIST=IEXIST)
      IF(IEXIST) THEN
         LF = LENOCC(CHFILE)
         CHMODE = CHFILE(LF-1:LF-1)
         IF((LLT(CHMODE,'G').AND.CHMODE.NE.'A').OR.
     +      LGT(CHMODE,'G')) THEN
            IF(IDEBFA.GE.0) PRINT *,'FMKUIP. warning - ',
     +         'found file ',CHFILE(2:LENOCC(CHFILE))
            IF(IDEBFA.GE.0) PRINT *,'This file must be on ',
     +         'disks A or G to be executed automatically'
            IF(IDEBFA.GE.0) PRINT *,'(An extension, e.g. B/A ',
     +         'will also work)'
            IMAC = .FALSE.
         ELSE
            IMAC = .TRUE.
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      ICONT = 0
      IMAC = LIB$FIND_FILE('FATMEN.KUMAC',MATCH,ICONT,,,,)
#endif
#if defined(CERNLIB_UNIX)
      INQUIRE (FILE='fatmen.kumac', EXIST=IMAC)
#endif
      IF(IMAC) THEN
         CALL KUEXEC('EXEC FATMEN')
         IRC = 1
*     ELSE
*        IF(.NOT.IEXIST) PRINT *,'FATMEN.KUMAC not found'
      ENDIF
 
      END
