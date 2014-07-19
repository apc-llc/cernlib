*
* $Id: fmqfat.F,v 1.1.1.1 1996/03/07 15:18:18 mclareni Exp $
*
* $Log: fmqfat.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:18  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMQFAT(CHSYS,CHGRP,IRC)
*
*     Return FATMEN system and group
*
      CHARACTER*(*) CHSYS,CHGRP
      CHARACTER*16  CHTEMP
#include "fatmen/slate.inc"
#if defined(CERNLIB_IBMMVS)
      CHARACTER*100 CHPARM
#endif
#include "fatmen/fatbug.inc"
#if defined(CERNLIB_IBMMVS)
      CALL GOPARM(LPARM,CHPARM)
      IF(LPARM.EQ.0) THEN
         NPARMS = 0
      ELSE
         CALL FMNWRD(',',CHPARM(1:LPARM),NPARMS)
      ENDIF
      IF(NPARMS.GT.0) THEN
         CALL FMWORD(CHSYS,0,',',CHPARM(1:LPARM),IRC)
         LCHSYS = LENOCC(CHSYS)
      ELSE
         PRINT *,'FMQFAT. GOPARM variable FATSYS not defined. '
         PRINT *,'FMQFAT. defaulted to CERN'
         CHSYS  = 'CERN'
         LCHSYS = 4
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMREXX('F','FATSYS',CHSYS,IRC)
      IF(IRC.EQ.0) LCHSYS = LENOCC(CHSYS)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATSYS',CHSYS)
      LCHSYS = IS(1)
      IRC = 0
      IF(LCHSYS .EQ.0) IRC = -1
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      IF(IRC.NE.0) THEN
         CHSYS  = 'CERN'
         LCHSYS = 4
#endif
#if defined(CERNLIB_IBMVM)
         PRINT *,'FMQFAT. REXX variable FATSYS not defined. '
#endif
#if defined(CERNLIB_VAXVMS)
         PRINT *,'FMQFAT. symbol FATSYS not defined.'
#endif
#if defined(CERNLIB_UNIX)
         PRINT *,'FMQFAT. environmental variable FATSYS not defined. '
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
         PRINT *,'FMQFAT. defaulted to CERN'
      ENDIF
#endif
      CALL CLTOU(CHSYS)
      IF(CHSYS(1:2).NE.'//') THEN
         CHTEMP = '//'//CHSYS(1:LCHSYS)
      ELSE
         CHTEMP = CHSYS(1:LCHSYS)
      ENDIF
      CHSYS = CHTEMP
#if defined(CERNLIB_IBMVM)
*
*     Take username from REXX variable 'FATMAN'
*     If not defined, use current username
*     This username determines the FATMEN group for whom we are working...
*
      CALL VMREXX('F','FATMAN',CHGRP,IRC)
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
      CALL GETENVF('FATGRP',CHGRP)
      LDEF = IS(1)
      IRC = 0
      IF(LDEF.EQ.0) IRC = -1
#endif
#if defined(CERNLIB_IBMMVS)
      IF(NPARMS.GE.2) THEN
         CALL FMWORD(CHGRP,1,',',CHPARM(1:LPARM),IRC)
      ELSE
         PRINT *,'FMQFAT. GOPARM variable FATUSER not defined. '
         IRC = -1
      ENDIF
#endif
      IF(IRC.NE.0) THEN
         LCHGRP = LENOCC(CHGRP)
         IF(CHGRP(1:2).NE.'FM') THEN
            CHTEMP = 'FM'//CHGRP(1:LCHGRP)
         ELSE
            CHTEMP = CHGRP(1:LCHGRP)
         ENDIF
         CHGRP = CHTEMP
      ENDIF
 
 
      END
