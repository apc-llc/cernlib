*
* $Id: fmdump.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmdump.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDUMP(GENAME,KEYS,NBYTES,NBLOCKS,NFILES,CODE,IRC)
*
*     Request tape dump of VID corresponding to specified generic name
*
#include "fatmen/fmpath.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsel.inc"
#include "fatmen/fatbank.inc"
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
      CHARACTER*(*) GENAME
      CHARACTER*(*) CODE
      CHARACTER*6   VID
      CHARACTER*15  XVID
      CHARACTER*8   VIP
      CHARACTER*255 COMAND
      INTEGER       SYSTEMF
#include "fatmen/tmsdef.inc"
      IRC = 0
#if (!defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_CRAY))
      PRINT *,'This routine is currently only available on VM/CMS and',
     +        ' CRAY/Unicos systems'
      IRC = 99
      RETURN
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_CRAY)
*
*     Set media type range from 3480 to 3420
*
      MRMTFA(1) = 2
      MRMTFA(2) = 3
      LGN = LENOCC(GENAME)
      CALL FMGETK(GENAME(1:LGN),LBANK,KEYS,IRC)
*
*     Restore default FATMEN media type range
*
      MRMTFA(1) = 1
      MRMTFA(2) = 3
      IF(IRC.NE.0) THEN
         PRINT *,'Return code ',IRC,' from FMGETK'
         RETURN
         ENDIF
 
      CALL FMGETC(LBANK,VID,MVIDFA,6,IRC)
      LVID = LENOCC(VID)
*
*     Set IQUEST(11) to media type in case volume unknown or
*     TMS option not installed.
*
      IQUEST(11) = IQ(LBANK+MMTPFA)
#endif
#if defined(CERNLIB_PREFIX)
       CALL FMXVID(VID,IQ(LBANK+MVIPFA),XVID,VIP,'C',IC)
       LXVID = LENOCC(XVID)
       CALL FMQTMS(XVID(1:LXVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
       IF(IC.EQ.100) THEN
          IF(IDEBFA.GE.0) PRINT *,'Volume ',XVID,' unknown to TMS'
       ENDIF
#endif
#if (!defined(CERNLIB_PREFIX))&&(defined(CERNLIB_IBMVM)||defined(CERNLIB_CRAY))
      CALL FMQTMS(VID(1:LVID),LIB,MODEL,DENS,MNTTYP,LABTYP,IC)
      IF(IC.EQ.100) THEN
         IF(IDEBFA.GE.0) PRINT *,'Volume ',VID,' unknown to TMS'
      ENDIF
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_CRAY)
      LC = LENOCC(CODE)
      WRITE(COMAND,9001) VID,MODEL,NBYTES,NBLOCKS,NFILES,CODE(1:LC)
#endif
#if defined(CERNLIB_CRAY)
9001  FORMAT('dumptape -V ',A6,' -g ',A8,' -B ',I6,' - N 1,',I6,
     +       ' -F ',I6,' -C ',A6)
      IF(IDEBFA.GE.0)
     +PRINT *,'FMDUMP. executing ',COMAND(1:LENOCC(COMAND))
      IRC = SYSTEMF(COMAND(1:LENOCC(COMAND)))
#endif
#if defined(CERNLIB_IBMVM)
9001  FORMAT('EXEC DUMPTAPE VID ',A6,' DEVTYP ',A8,' BYTES ',I6,
     +       ' BLOCKS ',I6,' FILES ',I6,' CODE ',A6)
      IF(IDEBFA.GE.0)
     +PRINT *,'FMDUMP. executing ',COMAND(1:LENOCC(COMAND))
      CALL VMCMS(COMAND(1:LENOCC(COMAND)),IRC)
#endif
 
      END
 
