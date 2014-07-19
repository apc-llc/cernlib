*
* $Id: fmend.F,v 1.2 1996/10/29 10:09:01 jamie Exp $
*
* $Log: fmend.F,v $
* Revision 1.2  1996/10/29 10:09:01  jamie
* Dont call FMUPDT for unit 0
*
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMEND(IRC)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fstate.inc"
#include "fatmen/fatupd.inc"
#include "fatmen/fmnkeys.inc"
      DIMENSION KEYS(LKEYFA)
 
      LURZFA = IQ(LTOPFA+KOFUFA+MLUNFA)
*
*     Ensure that any outstanding updates have been sent
*
      IF(MFPHAS.EQ.1) THEN
*
*     Send logging record
*
         MAXUPD = MAX(MAXUPD,0)
         IF(NUPDT.GE.MAXUPD) MAXUPD = NUPDT + 2
#if !defined(CERNLIB_IBMVM)
         IF(LUFZFA.GT.0) THEN
#endif
#if defined(CERNLIB_IBMVM)
         IF(LUFZFA.NE.0) THEN
#endif
            CALL FMFZO('LOG ',FATTOP(1:LENOCC(FATTOP)),0,KEYS,IRC)
            CALL FMUPDT(0,0,0,IRC)
            IF((IRC.NE.0) .AND. (IDEBFA.GT.-3))
     +        PRINT *,'FMEND. Return code ',IRC,' from FMUPDT.'
         ENDIF
      ENDIF
*
*     Set program phase
*
      MFPHAS = 3
 
      IRC=0
      IF(IDEBFA.GT.-3) WRITE(LPRTFA,9001)
#if !defined(CERNLIB_CZ)
      CALL RZEND(TOPDIR(3:LENOCC(TOPDIR)))
#endif
      CALL MZWIPE(IDIVFA)
9001  FORMAT(' FMEND. Terminating FATMEN package')
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_FMOTOZ))
      CALL FMONIT('End FATMEN')
#endif
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         CALL XZCLOS(LURZFA,' ',IRC)
      ENDIF
#endif
      END
