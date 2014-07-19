*
* $Id: fmendk.F,v 1.1.1.1 1996/03/07 15:17:36 mclareni Exp $
*
* $Log: fmendk.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:36  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMENDK(IRC)
*CMZ :          17/09/91  12.00.51  by  Jamie Shiers
*-- Author :    Jamie Shiers   17/09/91
#include "fatmen/fatbank.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatron.inc"
      IF(THRONG(1:LTHR).EQ.'NONE') RETURN
#if defined(CERNLIB_IBMVM)
      IF(SERNAM(1:4).NE.'INIT'.AND.SERMOD.NE.'?'
     +   .AND.SERMOD.NE.'A') THEN
         CALL VMCMS('EXEC DROP '//SERMOD,IRC)
         SERMOD = '?'
      ENDIF
#endif
      LURZFA = IQ(LTOPFA+KOFUFA+MLUNFA)
      CALL FMEND(IRC)
      IF(LURZFA.GT.1000) THEN
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
         IF(IDEBFA.GE.3) PRINT *,'FMENDK. closing unit ',LURZFA-1000,
     +      ' (C)'
         CALL CFCLOS(LURZFA-1000,0)
#endif
      ELSE
         IF(IDEBFA.GE.3) PRINT *,'FMENDK. closing unit ',LURZFA,
     +      ' (FORTRAN)'
         CLOSE(LURZFA)
      ENDIF
      THRONG = 'NONE'
      LTHR   = 4
      END
