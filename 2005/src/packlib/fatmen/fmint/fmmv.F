*
* $Id: fmmv.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmmv.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMMV(GENAMA,GENAMB,IRC)
      CHARACTER*(*) GENAMA,GENAMB
      PARAMETER (LKEYFA=10)
      DIMENSION KEYS(LKEYFA)
#include "fatmen/faust.inc"
#include "fatmen/fatupd.inc"
#include "fatmen/fatbank.inc"
*
*     Check that we have enough update tokens left
*
      IF(MAXUPD-NUPDT.LT.2) THEN
         IF(IDEBFA.GE.0) PRINT 9001,NUPDT,MAXUPD
9001  FORMAT(' FMMVC. A "mv" operation requires two updates'/,
     +       '        You have made ',I10,' updates out of ',I10,
     +       ' maximum')
         RETURN
      ENDIF
 
      CALL FMGET(GENAMA,LTDSFA,KEYS,IRC)
      IF(IRC.EQ.0) THEN
          CALL FMRM(GENAMA,LTDSFA,KEYS,JRC)
          CALL FMPUT(GENAMB,LTDSFA,JRC)
          NFMVFL = NFMVFL + 1
      ENDIF
      END
 
