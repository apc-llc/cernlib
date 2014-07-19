*
* $Id: fmselb.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmselb.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSELB(GENAM,KEYSIN,NKEYS,UEXIT,ISEL,IRC)
*
*     This routine allows the user to select a copy of a dataset
*     based upon the information in the FATMEN bank
*
* Input:
*       GENAM  - the generic name in question
*       KEYSIN - the result of a call to FMSELK
* For each file FMSELB will obtain the corresponding FATMEN
* bank and pass it to the user-exit routine as follows:
*       CALL USER-EXIT(GENAM,LBANK,KEYS,NKEYS,N,ISEL,IRC)
*       NKEYS  = number of candidates
*       N      = number of this candidate
*       ISEL   = index of candidate selected
*       On the last call to USER-EXIT it should return
*       the index of the candidate selected, or zero.
#include "fatmen/fatbank.inc"
      EXTERNAL UEXIT
      CHARACTER*(*) GENAM
      PARAMETER (LKEYFA=10)
      DIMENSION KEYSIN(LKEYFA,NKEYS)
      IRC  = 0
      ISEL = 0
*
*     Loop over all candidates
*
      DO 10 I=1,NKEYS
      CALL FMGETK(GENAM,LBANK,KEYSIN(1,I),IRC)
 
      IF((IRC.NE.0) .AND. (IDEBFA.GT.-3)) THEN
         PRINT *,'FMSELB. Return code ',IRC,' from FMGETK.'
         GOTO 10
         ENDIF
 
      CALL UEXIT(GENAM,LBANK,KEYS,NKEYS,I,ISEL,IRC)
      IF((IRC.NE.0) .AND. (IDEBFA.GT.-3)) THEN
         PRINT *,'FMSELB. Return code ',IRC,' from user exit routine'
         GOTO 10
         ENDIF
 
10    CONTINUE
*
*     Have we got a selection?
*
      IF(ISEL.EQ.0) IRC = -1
 
      END
 
