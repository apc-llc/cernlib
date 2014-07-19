*
* $Id: fmscan.F,v 1.1.1.1 1996/03/07 15:18:24 mclareni Exp $
*
* $Log: fmscan.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:24  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSCAN(CHPATH,FILES,KEYS,KEYSEL,NFIRST,NLAST,NMAX,
     +                  CHOPT,IRC)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      PARAMETER (LKEYFA=10)
      PARAMETER (MAXFIL=10000)
      CHARACTER*(*) CHPATH,FILES(NMAX)
      DIMENSION KEYS(LKEYFA,NMAX)
      DIMENSION JSORT(MAXFIL)
*
*     CHOPT: R - search for generic names for which a ROBOT copy exists
*            M - search for generic names for which a MANUAL copy exists
*            D - search for generic names for which a DISK  copy exists
*            C - search for generic names for which multiple copies exist
*            U - call FUSCAN for all matching entries
*     RC implies that there are multiple copies, one of which is in the
*        robot,
*     DR implies that a copy exists both on disk and in the robot etc.
*
*     KEYSEL: keys vector as for call to FMSELK, e.g.
*     KEYSEL(MKLCFA) = 1
*     KEYSEL(MKMTFA) = 2
*     KEYSEL(MKCLFA) = 2
*     would look for location code 1 (CERN), media type 2 (3480)
*     and copy level 2 (byte-swapped IEEE format, e.g. DEC/Ultrix)
*
#include "fatmen/fatopts.inc"
*
*     Subroutine to return a list of FATMEN-selected generic names
*     corresponding to the (wild-carded) generic name and current
*     selection criteria
*
      IRC = 0
      IF((NFIRST.LE.0).OR.(NLAST.LT.NFIRST)) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSCAN - NFIRST/NLAST invalid ',
     +                   NFIRST,NLAST
         IRC = 1
         RETURN
         ENDIF
 
      CALL FMLIST(CHPATH,FILES,KEYS,NFOUND,NMAX,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSCAN - return code ',IRC,
     +                   'from FMLIST'
         RETURN
         ENDIF
 
*
*     Sort this list...
*
      CALL FMSORT(FILES,KEYS,NFOUND,JSORT,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSCAN - return code ',IRC,
     +                   'from FMSORT'
         RETURN
         ENDIF
 
*
*     For NFIRST to NLAST, make FATMEN selection
*
      DO 10 I=NFIRST,NLAST
      CALL FMGET(FILES,LBANK,KEYS,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSCAN - return code ',IRC,
     +                   'from FMGET'
         RETURN
         ENDIF
 
*
*     Make user selection on this entry
*
      IF(IOPTU.NE.0) THEN
         ISEL = 0
         CALL FUSCAN(FILES(I),LBANK,KEYS,ISEL)
         IF(ISEL.NE.0) GOTO 10
      ENDIF
*
*     Move generic name and keys to output array
*
*     CALL UCOPY
10    CONTINUE
      END
 
