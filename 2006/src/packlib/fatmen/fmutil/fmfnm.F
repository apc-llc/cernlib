*
* $Id: fmfnm.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fmfnm.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFNM(HFNM)
      CHARACTER*(*)  HFNM
      CHARACTER*8    FTYPE,CHUSER
      INTEGER FMJOB,FMUSER
 
      ICODE=FMUSER(CHUSER)
      CALL DATIME(ID,IT)
      CALL FMPKTM(ID,IT,IP,IRC)
      WRITE(FTYPE,9001) IP
9001  FORMAT(Z8)
      LUSER = LENOCC(CHUSER)
      IF(LUSER.EQ.0) THEN
         CHUSER = 'UNKNOWN'
         LUSER  = 7
      ENDIF
      HFNM  = CHUSER(1:LUSER) // '.' // FTYPE
      END
