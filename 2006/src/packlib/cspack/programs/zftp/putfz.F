*
* $Id: putfz.F,v 1.1.1.1 1996/03/08 15:44:19 mclareni Exp $
*
* $Log: putfz.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:19  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE PUTFZ
*
*         Action routine for PUTFZ command
*
#include "cspack/czsock.inc"
      CHARACTER*64 LOCAL,REMOTE
      CHARACTER*4  CHOPT,LFORM,RFORM
      INTEGER      RRECL
#include "cspack/czopen.inc"
      CALL KUGETS(LOCAL,NCHL)
      CALL KUGETS(REMOTE,NCHR)
      IF(NCHR.LE.0)REMOTE=LOCAL
      IF(REMOTE(1:NCHR).EQ.'=') REMOTE=LOCAL
      NCHR = LENOCC(REMOTE)
      CALL KUGETI(LRECL)
      CALL KUGETC(LFORM,NLF)
      CALL KUGETI(RRECL)
      CALL KUGETC(RFORM,NRF)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
      CALL XZPUTF(LOCAL,REMOTE,LRECL,LFORM,RRECL,RFORM,CHOPT,IRC)
99    END
 
