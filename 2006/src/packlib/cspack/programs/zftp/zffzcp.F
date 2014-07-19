*
* $Id: zffzcp.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zffzcp.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFFZCP
*
*     Action routine for FZCOPY command
*
#include "cspack/czsock.inc"
      CHARACTER*64 LOCAL,REMOTE
      CHARACTER*4  CHOPT,LFORM,RFORM
      INTEGER      IRECL,RRECL
 
      CALL KUGETS(LOCAL,NCHL)
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETC(LFORM,NLF)
      CALL KUGETI(LRECL)
      CALL KUGETC(RFORM,NRF)
      CALL KUGETI(RRECL)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
      CALL XZFZCP(LOCAL,REMOTE,LRECL,LFORM,RRECL,RFORM,CHOPT,IRC)
99    END
 
