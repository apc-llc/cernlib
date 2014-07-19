*
* $Id: zfrzcp.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zfrzcp.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFRZCP
*
*     Action routine for RZCOPY command
*
#include "cspack/slate.inc"
#include "cspack/czsock.inc"
      CHARACTER*64 LOCAL,REMOTE
      CHARACTER*4  CHOPT
 
      CALL KUGETS(LOCAL,NCHL)
      CALL KUGETS(REMOTE,NCHR)
      CALL KUGETI(LRECL)
      CALL KUGETI(NRECS)
      CALL KUGETC(CHOPT,NCH)
#include "cspack/czfold.inc"
*
*     Pass quota for new file in SLATE (sorry)
*
      IS(1) = NRECS
      CALL XZRZCP(LOCAL,REMOTE,LRECL,CHOPT,IRC)
99    END
 
