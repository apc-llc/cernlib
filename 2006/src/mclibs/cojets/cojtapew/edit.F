*
* $Id: edit.F,v 1.1.1.1 1996/01/11 14:15:02 mclareni Exp $
*
* $Log: edit.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:02  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      LOGICAL FUNCTION EDIT(I)
C     ************************
C-- FROM ISAJET
C          DUMMY ROUTINE FOR USER DEFINED LOGICAL FUNCTION
C          EDIT=.TRUE. TO WRITE EVENT ON TAPE
C          EDIT=.FALSE. TO SUPPRESS WRITING
C          NOTE: DO NOT PUT COMMON /PARTCL/ HERE -- ITS CONTENT IS FULLY
C          USABLE, BUT IT IS MADE AVAILABLE IN ANOTHER WAY
C-- CREATED: 88/05/08
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/event.inc"
#include "cojets/final.inc"
#include "cojets/fstate.inc"
#include "cojets/idrun.inc"
#include "cojets/itapes.inc"
#include "cojets/jetpar.inc"
#include "cojets/jetset.inc"
#include "cojets/keys.inc"
#include "cojets/pfirst.inc"
#include "cojets/pinits.inc"
#include "cojets/pjets.inc"
      NPTCL=NPART
      EDIT=.TRUE.
      RETURN
      END
