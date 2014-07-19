*
* $Id: renamef.F,v 1.1.1.1 1996/02/15 17:52:27 mclareni Exp $
*
* $Log: renamef.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:27  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      INTEGER FUNCTION RENAMEF (FROM,TO)
C
C CERN PROGLIB#         RENAMEF         .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to rename
C
      CHARACTER*(*)  FROM, TO
      INTEGER        RENAME

      RENAMEF = RENAME (FROM, TO)
      RETURN
      END
#ifdef CERNLIB_CCGEN_RENAMEF
#undef CERNLIB_CCGEN_RENAMEF
#endif
#ifdef CERNLIB_CCGEN_RENAMI
#undef CERNLIB_CCGEN_RENAMI
#endif
