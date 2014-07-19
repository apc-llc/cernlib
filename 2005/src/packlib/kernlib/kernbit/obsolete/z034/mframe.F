*
* $Id: mframe.F,v 1.1.1.1 1996/02/15 17:47:43 mclareni Exp $
*
* $Log: mframe.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:43  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "_kernbit/pilot.h"
#if defined(CERNLIB_IBM)
      SUBROUTINE MFRAME (A)
      DATA B/4H9000/
      A = B
      RETURN
      END
#endif
#if (defined(CERNLIB_CDC))&&(!defined(CERNLIB_NOSBE))
      SUBROUTINE MFRAME (A)
      DATA B/4HMFZ /
      A = B
      RETURN
      END
#endif
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_NOSBE))
      SUBROUTINE MFRAME(A)
C         IDENTIFY CDC FRONTEND MAINFRAME MFA OR MFB
      DIMENSION TID(70)
      SSHPAR = SHIFT(LOCF(TID(1)),12)
#endif
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_NOSBE))&&(defined(CERNLIB_F4))
      CALL REQPP(4LSSHP.OR.2000000B.OR.LOCF(SSHPAR))
#endif
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_NOSBE))&&(!defined(CERNLIB_F4))
      CALL REQPP(L"SSHP".OR.O"2000000".OR.LOCF(SSHPAR))
#endif
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_NOSBE))
      A = 10H
      CALL CHMOVE(TID,8,A,1,3)
      RETURN
      END
#endif
