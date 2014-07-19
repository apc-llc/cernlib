*
* $Id: hqlif2.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqlif2.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 12/09/92  13.07.41  by  John Allison
*-- Author :    John Allison   02/09/92
      SUBROUTINE HQLIF2 (IHQERR)
      INTEGER IHQERR
* Lifts LQ (LHQUAD - 2) chain once no. of significant points is known.
* See HQINIT for description of bank structure.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
 
      INTEGER IQUEST
      COMMON /QUEST/ IQUEST (100)
 
      CHARACTER*80 CHQMES
      INTEGER NTOT, NWCOV, NWDER, NWWK1, NWWK2, NWWK3
      INTEGER NWW, NIO
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 2, NIO = 4)
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW = 1, NIO = 3)
#endif
 
* Calculate space needed (allow for aligning on 8-byte boundary).
      NWCOV = NWW * NSIG ** 2 + 1
      NWDER = NWW * NSIG ** 2 + 1
      NWWK1 = NWW * NSIG + 1
      NWWK2 = NWW * NSIG + 1
      NWWK3 = NWW * NSIG + 1
      NTOT  = NWCOV + NWDER + NWWK1 + NWWK2 + NWWK3 + 50
      CALL HSPACE (NTOT, 'HQLIF2', IDMQ)
      IF (IERR .NE. 0) GO TO 10
 
* Lift LQ (LHQUAD - 2) chain.
      CALL MZBOOK (IHDIV, LHQCOV, LHQUAD, -2, 'HQCO', 0, 0, NWCOV,
     +NIO, -1)
      CALL MZBOOK (IHDIV, LHQDER, LHQCOV,  0, 'HQDE', 0, 0, NWDER,
     +NIO, -1)
      CALL MZBOOK (IHDIV, LHQWK1, LHQDER,  0, 'HQW1', 0, 0, NWWK1,
     +NIO, -1)
      CALL MZBOOK (IHDIV, LHQWK2, LHQWK1,  0, 'HQW2', 0, 0, NWWK2,
     +NIO, -1)
      CALL MZBOOK (IHDIV, LHQWK3, LHQWK2,  0, 'HQW3', 0, 0, NWWK3,
     +NIO, -1)
 
      GO TO 20
 
   10 CONTINUE
      WRITE (CHQMES, '(''Need'', I8, '', short by'', I8)')
     +NTOT, -IQUEST(11)
      CALL HBUG (CHQMES, 'HQLIF2', IDMQ)
      IHQERR = 200
      GO TO 20
 
   20 CONTINUE
 
      END
 
