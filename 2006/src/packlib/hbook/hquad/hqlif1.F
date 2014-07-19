*
* $Id: hqlif1.F,v 1.1.1.1 1996/01/16 17:08:03 mclareni Exp $
*
* $Log: hqlif1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:03  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 18/12/92  14.21.59  by  John Allison
*-- Author :    John Allison   06/09/92
      SUBROUTINE HQLIF1 (IHQERR)
      INTEGER IHQERR
* Lifts LQ (LHQUAD - 1) chain, L1H etc.
* See HQINIT for description of bank structure.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
 
      INTEGER IQUEST
      COMMON /QUEST/ IQUEST (100)
 
      CHARACTER*80 CHQMES
      INTEGER NTOT, NW1, NW2, NW3
 
* Calculate space required.
      NW1 = 0
      NW2 = 0
      NW3 = 0
      IF (NDIM .EQ. 1) THEN
         NW1 = NX
      ELSE IF (NDIM .EQ. 2) THEN
         NW1 = MAX (NX, NY)
         NW2 = NX * NY
      ELSE IF (NDIM .EQ. 3) THEN
         NW1 = MAX (NX, NY, NZ)
         NW2 = MAX (NX * NY, NY * NZ, NZ * NX)
         NW3 = NX * NY * NZ
      ELSE
         GO TO 10
      END IF
      NTOT = 2 * (NW1 + NW2 + NW3) + 10 * NDIM
 
* Inquire if there's space.
      CALL HSPACE (NTOT, 'HQLIF1', IDMQ)
      IF (IERR .NE. 0) GO TO 20
 
* Book 1st bank of contents/variances linear structure:
*   1-D histogram contents.
      CALL MZBOOK (IHDIV, L1H, LHQUAD, -1, 'HQ1H', 0, 0, NW1, 3, -1)
* Book 2nd bank - 1-D variances.
      CALL MZBOOK (IHDIV, L1V, L1H, 0, 'HQ1V', 0, 0, NW1, 3, -1)
* Similarly for 2-D histograms.
      IF (NDIM .GE. 2) THEN
         CALL MZBOOK (IHDIV, L2H, L1V, 0, 'HQ2H', 0, 0, NW2, 3, -1)
         CALL MZBOOK (IHDIV, L2V, L2H, 0, 'HQ2V', 0, 0, NW2, 3, -1)
      END IF
* Similarly for 3-D histograms.
      IF (NDIM .GE. 3) THEN
         CALL MZBOOK (IHDIV, L3H, L2V, 0, 'HQ3H', 0, 0, NW3, 3, -1)
         CALL MZBOOK (IHDIV, L3V, L3H, 0, 'HQ3V', 0, 0, NW3, 3, -1)
      END IF
      IF (NDIM .GE. 4) GO TO 10
 
      GO TO 40
 
   10 CONTINUE
      WRITE (CHQMES, '(''Not programmed for'', I3,
     +'' dimensions.'')') NDIM
      IHQERR = 100
      GO TO 30
   20 CONTINUE
      WRITE (CHQMES, '(''Need'', I8, '', short by'', I8)')
     +NTOT, -IQUEST(11)
      IHQERR = 200
      GO TO 30
 
   30 CONTINUE
      CALL HBUG (CHQMES, 'HQLIF1', IDMQ)
 
   40 CONTINUE
 
      END
