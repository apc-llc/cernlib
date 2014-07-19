*
* $Id: hqxi.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hqxi.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 02/09/92  19.10.43  by  John Allison
*-- Author :
      REAL FUNCTION HQXI (IA)
      INTEGER IA (*)
* Returns contents of MQ histogram indexed by IA, which is a vector of
*   channel numbers - IA (1) is x-channel, IA (2) is y-channel, etc.
 
#include "hbook/hcqcom.inc"
#include "hbook/hcbook.inc"
 
      CHARACTER*80 CHQMES
 
      INTEGER L, IX, IY, IZ
 
      IF (NDIM .EQ. 1) THEN
         IX = IA (1)
         L = IX
         HQXI = Q (L1H + L)
      ELSE IF (NDIM .EQ. 2) THEN
         IX = IA (1)
         IY = IA (2)
         L = (IY - 1) * NX + IX
         HQXI = Q (L2H + L)
      ELSE IF (NDIM .EQ. 3) THEN
         IX = IA (1)
         IY = IA (2)
         IZ = IA (3)
         L = (IZ - 1) * NX * NY + (IY - 1) * NX + IX
         HQXI = Q (L3H + L)
      ELSE
         GO TO 10
      END IF
 
      GO TO 20
 
* Errors.
   10 CONTINUE
      WRITE (CHQMES, '(I3, '' dimensions not programmed yet.'')') NDIM
      CALL HBUG (CHQMES, 'HQXI', IDMQ)
 
   20 CONTINUE
 
      END
