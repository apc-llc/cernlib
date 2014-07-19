*
* $Id: ighcx.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: ighcx.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/04 07/06/94  09.55.36  by  O.Couet
*-- Author :
      REAL FUNCTION IGHCX(LSCAT,ICX)
*.==========>
*.           Returns the value of bin number icx
*..=========> ( R.Brun )
#include "higz/hipaw.inc"
      PARAMETER(KNBIT=1,KCON1=9)
*.___________________________________________
*
      NB            = IQ(LSCAT+KNBIT)
      IF(NB.LE.0)NB = 32
      NW            = 32/NB
      IF(NW.NE.1)THEN
         L1    = ICX*NB
         NBITH = 32-MOD(32,NB)
         L2    = MOD(L1,NBITH)+1
         L1    = LSCAT+KCON1+L1/NBITH
         IGHCX = JBYT(IQ(L1),L2,NB)
      ELSE
         IGHCX = Q(LSCAT+KCON1+ICX)
      ENDIF
*
      END
