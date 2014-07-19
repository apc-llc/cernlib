*
* $Id: icmfin.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: icmfin.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.34.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION ICMFIN(IDUMMY)
*****************************************************
*                                                   *
*   SYSTEM-INDEPENDENT ROUTINE: FINISH PROCESSING   *
*   OF THE COMMON BLOCKS' TABLE.                    *
*                                                   *
*   ICMFIN = NUMBER OF RECORDS IN THE TABLE         *
*                                                   *
*****************************************************
#include "comis/cstbls.inc"
      COMMON /CSPAG/ IPAG
*
*   CONVERT THE TEMPORARY TABLE TO THE FINAL ONE
*
      ICMFIN=MLLTOT(ITBLC,ITBFC)
*
*   STORE THE ROOT OF THE TABLE IN THE WORD 0
*
      L=MLROOT(ITBFC,IROOT)
      CALL MPSETW(IPAG,0,IROOT)
*
*   SAVE ALL PAGES IN THE FILE
*
      CALL MPSAV(IPAG)
*
*   NOW, DELETE THE TEMPORARY TABLE
*
      CALL MLLMDL(ITBLC,1)
      RETURN
      END
