*
* $Id: isbfin.F,v 1.1.1.1 1996/02/26 17:16:41 mclareni Exp $
*
* $Log: isbfin.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:41  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.13/00 27/03/92  14.34.06  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION ISBFIN(IDUMMY)
*****************************************************
*                                                   *
*   SYSTEM-INDEPENDENT ROUTINE: FINISH PROCESSING   *
*   OF THE SUBROUTINES' TABLE.                      *
*                                                   *
*   ISBFIN = NUMBER OF RECORDS IN THE TABLE         *
*                                                   *
*****************************************************
#include "comis/cstbls.inc"
      COMMON /CSPAG/ IPAG
*
*   CONVERT THE TEMPORARY TABLE TO THE FINAL ONE
*
      ISBFIN=MLLTOT(ITBLS,ITBFS)
*
*   STORE THE ROOT OF THE TABLE IN THE WORD 1
*
      L=MLROOT(ITBFS,IROOT)
      CALL MPSETW(IPAG,1,IROOT)
*
*   SAVE ALL PAGES IN THE FILE
*
      CALL MPSAV(IPAG)
*
*   NOW, DELETE THE TEMPORARY TABLE
*
      CALL MLLMDL(ITBLS,1)
      RETURN
      END
