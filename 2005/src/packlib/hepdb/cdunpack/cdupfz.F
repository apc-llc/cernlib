*
* $Id: cdupfz.F,v 1.1.1.1 1996/02/28 16:24:36 mclareni Exp $
*
* $Log: cdupfz.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUPFZ (AI, NDI, AO, NDO)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUPFZ (AI, NDI, AO*, NDO)                              *
*                                                                      *
*   Unompress floating point data  when only nonzero elements are      *
*   stored                                                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     AI       Input array to be uncompressed                          *
*     NDI      Number of elements in AI                                *
*     AO       Output array of the uncompressed elements               *
*     NDO      Number of elements in AO                                *
*                                                                      *
*   Called by CDUCMZ                                                   *
*                                                                      *
************************************************************************
*
      DIMENSION       AI(NDO), AO(NDO)
*
*     ------------------------------------------------------------------
*
      CALL VZERO (AO(1), NDO)
*
      DO 10 I = 1, NDI
        I2     = 2*I
        I1     = I2 - 1
        J      = AI(I1)
        AO(J)  = AI(I2)
   10 CONTINUE
*                                                             END CDUPFZ
      END
