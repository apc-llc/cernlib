*
* $Id: cdupiz.F,v 1.1.1.1 1996/02/28 16:24:36 mclareni Exp $
*
* $Log: cdupiz.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDUPIZ (IAI, NDI, IAO, NDO)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDUPIZ (IAI, NDI, IAO*, NDO)                            *
*                                                                      *
*   Uncompress integer data when only nonzero elements are stored      *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IAI      Input array to be uncompressed                          *
*     NDI      Number of elements in IAI                               *
*     IAO      Output array of the uncompressed elements               *
*     NDO      Number of elements in IAO                               *
*                                                                      *
*   Called by CDUCMZ                                                   *
*                                                                      *
************************************************************************
*
      DIMENSION       IAI(NDO), IAO(NDO)
*
*     ------------------------------------------------------------------
*
      CALL VZERO (IAO(1), NDO)
*
      DO 10 I = 1, NDI
        I2     = 2*I
        I1     = I2 - 1
        J      = IAI(I1)
        IAO(J) = IAI(I2)
   10 CONTINUE
*                                                             END CDUPIZ
      END
