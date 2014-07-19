*
* $Id: cdfzin.F,v 1.1.1.1 1996/02/28 16:24:20 mclareni Exp $
*
* $Log: cdfzin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFZIN (AI, NDI, AO, NDO, EPSIL, IER)
*     ================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFZIN (AI, NDI, AO*, NDO*, EPSIL, IER*)                *
*                                                                      *
*   Compress floating point data (only nonzero elements are stored).   *
*   Data are set to 0 if their absolute value is less than EPSIL       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     AI       Input array to be compressed                            *
*     NDI      Number of elements in AI                                *
*     AO       Output array of the nonzero elements                    *
*     NDO      Number of elements in AO                                *
*     EPSIL    Cutoff value                                            *
*     IER      Error Code : 0    No error                              *
*                           1    NDO .ge. NDI                          *
*                                                                      *
*   Called by CDCMPZ                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IER       =  0 : No error (see above)                            *
*                                                                      *
************************************************************************
*
      DIMENSION       AI(NDI), AO(NDI)
*
*     ------------------------------------------------------------------
*
      IER    = 0
*
      NDO    = 0
      DO 10 I = 1, NDI
        IF (ABS(AI(I)).LE.EPSIL) GO TO 10
        NDO    = NDO + 1
        IF (NDO.GE.NDI) THEN
          IER    = 1
          GO TO 999
        ENDIF
        AO(NDO) = I
        NDO    = NDO + 1
        IF (NDO.GE.NDI) THEN
          IER    = 1
          GO TO 999
        ENDIF
        AO(NDO) = AI(I)
   10 CONTINUE
*                                                             END CDFZIN
  999 END
