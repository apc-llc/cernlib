*
* $Id: cdizin.F,v 1.1.1.1 1996/02/28 16:24:21 mclareni Exp $
*
* $Log: cdizin.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:21  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDIZIN (IAI, NDI, IAO, NDO, EPSIL, IER)
*     ==================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDIZIN (IAI, NDI, IAO*, NDO*, EPSIL, IER*)              *
*                                                                      *
*   Compress integer data (only nonzero elements are stored).          *
*   Data are set to 0 if their absolute value is less than EPSIL       *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IAI      Input array to be compressed                            *
*     NDI      Number of elements in IAI                               *
*     IAO      Output array of the nonzero elements                    *
*     NDO      Number of elements in IAO                               *
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
      DIMENSION       IAI(NDI), IAO(NDI)
*
*     ------------------------------------------------------------------
*
      IER    = 0
      IEPSI  = EPSIL
*
      NDO    = 0
      DO 10 I = 1, NDI
        IF (IABS(IAI(I)).LE.IEPSI) GO TO 10
        NDO    = NDO + 1
        IF (NDO.GE.NDI) THEN
          IER    = 1
          GO TO 999
        ENDIF
        IAO(NDO) = I
        NDO    = NDO + 1
        IF (NDO.GE.NDI) THEN
          IER    = 1
          GO TO 999
        ENDIF
        IAO(NDO) = IAI(I)
   10 CONTINUE
*                                                             END CDIZIN
  999 END
