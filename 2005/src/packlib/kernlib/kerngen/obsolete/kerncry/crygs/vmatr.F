*
* $Id: vmatr.F,v 1.1.1.1 1996/02/15 17:52:41 mclareni Exp $
*
* $Log: vmatr.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:41  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE VMATR (A,G,X,N,K)
C
C CERN PROGLIB# F121    VMATR           .VERSION KERNCRY  1.11  890627
C ORIG. 30/05/89  FCA
C
      DIMENSION A(*),X(*),G(K,N)
C
      CALL SGEMV ('N',K,N,1.,G,K,A,1,0.,X,1)
      END
#ifdef CERNLIB_TCGEN_VMATR
#undef CERNLIB_TCGEN_VMATR
#endif
