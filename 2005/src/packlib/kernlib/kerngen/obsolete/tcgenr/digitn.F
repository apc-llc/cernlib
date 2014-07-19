*
* $Id: digitn.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: digitn.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION DIGITN  (NN,A)
C
C
      COMMON /SLATE/BIN,B(10),XQX(29)
      EQUIVALENCE (BIN,IBIN)
C
C
      ICONV= 0
      GO TO 12
C
         ENTRY IDIGIT                                                    CDC
C        ENTRY IDIGIT  (NN,A)                                           -CDC
C
      ICONV= 1
C
   12 N= NN
      CALL UBLOW (A,B,N)
      BIN= B(N)
      IF (ICONV.NE.0)  IBIN=IFROMA(BIN)
      DIGITN= BIN
C     IDIGIT= IBIN                                                      IBM
      RETURN
      END
