*
* $Id: mlnumb.F,v 1.1.1.1 1996/02/26 17:16:57 mclareni Exp $
*
* $Log: mlnumb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:57  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)||defined(CERNLIB_APOLLO)||!defined(CERNLIB_ASSEMB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION MLNUMB(J,N,I)
C=========================================================================
C  CONSIDERS STRING (J,N) AS A SYMBOLIC REPRESENTATION OF INTEGER NUMBER I
C  (I.E. NUMBER IS CONSIDERED FROM THE BEGINNING OF STRING TO THE FIRST
C  NON-DIGIT OR TO THE END OF STRING).
C   RESULTS:
C  I - NUMBER,
C  MLNUMB=1 ,IF CONTENT OF STRING IS A SYMBOLIC REPRESENTATION OF NUMBER
C           (I.E. THE FIRST SYMBOL IS A DIGIT).
C       =0 ,IF NOT.
C=========================================================================
      I=0
      MLNUMB=0
    1 IF(N) 4,4,2
    2 K=MKCHAR(J)
      IF(MLDIGI(K)) 4,4,3
    3 MLNUMB=1
      I=I*10+MDIGIT(K)
      J=J+1
      N=N-1
      GO TO 1
    4 RETURN
      END
#endif
