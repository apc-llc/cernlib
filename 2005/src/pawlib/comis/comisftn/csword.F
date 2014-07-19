*
* $Id: csword.F,v 1.1.1.1 1996/02/26 17:16:50 mclareni Exp $
*
* $Log: csword.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:50  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_LIB)
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSWORD(JW,IV)
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
      IA(JW-JTOPA)=IV
      END
C+DECK,MLCEQCA,IF=IBM,APOLLO.
C      FUNCTION MLCEQC(KC1,KC2)
C      MLCEQC=0
C      IF(KC1.EQ.KC2)MLCEQC=1
C      END
C+DECK,MLCEQCV,IF=VAX.
C      FUNCTION MLCEQC(IC1,IC2)
C      BYTE IC1(4),IC2(4)
C      MLCEQC=0
C      IF(IC1(1).EQ.IC2(1))MLCEQC=1
C      END
#endif
