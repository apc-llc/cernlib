*
* $Id: fmexst.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fmexst.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMEXST(GENEN,IRC)
*
*     Routine to check whether specified generic name
*     already exists on RZ file. Does not attempt to
*     read in data structure - just uses keys
*     N.B. the maximum number of copies of a file is limited to MAXCOP
*
      CHARACTER*(*) GENEN
#include "fatmen/fmaxcop.inc"
      PARAMETER (LKEYFA=10)
      DIMENSION KEYSIN(LKEYFA),KEYSOU(LKEYFA,MAXCOP)
#include "fatmen/fatpara.inc"
      IRC    = 0
      NFOUND = 0
      NCH    = LENOCC(GENEN)
      LCH    = INDEXB(GENEN(1:NCH),'/') + 1
 
      CALL VBLANK(KEYSIN(2),5)
      CALL UCTOH(GENEN(LCH:NCH),KEYSIN(2),4,MIN(NCH-LCH+1,20))
*
*     Don't compare copy level, location code or media type
*
      KEYSIN(MKCLFA) = -1
      KEYSIN(MKLCFA) = -1
      KEYSIN(MKMTFA) = -1
      KEYSIN(1)      = 0
      KEYSIN(10)     = 10
 
      CALL FMSELK(GENEN(1:NCH),KEYSIN,KEYSOU,NFOUND,MAXCOP,IRC)
 
      IRC = NFOUND
 
      END
 
