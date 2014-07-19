*
* $Id: fmsetm.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: fmsetm.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSETM(MTP,NMTP,IC)
*CMZ :          22/01/91  16.33.20  by  Jamie Shiers
*-- Author :    Jamie Shiers   22/01/91
*     Routine to set the media type vector
*
      DIMENSION MTP(NMTP)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatmtp.inc"
      IC = 0
      NUMMTP = 0
      IF(NMTP.GT.KMXMTP) IC = 1
      DO 10 I=1,MIN(NMTP,KMXMTP)
      IF(MTP(I).LE.0) THEN
         IF(IDEBFA.GE.0) PRINT *,'FMSETM. Invalid media type ',MTP(I),
     +   ' ignored. Media types must be positive integers'
         IC = 2
         GOTO 10
      ENDIF
      NUMMTP = NUMMTP + 1
      MFMMTP(NUMMTP) = MTP(I)
10    CONTINUE
      END
