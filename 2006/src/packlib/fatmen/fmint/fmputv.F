*
* $Id: fmputv.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmputv.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPUTV(LBANK,IVECT,ISTART,NWORDS,IRC)
*
*     Routine to put an INTEGER vector into the bank at LBANK
*
      DIMENSION IVECT(NWORDS)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      JL = ISTART
      JR = ISTART + NWORDS - 1
      IRC = 0
*
*     Check that JL:JR is a valid range
*
      IF(JR.LT.JL) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMPUTV. invalid range, end < start'
            PRINT *,'FMPUTV. ISTART/IEND = ',JL,JR
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
*
*     Check that we are not outside the bank
*
      IF((JL.LE.0).OR.(JR.GT.NWDSFA)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMPUTV. attempt to write outside bank boundaries'
            PRINT *,'FMPUTV. ISTART/IEND = ',JL,JR
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
      LADDR = LBANK+KOFUFA
      IF(LBANK.EQ.-1) LADDR = LADDBK + KOFUFA
 
      CALL UCOPY(IVECT,IQ(LADDR+JL),JR-JL+1)
      END
