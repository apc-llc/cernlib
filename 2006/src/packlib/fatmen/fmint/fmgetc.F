*
* $Id: fmgetc.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmgetc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGETC(LBANK,STRING,ISTART,NCH,IRC)
*
*     Routine to get a STRING from the bank at LBANK
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      CHARACTER*(*) STRING
      JL  = ISTART
      JR  = ISTART -1 + (NCH+3)/4
      IRC = 0
*
*     Check that JL:JR is a valid range
*
      IF(JR.LT.JL) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMGETC. invalid range, end < start'
            PRINT *,'FMGETC. ISTART/IEND = ',JL,JR
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
*
*     Check that we are not outside the bank
*
      IF((JL.LE.0).OR.(JR.GT.NWDSFA)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMGETC. attempt to read outside bank boundaries'
            PRINT *,'FMGETC. ISTART/IEND = ',JL,JR
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
 
      LADDR = LBANK+KOFUFA
      IF(LBANK.EQ.-1) LADDR = LADDBK+KOFUFA
*
*     Special case: VID prefix
*
      IF(ISTART.EQ.MVIPFA) THEN
         CALL FMPREF(STRING,IQ(LADDR+JL),'I',IRC)
      ELSE
         CALL UHTOC(IQ(LADDR+JL),4,STRING,NCH)
      ENDIF
      END
 
