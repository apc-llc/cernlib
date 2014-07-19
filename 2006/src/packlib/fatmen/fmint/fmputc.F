*
* $Id: fmputc.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmputc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPUTC(LBANK,STRING,ISTART,NCH,IRC)
*
*     Routine to write a STRING into the bank at LBANK
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatusr.inc"
      CHARACTER*(*) STRING
      CHARACTER*255 CHSTR,CHBLA
      JL  = ISTART
      JR  = ISTART - 1 + (NCH+3)/4
      IRC = 0
      IF(NCH.EQ.0) RETURN
      LCH = LEN(STRING)
      CHSTR = STRING(1:LCH)
      CHBLA = ' '
*
*     Check that JL:JR is a valid range
*
      IF(JR.LT.JL) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMPUTC. invalid range, end < start'
            PRINT *,'FMPUTC. ISTART/IEND = ',JL,JR
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
*
*     Check that we are not outside the bank
*
      IF((JL.LE.0).OR.(JR.GT.NWDSFA)) THEN
         IF(IDEBFA.GE.0) THEN
            PRINT *,'FMPUTC. attempt to write outside bank boundaries'
            PRINT *,'FMPUTC. ISTART/IEND = ',JL,JR
         ENDIF
         IRC = 1
         RETURN
      ENDIF
 
      LADDR = LBANK+KOFUFA
      IF(LBANK.EQ.-1) LADDR = LADDBK+KOFUFA
*
*     Special case: VID prefix - first translate from CHARACTER to
*     integer.
*
      IF(ISTART.EQ.MVIPFA) THEN
         CALL FMPREF(STRING,JP,'C',IRC)
         IQ(LADDR+JL) = JP
      ELSE
         CALL UCTOH(CHSTR(1:LCH)//CHBLA,IQ(LADDR+JL),4,NCH)
      ENDIF
 
      END
