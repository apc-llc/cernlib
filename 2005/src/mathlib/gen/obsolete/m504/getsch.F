*
* $Id: getsch.F,v 1.1.1.1 1996/04/01 15:03:21 mclareni Exp $
*
* $Log: getsch.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:21  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE GETSCH(STR,DEL,TRM,NTH,SST,NCH)
*   Return in SST the character string found between the NTH-1 and NTH
*   occurence of the delimiter 'DEL' but before the terminator 'TRM' in
*   the input string 'STR'. If TRM=DEL then STR is considered unlimited.
*   NCH=Length of the string returned in SST or =-1 if NTH is <1 or if
*   NTH is greater than the number of delimiters in STR.
      IMPLICIT INTEGER (A-Z)
      CHARACTER STR*(*),DEL*1,TRM*1,SST*(*)
      NCH=-1
      SST=' '
      IF(NTH.GT.0) THEN
        IF(TRM.EQ.DEL) THEN
          LENGTH=LEN(STR)
        ELSE
          LENGTH=INDEX(STR,TRM)-1
          IF(LENGTH.LT.0) LENGTH=LEN(STR)
        ENDIF
*     Find beginning and end of the NTH DEL-limited substring in STR
        END=-1
        DO 1,N=1,NTH
        IF(END.EQ.LENGTH) RETURN
        BEG=END+2
        END=BEG+INDEX(STR(BEG:LENGTH),DEL)-2
        IF(END.EQ.BEG-2) END=LENGTH
*        PRINT *,'NTH,LENGTH,N,BEG,END=',NTH,LENGTH,N,BEG,END
    1   CONTINUE
        NCH=END-BEG+1
        IF(NCH.GT.0) SST=STR(BEG:END)
      ENDIF
      END
