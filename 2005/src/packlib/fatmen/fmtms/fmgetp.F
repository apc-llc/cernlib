*
* $Id: fmgetp.F,v 1.1.1.1 1996/03/07 15:18:15 mclareni Exp $
*
* $Log: fmgetp.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:15  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMGETP(VID,POOL,CHOPT,IRC)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fattyp.inc"
      CHARACTER*(*) POOL,VID
      CHARACTER*6   VSN
      CHARACTER*80  COMAND
#include "fatmen/tmsrep.inc"
      PARAMETER     (LENTAP=100)
      CHARACTER*132 TAPREP(LENTAP)
      CHARACTER*8   GROUP
      PARAMETER     (MAXRET=1)
#include "fatmen/fatopts.inc"
 
      IRC = 0
*
*     Get VSN, VID, Density and Group
*
      COMAND = ' '
      WRITE(COMAND,9002) VID
 9002 FORMAT('QUERY VID ',A,' (VSN DENSITY GROUP')
      LC = LENOCC(COMAND)
      IF(IDEBFA.GT.0)
     +PRINT *,'FMGETP. Issuing TMS command ',COMAND(1:LC)
      I = LENREP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TMSREP,I)
      IF((IDEBFA.GE.0).AND.(IRC.NE.0)) THEN
         PRINT *,'FMGETP. Return code ',IRC,' from FMSREQ'
         PRINT *,TMSREP(1)
         RETURN
      ENDIF
 
      VSN   = TMSREP(1)(8:13)
      READ(TMSREP(1)(17:22),'(I6)') IDENS
      IF(IOPTG.NE.0) THEN
         IF(IDEBFA.GT.2)
     +   PRINT *,'FMGETP. Option G specified: group set to *None'
         GROUP = '*None   '
      ELSE
         GROUP = TMSREP(1)(24:31)
         IF(IDEBFA.GT.2)
     +   PRINT *,'FMGETP. Group will remain ',GROUP
      ENDIF
 
      COMAND = ' '
      WRITE(COMAND,9003) VID,POOL,GROUP
 9003 FORMAT('GETPOOL VID ',A,' FROM ',A,' NEWGROUP ',A)
      LC = LENOCC(COMAND)
      IF(IDEBFA.GT.0)
     +PRINT *,'FMGETP. Issuing TMS command ',COMAND(1:LC)
      I = LENREP
      CALL FMSREQ('TMS     ',COMAND(1:LC),
     +            IRC,TMSREP,I)
 
      IF((IDEBFA.GE.0).AND.(IRC.NE.0)) THEN
         PRINT *,'FMGETP. Return code ',IRC,' from FMSREQ'
         PRINT *,TMSREP(1)
         RETURN
      ENDIF
 
      END
