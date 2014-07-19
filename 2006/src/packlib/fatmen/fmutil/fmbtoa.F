*
* $Id: fmbtoa.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmbtoa.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMBTOA(GENAME,LBANK,KEYS,CHOPT,IRC)
*
*     Convert bank to ASCII format
*
      CHARACTER*(*) GENAME
      CHARACTER*255 CHTEXT
      CHARACTER*255 CHLINE,SPACES
      CHARACTER*2   CHIDNT
#include "fatmen/fmnkeys.inc"
      DIMENSION     KEYS(LKEYFA)
#include "fatmen/fatpara.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatopts.inc"
 
#include "fatmen/fatchar.inc"
 
      IF(IOPTS.NE.0) THEN
         CHIDNT = 'S_'
      ELSEIF(IOPTD.NE.0) THEN
         CHIDNT = 'D_'
      ENDIF
 
      IRC = 0
      IF(LBANK.EQ.0) THEN
         CALL FMGETK(GENAME,LBANK,KEYS,IRC)
         IF(IRC.NE.0) THEN
            IF(IDEBFA.GE.-3)
     +      PRINT *,'FMBTOA. Return code ',IRC,' from FMGETK'
            IRC = 1
            RETURN
         ENDIF
      ENDIF
 
      LGN = LENOCC(GENAME)
 
      CHLINE = ' '
      WRITE(CHLINE,9001) CHIDNT,'GENAME=',GENAME(1:LGN)
 9001 FORMAT(A2,A7,A)
      WRITE(LUFZFA,9004) CHLINE(1:LENOCC(CHLINE))
 
      DO 10 I=1,35
         IF(IBTYPE(I).EQ.2) THEN
*
*     Character fields
*
            CHTEXT = ' '
            CALL UHTOC(IQ(LBANK+KOFUFA+IBOFFS(I)),4,CHTEXT,IBLENG(I))
            LTEXT = LENOCC(CHTEXT)
            IF(LTEXT.GT.0) THEN
               CHLINE = ' '
               WRITE(CHLINE,9002) CHIDNT,CHBANK(I),'=',CHTEXT(1:LTEXT)
 9002          FORMAT(A2,A6,A1,A)
               WRITE(LUFZFA,9004) CHLINE(1:LENOCC(CHLINE))
            ENDIF
 
         ELSE
*
*     Integer fields
*
            WRITE(CHLINE,9003) CHIDNT,CHBANK(I),
     +         (IQ(LBANK+KOFUFA+IBOFFS(I)-1+J),J=1,IBLENG(I))
 9003       FORMAT(A2,A6,'=',10(I10,','))
            CHLINE = SPACES(CHLINE,0)
            LLINE  = LENOCC(CHLINE) - 1
            WRITE(LUFZFA,9004) CHLINE(1:LLINE)
 
        ENDIF
 
   10 CONTINUE
 
 9004 FORMAT(A)
 
      END
