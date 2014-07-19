*
* $Id: fmatob.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmatob.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMATOB(GENAME,LBANK,CHLINE,IRC)
 
      CHARACTER*(*) GENAME,CHLINE
      CHARACTER*6   CHTYPE
#include "fatmen/fatbank.inc"
#include "fatmen/slate.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatchar.inc"
 
      IRC = 0
 
      LLINE  = LENOCC(CHLINE)
      CHTYPE = CHLINE(3:8)
 
      IF(IDEBFA.GE.3) PRINT *,'FMATOB. enter for ',CHLINE(1:LLINE),
     +   ' type = ',CHTYPE
*
*     Generic name
*
      IF(CHTYPE.EQ.'GENAME') THEN
         GENAME = CHLINE(10:)
         RETURN
      ENDIF
*
*     Find field
*
      DO 10 I=1,35
         IF(CHTYPE.NE.CHBANK(I)) GOTO 10
         ITYPE = IBTYPE(I)
         ILENG = IBLENG(I)
 
         IF(IBTYPE(I).EQ.2) THEN
*
*     Character fields
*
            CALL UCTOH(CHLINE(10:),IQ(LBANK+KOFUFA+IBOFFS(I)),
     +                 4,LLINE-10+1)
 
         ELSE
*
*     Integer fields
*
            JL = 10
            JR = LLINE
            DO 20 J=1,IBLENG(I)
               IQ(LBANK+KOFUFA+IBOFFS(I)+J-1) = ICDECI(CHLINE,JL,JR)
               JL = JL + IS(1) + 1
20          CONTINUE
         ENDIF
10    CONTINUE
 
      END
