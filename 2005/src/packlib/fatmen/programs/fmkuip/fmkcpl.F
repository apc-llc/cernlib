*
* $Id: fmkcpl.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmkcpl.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKCPL
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatcpl.inc"
      DIMENSION MYCPL(KMXCPL)
      CHARACTER*80 CHCPL,WORD
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(CHCPL,LCPL)
 
      IF(LCPL.EQ.0) RETURN
*
      IF(CHCPL(1:1).EQ.'-') THEN
         NUMCPL = 0
         DO 1 I=1,KMXCPL
            MFMCPL(I) = -1
1        CONTINUE
         IF(IDEBFA.GE.0) PRINT *,'FMKCPL. copy levels now cleared'
 
         RETURN
      ENDIF
*
*     Get number of elements in CHCPL
*
      CALL FMNWRD(',',CHCPL(1:LCPL),NWORDS)
 
      NCPL  = 0
      DO 10 I=0,NWORDS-1
         WORD = ' '
         CALL FMWORD(WORD,I,',',CHCPL(1:LCPL),IRC)
         LWORD = LENOCC(WORD)
*        PRINT *,'Word ',I,WORD(1:LWORD)
*
*     Range?
*
         IMINUS = INDEX(WORD(1:LWORD),'-')
         IF(IMINUS.EQ.0) THEN
*
*     Is it numeric?
*
            IF(ICNUM(WORD,1,LWORD).LE.LWORD) THEN
               IF(IDEBFA.GE.0) PRINT *,'FMKCPL. element ',I+1, ' of ',
     +         CHCPL(1:LCPL),' is not numeric - ignored'
               GOTO 10
            ENDIF
 
            NCPL = NCPL + 1
            IF(NCPL.GT.KMXCPL) THEN
               PRINT *,'FMKCPL. maximum number of copy levels ',
     +                 'reached - excess values discarded'
               GOTO 30
            ENDIF
            MYCPL(NCPL) = ICDECI(WORD,1,LWORD)
            IF(IDEBFA.GE.3) PRINT *,'FMKCPL. copy level # ',NCPL,
     +         ' = ',MYCPL(NCPL)
 
         ELSE
            JLOW = ICDECI(WORD,1,IMINUS-1)
            JHIGH = ICDECI(WORD,IMINUS+1,LWORD)
            DO 20 J=JLOW,JHIGH
               NCPL = NCPL + 1
            IF(NCPL.GT.KMXCPL) THEN
               PRINT *,'FMKCPL. maximum number of copy levels ',
     +                 'reached - excess values discarded'
               GOTO 30
            ENDIF
               MYCPL(NCPL) = J
               IF(IDEBFA.GE.3) PRINT *,'FMKCPL. copy level # ',NCPL,
     +            ' = ',MYCPL(NCPL)
   20       CONTINUE
         ENDIF
   10 CONTINUE
   30 CONTINUE
 
      NUMCPL = MIN(NCPL,KMXCPL)
 
      CALL UCOPY(MYCPL,MFMCPL,NUMCPL)
 
      END
