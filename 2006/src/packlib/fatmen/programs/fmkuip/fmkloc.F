*
* $Id: fmkloc.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmkloc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKLOC
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatloc.inc"
      DIMENSION MYLOC(KMXLOC)
      CHARACTER*80 CHLOC,WORD
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(CHLOC,LLOC)
 
      IF(LLOC.EQ.0) RETURN
*
      IF(CHLOC(1:1).EQ.'-') THEN
         NUMLOC = 0
         DO 1 I=1,KMXLOC
            MFMLOC(I) = -1
1        CONTINUE
         IF(IDEBFA.GE.0) PRINT *,'FMKLOC. location codes now cleared'
 
         RETURN
      ENDIF
*
*     Get number of elements in CHLOC
*
      CALL FMNWRD(',',CHLOC(1:LLOC),NWORDS)
 
      NLOC  = 0
      DO 10 I=0,NWORDS-1
         WORD = ' '
         CALL FMWORD(WORD,I,',',CHLOC(1:LLOC),IRC)
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
               IF(IDEBFA.GE.0) PRINT *,'FMKLOC. element ',I+1, ' of ',
     +         CHLOC(1:LLOC),' is not numeric - ignored'
               GOTO 10
            ENDIF
 
            NLOC = NLOC + 1
            IF(NLOC.GT.KMXLOC) THEN
               PRINT *,'FMKLOC. maximum number of location codes ',
     +                 'reached - excess values discarded'
               GOTO 30
            ENDIF
            MYLOC(NLOC) = ICDECI(WORD,1,LWORD)
            IF(IDEBFA.GE.3) PRINT *,'FMKLOC. location code # ',NLOC,
     +         ' = ',MYLOC(NLOC)
 
         ELSE
            JLOW = ICDECI(WORD,1,IMINUS-1)
            JHIGH = ICDECI(WORD,IMINUS+1,LWORD)
            DO 20 J=JLOW,JHIGH
               NLOC = NLOC + 1
            IF(NLOC.GT.KMXLOC) THEN
               PRINT *,'FMKLOC. maximum number of location codes ',
     +                 'reached - excess values discarded'
               GOTO 30
            ENDIF
            MYLOC(NLOC) = ICDECI(WORD,1,LWORD)
 
               MYLOC(NLOC) = J
               IF(IDEBFA.GE.3) PRINT *,'FMKLOC. location code # ',NLOC,
     +            ' = ',MYLOC(NLOC)
   20       CONTINUE
         ENDIF
   10 CONTINUE
   30 CONTINUE
 
      NUMLOC = MIN(NLOC,KMXLOC)
 
      CALL UCOPY(MYLOC,MFMLOC,NUMLOC)
 
      END
