*
* $Id: fmkmtp.F,v 1.1.1.1 1996/03/07 15:17:43 mclareni Exp $
*
* $Log: fmkmtp.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:43  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMKMTP
*CMZ :          30/04/91  16.52.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   30/04/91
#include "fatmen/fatsys.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatbug.inc"
#include "fatmen/fatmtp.inc"
      DIMENSION MYMTP(KMXMTP)
      CHARACTER*80 CHMTP,WORD
#include "fatmen/fatinit.inc"
 
      CALL KUGETC(CHMTP,LMTP)
 
      IF(LMTP.EQ.0) RETURN
*
      IF(CHMTP(1:1).EQ.'-') THEN
         NUMMTP = 0
         DO 1 I=1,KMXMTP
            MFMMTP(I) = -1
1        CONTINUE
         IF(IDEBFA.GE.0) PRINT *,'FMKMTP. media types now cleared'
 
         RETURN
      ENDIF
*
*     Get number of elements in CHMTP
*
      CALL FMNWRD(',',CHMTP(1:LMTP),NWORDS)
 
      NMTP  = 0
      DO 10 I=0,NWORDS-1
         WORD = ' '
         CALL FMWORD(WORD,I,',',CHMTP(1:LMTP),IRC)
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
               IF(IDEBFA.GE.0) PRINT *,'FMKMTP. element ',I+1, ' of ',
     +         CHMTP(1:LMTP),' is not numeric - ignored'
               GOTO 10
            ENDIF
 
            NMTP = NMTP + 1
            IF(NMTP.GT.KMXMTP) THEN
               PRINT *,'FMKMTP. maximum number of media types ',
     +                 'reached - excess values discarded'
               GOTO 30
            ENDIF
            MYMTP(NMTP) = ICDECI(WORD,1,LWORD)
            IF(IDEBFA.GE.3) PRINT *,'FMKMTP. media type # ',NMTP,
     +         ' = ',MYMTP(NMTP)
 
         ELSE
            JLOW = ICDECI(WORD,1,IMINUS-1)
            JHIGH = ICDECI(WORD,IMINUS+1,LWORD)
            DO 20 J=JLOW,JHIGH
               NMTP = NMTP + 1
            IF(NMTP.GT.KMXMTP) THEN
               PRINT *,'FMKMTP. maximum number of media types ',
     +                 'reached - excess values discarded'
               GOTO 30
            ENDIF
               MYMTP(NMTP) = J
               IF(IDEBFA.GE.3) PRINT *,'FMKMTP. media type # ',NMTP,
     +            ' = ',MYMTP(NMTP)
   20       CONTINUE
         ENDIF
   10 CONTINUE
   30 CONTINUE
 
      NUMMTP = MIN(NMTP,KMXMTP)
 
      CALL UCOPY(MYMTP,MFMMTP,NUMMTP)
 
      END
