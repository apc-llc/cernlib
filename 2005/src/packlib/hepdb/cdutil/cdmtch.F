*
* $Id: cdmtch.F,v 1.1.1.1 1996/02/28 16:24:37 mclareni Exp $
*
* $Log: cdmtch.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:37  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDMTCH(CHFILE,CHMTCH,IRC)
#include "hepdb/cdunit.inc"
#include "hepdb/quest.inc"
*
*     This routine performs wild card file name matching
*     a la VM/CMS (FILELIST/LISTFILE) and VAX/VMS (DIRECTORY)
*     Thus: % corresponds to a single arbitrary character,
*           * corresponds to n (n>=0) arbitrary characters
*     Numeric ranges are also supported, thus (9:12) will
*     match strings 09,10,11,12.
*
*     IRC = 1 - trailing unmatched characters in MATCH
*               IQUEST(1)-IQUEST(2) first/last unmatched characters
*           2 - trailing unmatched characters in FILE
*               IQUEST(1)-IQUEST(2) first/last unmatched characters
*           3 - substring mismatch
*               IQUEST(1)-IQUEST(2) first/last characters of substring in FILE
*               IQUEST(3)-IQUEST(4) first/last characters of substring in MATCH
*           4 - cannot find character
*               IQUEST(1)-IQUEST(2) character in MATCH
*               IQUEST(3)-IQUEST(4) substring of FILE being searched
*           5 - range mismatch
*           6 - character mismatch
*               IQUEST(1) - character in FILE
*               IQUEST(2) - character in MATCH
*
      COMMON /SLATE/ NDSLAT,NESLAT, DUMMY(38)
      CHARACTER*(*) CHFILE,CHMTCH
      CHARACTER*255 FILE,MATCH
      CHARACTER*10  CHLOW,CHHIGH,CHMAT
      CHARACTER*3   WILD
      DATA          WILD/'%*('/
 
      ILOW   = 0
      IHIGH  = 0
      ISTART = 0
      JSTART = 0
      IRC    = 0
      MATCH  = ' '
      FILE   = ' '
      IEND   = LENOCC(CHMTCH)
      JEND   = LENOCC(CHFILE)
      MATCH  = CHMTCH(1:IEND)
      FILE   = CHFILE(1:JEND)
*
*     Convert <> characters to *
*
      IF(ICFMUL('<>',MATCH,1,IEND).LE.IEND) THEN
         IF(LLOGCD.GE.3) PRINT *,
     +      'CDMTCH. translating <> characters to *'
         CALL CTRANS('<','*',MATCH,1,IEND)
         CALL CTRANS('>','*',MATCH,1,IEND)
      ENDIF
*
*     Convert [] to ()
*
      CALL CTRANS('[','(',MATCH,1,IEND)
      CALL CTRANS(']',')',MATCH,1,IEND)
*
*     Remove multiple *'s
*
      NESLAT = IEND
      CALL CSQMCH(MATCH,'*',1,IEND)
      IF(NESLAT.LT.IEND) IEND   = NESLAT
      IF(LLOGCD.GE.3) PRINT *,'CDMTCH. Compare ',MATCH(1:IEND),
     +                        ' against ',FILE(1:JEND)
 
      IF(MATCH(1:IEND).EQ.FILE(1:JEND)) RETURN
*
*     Process all characters in FILE
*
10    CONTINUE
*
*     Have reached the end of both match and file and all is ok
*
      IF((ISTART.EQ.IEND).AND.(JSTART.EQ.JEND)) RETURN
      ISTART = ISTART + 1
      JSTART = JSTART + 1
*
*     Have processed all characters in FILE...
*
      IF(JSTART.GT.JEND)  THEN
*
*     but there are unmatched characters in MATCH...
*
         IF(ISTART.LE.IEND) THEN
            IF(LLOGCD.GE.3) PRINT *,'CDMTCH. MATCH contains ',
     +         'trailing unmatched characters - ',MATCH(ISTART:IEND)
            IRC = 1
            IQUEST(1) = ISTART
            IQUEST(2) = IEND
            GOTO 99
         ENDIF
      ENDIF
*
*     Have processed all characters in MATCH and all compare
*
      IF(ISTART.GT.IEND)  THEN
*
*     but FILE is longer than MATCH...
*
         IF(ISTART.LE.JEND) THEN
            IF(LLOGCD.GE.3) PRINT *,'CDMTCH. FILE contains ',
     +         'trailing unmatched characters - ',FILE(JSTART:JEND)
            IRC = 2
            IQUEST(1) = JSTART
            IQUEST(2) = JEND
            GOTO 99
         ENDIF
      ENDIF
*
*     Look down MATCH for a * or %
*
      IX = ICFMUL(WILD,MATCH,ISTART,IEND)
      IF(IX.GT.IEND) IX = 0
*
*     Match any characters up until first wild card
*
      IF(IX.GT.ISTART) THEN
         LWORD = IX - ISTART - 1
         IF(FILE(JSTART:JSTART+LWORD).NE.
     +      MATCH(ISTART:ISTART+LWORD)) THEN
            IF(LLOGCD.GE.4) PRINT *,'CDMTCH. substring mismatch ',
     +         FILE(JSTART:JSTART+LWORD),' ',
     +         MATCH(ISTART:ISTART+LWORD)
            IRC = 3
            IQUEST(1) = JSTART
            IQUEST(2) = JSTART + LWORD
            IQUEST(3) = ISTART
            IQUEST(4) = ISTART + LWORD
            GOTO 99
        ENDIF
        ISTART = ISTART + LWORD
        JSTART = JSTART + LWORD
        GOTO 10
      ENDIF
 
         IF(MATCH(IX:IX).EQ.'*') THEN
*
*     Current character is a *
*     Find first non-wild character in MATCH and find
*     in FILE.
*
         IF(IX.EQ.IEND)       RETURN
         DO 20 I=ISTART+1,IEND
         IF((MATCH(I:I).NE.'*').AND.(MATCH(I:I).NE.'%') .AND.
     +      (MATCH(I:I).NE.'(')) GOTO 30
20       CONTINUE
*
*     MATCH only contains * followed by other wild-cards - ok
*
         IF(LLOGCD.GT.3)
     +     PRINT *,'CDMTCH. Match pattern contains only wild-cards'
         RETURN
30       CONTINUE
*
*     Look for current word in FILE and reset JSTART
*     Word is delimited by two wild-chars or end of string.
*
         IWILD = ICFMUL(WILD,MATCH,I,IEND)
         IF(IWILD.LE.IEND) THEN
            II = IWILD - 1
         ELSE
            II = IEND
         ENDIF
*        II = MIN(IEND,ICFMUL(WILD,MATCH,I,IEND))
*        JFIND = INDEX(FILE(JSTART:JEND),MATCH(I:I))
         JFIND = INDEX(FILE(JSTART:JEND),MATCH(I:II))
         IF(JFIND.EQ.0) THEN
*          IF(LLOGCD.GE.4) PRINT *,'CDMTCH. cannot find ',MATCH(I:I),
           IF(LLOGCD.GE.4) PRINT *,'CDMTCH. cannot find ',MATCH(I:II),
     +        ' in ',FILE(JSTART:JEND)
            IRC = 4
            IQUEST(1) = I
            IQUEST(2) = II
            IQUEST(3) = JSTART
            IQUEST(4) = JEND
            GOTO 99
         ENDIF
 
         ISTART = I
         JSTART = JSTART+JFIND-1
         GOTO 10
 
         ELSEIF(MATCH(IX:IX).EQ.'%') THEN
*
*     Current character is a %
*
         ELSEIF(MATCH(IX:IX).EQ.'(') THEN
*
*     Found a range delimiter
*
           IF(LLOGCD.GT.3)
     +     PRINT *,'CDMTCH. range encountered in MATCH pattern'
           IBRA   = INDEX(MATCH(IX:IEND),')')
           IF(IBRA.EQ.0) THEN
              IF(LLOGCD.GE.-3) PRINT *,'CDMTCH. no trailing delimiter',
     +           ' for range pattern'
              IRC = -4
              RETURN
           ENDIF
 
           ICOL   = INDEX(MATCH(IX:IEND),':')
           IF(ICOL.EQ.0) THEN
              IF(LLOGCD.GE.-3) PRINT *,'CDMTCH. missing colon in ',
     +           'range pattern'
              IRC = -4
              RETURN
           ENDIF
 
           CHLOW  = MATCH(IX+1:IX+ICOL-2)
           CHHIGH = MATCH(IX+ICOL:IX+IBRA-2)
           LNHIGH = LENOCC(CHHIGH)
 
           IF(IX+IBRA.LT.IEND) THEN
              IF(LLOGCD.GE.4)
     +        PRINT *,'Looking for ',MATCH(IX+IBRA:IX+IBRA),' in ',
     +                FILE(JSTART:JEND)
              LNMAT = INDEX(FILE(JSTART:JEND),
     +                      MATCH(IX+IBRA:IX+IBRA))
              IF(LNMAT.EQ.0) THEN
                 IF(LLOGCD.GE.3) PRINT *,'CDMTCH. no match after',
     +              ' range pattern'
                 IRC = -4
                 RETURN
              ENDIF
              LNMAT = LNMAT - 1
           ELSE
              LNMAT = LNHIGH
           ENDIF
*
*     Extend CHLOW on the left with blanks
*
           CALL CRIGHT(CHLOW,1,LNHIGH)
*
*     And convert to zeroes
*
           CALL CTRANS(' ','0',CHLOW,1,LNHIGH)
           LNLOW  = LNHIGH
           IF(LLOGCD.GT.3)
     +     PRINT *,'CDMTCH. range = ',CHLOW,' to ',CHHIGH
           CHMAT  = FILE(JSTART:JSTART+LNMAT-1)
*
*     Extend CHMAT on the left with blanks
*
           CALL CRIGHT(CHMAT,1,LNHIGH)
*
*     And convert to zeroes
*
           CALL CTRANS(' ','0',CHMAT,1,LNHIGH)
           IF(LLOGCD.GT.3)
     +     PRINT *,'CDMTCH. match = ',CHMAT
           IF(LLT(CHMAT(1:LNHIGH),CHLOW(1:LNLOW)) .OR.
     +        LGT(CHMAT(1:LNHIGH),CHHIGH(1:LNHIGH))) THEN
              IF(LLOGCD.GE.4) PRINT *,'CDMTCH. range mismatch ',
     +           'CHLOW/CHHIGH/CHMAT = ',CHLOW(1:LNLOW),
     +           ' ',CHHIGH(1:LNHIGH),' ',CHMAT(1:LNHIGH)
              IRC = 5
              GOTO 99
           ENDIF
 
           ISTART = ISTART + IBRA   - 1
*          JSTART = JSTART + LNHIGH - 1
           JSTART = JSTART + LNMAT  - 1
           GOTO 10
 
         ELSE
*
*     Current character is a not a wild card
*
         IF(FILE(JSTART:JSTART).NE.MATCH(ISTART:ISTART)) THEN
            IF(LLOGCD.GT.3) PRINT *,'CDMTCH. mismatch at ',
     +         FILE(JSTART:JSTART),JSTART,MATCH(ISTART:ISTART),ISTART
            IRC = 6
            IQUEST(1) = ISTART
            IQUEST(2) = JSTART
            GOTO 99
         ENDIF
 
         ENDIF
 
      GOTO 10
 
99    CONTINUE
      END
 
