*
* $Id: faselp.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: faselp.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FASELP(CHFILE,CHMTCH,IFILE,NFILES,NMATCH,IRC)
*
*     This routine performs wild card file name matching
*     a la VM/CMS (FILELIST/LISTFILE) and VAX/VMS (DIRECTORY)
*     Thus: % corresponds to a single arbitrary character,
*           * corresponds to n (n>=0) arbitrary characters
*     Numeric ranges are also supported, thus (9:12) will
*     match strings 09,10,11,12.
*
#include "fatmen/fatbank.inc"
#include "fatmen/farnge.inc"
*
      COMMON /SLATE/ NDSLAT,NESLAT, DUMMY(38)
      CHARACTER*(*) CHFILE,CHMTCH
      CHARACTER*255 FILE,MATCH
      CHARACTER*10  CHLOW,CHHIGH,CHMAT,CHPASS,CHPLOW,CHPHI
      CHARACTER*5   WILD
      CHARACTER*27  HUNT
      SAVE          CHPLOW,CHPHI,JMATCH
      DATA          WILD/'%*(<>'/
      DATA          HUNT/'ABCDEFGHIJKLMNOPQRSTUVWXYZ/'/
 
*
*     Reset if this is a new call
*
      IF(IFILE.EQ.1) THEN
         CHPLOW = '9999999999'
         CHPHI  = '          '
         JFOUND = 0
         JMATCH = 0
         NMATCH = 0
      ENDIF
 
      ISTART = 0
      JSTART = 0
      IRC    = 0
      IFOUND = 0
      JELEM  = 0
      IEND   = LENOCC(CHMTCH)
      JEND   = LENOCC(CHFILE)
      MATCH  = CHMTCH(1:IEND)
      FILE   = CHFILE(1:JEND)
*
      ISQ    = INDEX(MATCH(1:IEND),'[')
      ILOW   = INDEX(MATCH(1:IEND),'<')
      IHIGH  = INDEX(MATCH(1:IEND),'>')
*
*     Convert [] to ()
*
      CALL CTRANS('[','(',MATCH,1,IEND)
      CALL CTRANS(']',')',MATCH,1,IEND)
      NLPAT  = IQUEST(13) + 1
*
*     Remove multiple *'s
*
      NESLAT = IEND
      CALL CSQMCH(MATCH,'*',1,IEND)
      IF(NESLAT.LT.IEND) IEND   = NESLAT
      IF(IDEBFA.GT.2) PRINT *,'FASELP. Compare ',MATCH(1:IEND),
     +                        ' against ',FILE(1:JEND),
     +                        ' (candidate # ',IFILE,' of ',NFILES,')'
*
*     Process all characters in FILE
*
10    CONTINUE
*
*     Have reached the end of both match and file and all is ok
*
      IF((ISTART.EQ.IEND).AND.(JSTART.EQ.JEND)) GOTO 1
 
      ISTART = ISTART + 1
      JSTART = JSTART + 1
*
*     Have processed all characters in MATCH and all compare
*
      IF(ISTART.GT.IEND)  THEN
*
*     but FILE is longer than MATCH...
*
         IF(ISTART.LE.JEND) IRC = 1
         GOTO 1
      ENDIF
*
*     Look down MATCH for a * or %
*
      IX = ICFMUL(WILD,MATCH,ISTART,IEND)
*
*     Match any characters up until first wild card
*
      IF(IX.GT.ISTART) THEN
        LWORD = IX-ISTART-1
        IF(FILE(JSTART:JSTART+LWORD).NE.MATCH(ISTART:ISTART+LWORD))
     +    GOTO 99
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
         IF(IX.EQ.IEND)       GOTO 1
         DO 20 I=ISTART+1,IEND
         IF((MATCH(I:I).NE.'*').AND.(MATCH(I:I).NE.'%') .AND.
     +      (MATCH(I:I).NE.'(')) GOTO 30
20       CONTINUE
*
*     MATCH only contains * followed by other wild-cards - ok
*
         IF(IDEBFA.GT.2)
     +     PRINT *,'FASELP. Match pattern contains only wild-cards'
         GOTO 1
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
         JFIND = INDEX(FILE(JSTART:JEND),MATCH(I:II))
         IF(JFIND.EQ.0) GOTO 99
         ISTART = I
         JSTART = JSTART+JFIND-1
         GOTO 10
 
         ELSEIF(MATCH(IX:IX).EQ.'%') THEN
*
*     Current character is a <
*
         ELSEIF((MATCH(IX:IX).EQ.'<').OR.(MATCH(IX:IX).EQ.'>')) THEN
*
*     Current character is a < or >
*     Look for a /, an alphabetic character or the end of the string
*
         IF(IDEBFA.GT.2)
     +     PRINT *,'FASELP. Found a < or > character'
         JX = ICFMUL(HUNT,FILE,JSTART,JEND)
         IF(JX.GT.JEND) THEN
            JX = JEND
         ELSE
            JX = JX - 1
         ENDIF
 
         CHPASS = FILE(JSTART:JX)
 
         IF(MATCH(IX:IX).EQ.'<') THEN
 
            IF(LLT(CHPASS,CHPLOW)) THEN
               CHPLOW = CHPASS
               JFOUND = IFILE
               IFOUND = IFILE
*              IF(ISQ.NE.0.AND.JELEM.NE.0)
*    +            IFELEM(JELEM,NLPAT) = IFILE
            ENDIF
 
         ELSE
 
            IF(LGT(CHPASS,CHPHI)) THEN
               CHPHI  = CHPASS
               JFOUND = IFILE
               IFOUND = IFILE
*              IF(ISQ.NE.0.AND.JELEM.NE.0)
*    +            IFELEM(JELEM,NLPAT) = IFILE
            ENDIF
 
         ENDIF
 
         IF(IDEBFA.GT.2)
     +     PRINT 9001,CHPASS, CHPLOW, CHPHI
9001     FORMAT(' FASELP. CHPASS, CHPLOW, CHPHI = ',3A20)
         JSTART = JX
         GOTO 10
*
*     Current character is a %
*
         ELSEIF(MATCH(IX:IX).EQ.'(') THEN
*
*     Found a range delimiter
*
           IF(IDEBFA.GT.2)
     +     PRINT *,'FASELP. range encountered in MATCH pattern'
           IBRA   = INDEX(MATCH(IX:IEND),')')
           ICOL   = INDEX(MATCH(IX:IEND),':')
           IF(IBRA.EQ.0) THEN
              IF(IDEBFA.GE.-3) PRINT *,'FASELP. no trailing delimiter',
     +           ' for range pattern'
              IRC = -4
              RETURN
           ENDIF
 
           IF(IX+IBRA.LT.IEND) THEN
              IF(IDEBFA.GE.4)
     +        PRINT *,'Looking for ',MATCH(IX+IBRA:IX+IBRA),' in ',
     +                FILE(JSTART:JEND)
              LNMAT = INDEX(FILE(JSTART:JEND),
     +                      MATCH(IX+IBRA:IX+IBRA))
              IF(LNMAT.EQ.0) THEN
                 IF(IDEBFA.GE.-3) PRINT *,'FASELP. no match after',
     +              ' range pattern'
                 IRC = -4
                 RETURN
              ENDIF
              LNMAT = LNMAT - 1
           ELSE
              LNMAT = IEND  + 1
           ENDIF
 
           IF(ICOL.EQ.0) THEN
              IF(IDEBFA.GE.-3) PRINT *,'FASELP. missing colon in ',
     +           'range pattern'
              IRC = -4
              RETURN
           ENDIF
 
           CHLOW  = MATCH(IX+1:IX+ICOL-2)
           CHHIGH = MATCH(IX+ICOL:IX+IBRA-2)
           LNHIGH = LENOCC(CHHIGH)
*
*     Extend CHLOW on the left with blanks
*
           CALL CRIGHT(CHLOW,1,LNHIGH)
*
*     And convert to zeroes
*
           CALL CTRANS(' ','0',CHLOW,1,LNHIGH)
           IF(IDEBFA.GT.2)
     +     PRINT *,'FASELP. range = ',CHLOW,' to ',CHHIGH
           CHMAT  = FILE(JSTART:JSTART+LNMAT-1)
*
*     Extend CHMAT on the left with blanks
*
           CALL CRIGHT(CHMAT,1,LNHIGH)
*
*     And convert to zeroes
*
           CALL CTRANS(' ','0',CHMAT,1,LNHIGH)
           IF(ISQ.NE.0) THEN
              IELEM = ICDECI(CHMAT,1,LNHIGH)
              IF(IDEBFA.GE.3) PRINT *,'FASELP. IELEM = ',IELEM
              IF(IELEM.NE.0) THEN
                 JELEM = IUFIND(IELEM,IFRNGE(1,NLPAT),1,
     +                          NFRNGE(NLPAT))
                 IF(JELEM.GT.NFRNGE(NLPAT)) JELEM = 0
              ELSE
                 JELEM = 0
              ENDIF
              IF(IDEBFA.GE.3) PRINT *,'FASELP. JELEM = ',JELEM
           ENDIF
           IF(IDEBFA.GT.2)
     +     PRINT *,'FASELP. match = ',CHMAT
           IF(LLT(CHMAT(1:LNHIGH),CHLOW(1:LNHIGH)) .OR.
     +        LGT(CHMAT(1:LNHIGH),CHHIGH(1:LNHIGH))) GOTO 99
           ISTART = ISTART + IBRA   - 1
*          JSTART = JSTART + LNHIGH - 1
           JSTART = JSTART + LNMAT  - 1
           GOTO 10
 
         ELSE
*
*     Current character is a not a wild card
*
         IF(FILE(JSTART:JSTART).NE.MATCH(ISTART:ISTART)) GOTO 99
 
         ENDIF
 
      GOTO 10
 
99    CONTINUE
      IRC = 2
1     CONTINUE
      IF(IRC.EQ.0) JMATCH = JFOUND
      IF(IFILE.EQ.NFILES) NMATCH = JMATCH
      IF(JELEM.NE.0) THEN
         IVALUE = ICDECI(CHPASS,1,LEN(CHPASS))
         IF(ILOW.NE.0) THEN
            IF(IDEBFA.GE.3) PRINT *,'FASELP. looking for ',
     +         'lowest value. Current value = ',IFVAL(JELEM,NLPAT)
            IF(IVALUE.LT.IFVAL(JELEM,NLPAT)) THEN
               IFVAL(JELEM,NLPAT)  = IVALUE
               IFELEM(JELEM,NLPAT) = IFILE
            ENDIF
         ELSEIF(IHIGH.NE.0) THEN
            IF(IDEBFA.GE.3) PRINT *,'FASELP. looking for ',
     +         'highest value. Current value = ',IFVAL(JELEM,NLPAT)
            IF(IVALUE.GT.IFVAL(JELEM,NLPAT)) THEN
               IFVAL(JELEM,NLPAT)  = IVALUE
               IFELEM(JELEM,NLPAT) = IFILE
            ENDIF
         ENDIF
      ENDIF
      IF(IDEBFA.GE.2) PRINT *,'FASELP. best candidate so far = ',
     +   JMATCH,' return code from this pass = ',IRC
      END
 
