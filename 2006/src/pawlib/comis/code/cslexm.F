*
* $Id: cslexm.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: cslexm.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 10/01/95  09.36.06  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   09/12/93
      SUBROUTINE CSLEXM(LINE, IPOS, TOKEN, LTOKEN)
*-------------------------------------------------
*  fortran lexer: tries pick up next lexem from line(ipos:)
*  after call ipos point out to first lexem character
*             ltoken is length of lexem
*  ltoken=0 if no more lexem and ltoken=-1 if any error detected
*-------------------------------------------------
      CHARACTER *(*) LINE
      INTEGER TOKEN
      CHARACTER *(*) STR1, STR2, CH*1
      PARAMETER ( STR1='.AND.OR.EQ.NE.LE.LT.GE.GT.',
     +            STR2='.NOT.TRUE.FALSE.' )
****************************************************************
* token values:
      INTEGER UNDEF,  IDENT,  INUM,  RNUM, DNUM, HOLL
     +       ,DOTDOT, CCONST, DELIM, NOMORE
      PARAMETER (UNDEF=0, IDENT=1, INUM=2, RNUM=3, DNUM=4, HOLL=5,
     +           DOTDOT=6,CCONST=7,DELIM=8,NOMORE=9 )
*****************************************************************
      LOGICAL CSALPHCH,CSANUMCH,CSDIGCH
      LPOS=LEN(LINE)
* skip blanks
      DO 1 IPOS=IPOS,LPOS
        CH=LINE(IPOS:IPOS)
        IF(CH.NE.' ')GOTO 2
 1    CONTINUE
      LTOKEN=0
      TOKEN=NOMORE
      RETURN
 2    LTOKEN=1
      IF(CSALPHCH(CH))THEN
*  try ident
        DO 3 I=IPOS+1,LPOS
          IF(.NOT.CSANUMCH(LINE(I:I)))GOTO 4
          LTOKEN=LTOKEN+1
 3      CONTINUE
 4      TOKEN=IDENT
      ELSEIF(CSDIGCH(CH))THEN
*  try number
        I=IPOS
        TOKEN=INUM
 5      I=I+1
        IF(I.GT.LPOS) GOTO 8
        CH=LINE(I:I)
        IF(CSDIGCH(CH)) GOTO 5
        IF(CH.EQ.'.')THEN
          I=I+1
          TOKEN=RNUM
          IF(I.GT.LPOS) GOTO 8
          L=LPOS-I+2
          IF(L.GE.4)THEN
            IF(INDEX(STR1,LINE(I-1:I+2)).NE.0)THEN
              I=I-1
              TOKEN=INUM
              GO TO 8
            ENDIF
          ENDIF
          CH=LINE(I:I)
 6        IF(CSDIGCH(CH))THEN
            I=I+1
            IF(I.GT.LPOS)GOTO 8
            CH=LINE(I:I)
            GO TO 6
          ENDIF
          IF(CH.EQ.'E' .OR. CH.EQ.'D')THEN
            IF(CH.EQ.'D')TOKEN=DNUM
            I=I+1
            IF(I.GT.LPOS)GOTO 99
            CH=LINE(I:I)
            IF(CH.EQ.'-' .OR. CH.EQ.'+')THEN
              I=I+1
              IF(I.GT.LPOS)GOTO 99
              CH=LINE(I:I)
            ENDIF
            IF(.NOT.CSDIGCH(CH))GOTO 99
 7          IF(CSDIGCH(CH))THEN
              I=I+1
              IF(I.GT.LPOS)GOTO 8
              CH=LINE(I:I)
              GO TO 7
            ENDIF
          ENDIF
        ELSEIF(CH.EQ.'H')THEN
*    nHconstant
          READ( LINE(IPOS:I-1), '(I3)' ) NH
          IF(I+NH+1.GT.LPOS)NH=LPOS-I
          TOKEN=HOLL
          I=I+NH+1
        ENDIF
 8      LTOKEN=I-IPOS
      ELSEIF(CH.EQ.'.')THEN
*  can be one of (.TRUE. .FALSE. .NOT. - .GT.) or arith const ( .2e-1 )
*  or just .
        I=IPOS+1
        IF(I.GT.LPOS)THEN
          TOKEN=DELIM
          RETURN
        ENDIF
        CH=LINE(I:I)
        IF( .NOT. CSDIGCH(CH) )THEN
          L=LPOS-I+2
          IF(L.GE.4)THEN
            K=INDEX(STR1,LINE(I-1:I+2))
            IF(K.GT.0)THEN
              I=I+3
              IF(K.EQ.1)THEN
                IF(L.GT.4)THEN
                  IF(LINE(I:I).NE.'.')GOTO 99
                  I=I+1
                ELSE
                  GOTO 99
                ENDIF
              ENDIF
            ELSE
              IF(L.LT.5)THEN
                TOKEN=DELIM
                RETURN
              ENDIF
              K=INDEX(STR2,LINE(I-1:I+3))
              IF(K.GT.0)THEN
                I=I+4
                IF(K.EQ.5)THEN
                  IF(L.GT.5)THEN
                    IF(LINE(I:I).NE.'.')GOTO 99
                    I=I+1
                  ELSE
                    GOTO 99
                  ENDIF
                ELSEIF(K.EQ.10)THEN
                  IF(L.GT.6)THEN
                    I=I+1
                    IF(LINE(I:I).NE.'.')GOTO 99
                    I=I+1
                  ELSE
                    GOTO 99
                  ENDIF
                ENDIF
              ELSE
                TOKEN=DELIM
                RETURN
              ENDIF
            ENDIF
          ELSE
*         ! unknown fortran lexem (.)
            TOKEN=DELIM
            RETURN
          ENDIF
          TOKEN=DOTDOT
          LTOKEN=I-IPOS
        ELSE
          TOKEN=RNUM
 9        IF(CSDIGCH(CH))THEN
            I=I+1
            IF(I.GT.LPOS)GOTO 11
            CH=LINE(I:I)
            GO TO 9
          ENDIF
          IF(CH.EQ.'E' .OR. CH.EQ.'D')THEN
            IF(CH.EQ.'D')TOKEN=DNUM
            I=I+1
            IF(I.GT.LPOS)GOTO 99
            CH=LINE(I:I)
            IF(CH.EQ.'-' .OR. CH.EQ.'+')THEN
              I=I+1
              IF(I.GT.LPOS)GOTO 99
              CH=LINE(I:I)
            ENDIF
            IF(.NOT.CSDIGCH(CH))GOTO 99
 10         IF(CSDIGCH(CH))THEN
              I=I+1
              IF(I.GT.LPOS)GOTO 11
              CH=LINE(I:I)
              GO TO 10
            ENDIF
          ENDIF
 11       LTOKEN=I-IPOS
        ENDIF
      ELSEIF(CH.EQ.'''')THEN
*  try char constant
        I=IPOS
 12     I=I+1
        IF(I.GT.LPOS)GO TO 99
        IF(LINE(I:I).NE.'''')GOTO 12
        I=I+1
        IF(I.GT.LPOS)GO TO 13
        IF(LINE(I:I).EQ.'''')GOTO 12
 13     LTOKEN=I-IPOS
        TOKEN=CCONST
      ELSE
*  just say separator
        TOKEN=DELIM
      ENDIF
      RETURN
 99   LTOKEN=-1
*   ! syntax error
      TOKEN=UNDEF
      END
