*
* $Id: kilexp.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kilexp.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  1.68/00 05/12/91  19.03.06  by  Fons Rademakers
*-- Author :
      SUBROUTINE KILEXP(STRING,SEPAR,QUOT1,QUOT2,MAXTOK,TOKVEC,NTOK,
     +                  CHOPT)
*
********************************************************************************
*
* Lexical analyser for parameter string.
* It accepts parameters (tokens) separeted by separators.
* If a separator is between characters QUOTE1 and QUOTE2 it is not considered
* (the order is important, i.e. first QUOTE1 then parameter and last QUOTE2);
* in this case a parameter can also start with an equal sign
* (ex.: If QUOTE1=QUOTE2=' then ='abc' is a valid parameter).
* If blank line then NTOK=0; if too many tokens or syntax error then NTOK=-1
* If two QUOTE1 characters are found consecutively, just one occurrence is kept
* (to be used to escape the QUOTE1 character).
*
*     Input/Output :
*
      CHARACTER*(*) STRING
*
*     Input :
*
      CHARACTER*1   SEPAR
      CHARACTER*1   QUOT1
      CHARACTER*1   QUOT2
      INTEGER MAXTOK
*
*     Output :
*
      INTEGER TOKVEC(2,MAXTOK)
      INTEGER NTOK
*
*     Input :
*
      CHARACTER*1   CHOPT
*
********************************************************************************
*
*
*                        +--------------------+
*                  separ |                    |
*   +--------------------|   POST-QUOTED PAR  |<-----------------+
*   |              //    |                    |                  |
*   |                    +--------------------+                  |
*   |                   EOL |      | other                       |
*   |                       |      |                             |
*   |           +-----------+      |                             |
*   |           |                  |                             |
*   |           v                  v                             |
*   |      +--------+         +---------+                        |
*   |      |        |         |         |                        |
*   | +--->|  STOP  |         |  ERROR  |                        |
*   | |    |        |         |         |                        |
*   | |    +--------+         +---------+                        |
*   | | EOL                                                      |
*   | | +---+                                                    |
*   | | |   | separ                                              | quote2
*   v | v   |                                                    |
* +-----------+          +--------------------+          +------------------+
* |           |  quote1  |                    | any      |                  |
* |   START   |--------->|   PRE-QUOTED PAR   |--------->|    QUOTED PAR    |
* |           | =quote1  |                    |          |                  |
* +-----------+          +--------------------+          +------------------+
*    ^     | |                      ^                          ^    ^    |
*    | other | //                   |                          |    |    | other
*    |     | |                      |                          |    +----+
*    |     | |                      |                          |
*    |     | |                      |                          |
*    |     | |                      |                          |
*    |     | |                      |                          +---------------+
*    |     | |                      |                                          |
*    |     | |                      |                                          |
*    |     | |                      |             quote1 +------------------+  |
*    |     | |                      +--------------------|                  |  |
*    |     | |                                           |   CONCATENATION  |  |
*    |     | +------------------------------------------>|                  |  |
*    |     |                                             +------------------+  |
*    |     |                                                      ^    |       |
*    |     |                                                   // |    | other |
*    |     |                                                      |    |       |
*    |     |                                                      |    |       |
*    |     |                                                      |    |       |
*    |     |                                                      |    |       |
*    |     |                                                      |    |       |
*    |     |                                                      |    |       |
*    |     |                                                      |    |       |
*    |     |   +--------+                                         |    |       |
*    |     |   |        |                                         |    |       |
*    |     |   |  STOP  |                                         |    |   +---+
*    |     |   |        |                                         |    |   |
*    |     |   +--------+                                         |    |   |
*    |     |        ^                                             |    |   |
*    |     |        |                                             |    v   |
*    |     |        |                                EOL +------------------+
*    |     |        +------------------------------------|                  |
*    |     +-------------------------------------------->|  NON-QUOTED PAR  |
*    |                                                   |                  |
*    |                                                   +------------------+
*    |                                                       |      ^    |
*    |                                                 separ |      |    | other
*    +-------------------------------------------------------+      +----+
*
*
********************************************************************************
*
#include "kuip/kcmcmd.inc"
*
      CHARACTER*(MAXCMD) CHTEMP
      INTEGER TOKFLG(100)
      CHARACTER*1 QUOTE1
      CHARACTER*1 QUOTE2
*
      IF (QUOT1.EQ.' '.OR.QUOT2.EQ.' ') THEN
*
* Disable quote logic if equal to blank
*
        QUOTE1=CHAR(2)
        QUOTE2=CHAR(2)
      ELSE
        QUOTE1=QUOT1
        QUOTE2=QUOT2
      ENDIF
*
      NTOK=0
      L=LENOCC(STRING)
      IF (L.EQ.0) GO TO 999
      I=0
*
* Encode QUOTE1 as CTRL/A
*
      III=1
5     CONTINUE
      IF (III.GT.L-1) GO TO 10
*
* The character '@' is now used to escape the QUOTE1 separator
*
        IF (STRING(III:III+1).EQ.'@'//QUOTE1) THEN
          STRING(III:)=CHAR(1)//STRING(III+2:)
          STRING(L:L)=' '
          L=L-1
        ENDIF
      III=III+1
      GO TO 5
*
10    CONTINUE
*
* Start
*
      I=I+1
11    IF (I.GT.L) GO TO 999
      IF (STRING(I:I).EQ.SEPAR) GO TO 10
      IF (STRING(I:I).EQ.QUOTE1.OR.(CHOPT.NE.' '.AND.
     +   (STRING(I:I).EQ.CHOPT.AND.STRING(I+1:I+1).EQ.QUOTE1))) THEN
        NTOK=NTOK+1
        IF (NTOK.GT.MAXTOK) GO TO 99
        TOKVEC(1,NTOK)=I
        TOKFLG(NTOK)=0
        GO TO 30
      ENDIF
      NTOK=NTOK+1
      IF (NTOK.GT.MAXTOK) GO TO 99
      TOKVEC(1,NTOK)=I
      TOKFLG(NTOK)=0
      GO TO 20

20    CONTINUE
*
* Non-quoted parameter
*
      I=I+1
      IF (I.GT.L) THEN
        TOKVEC(2,NTOK)=I-1
        GO TO 999
      ENDIF
      IF (STRING(I:I).EQ.QUOTE1) GO TO 40
      IF (STRING(I:I).EQ.SEPAR.OR.STRING(I:I+1).EQ.'//') THEN
        TOKVEC(2,NTOK)=I-1
        IF (STRING(I:I+1).EQ.'//') THEN
          TOKFLG(NTOK)=-1
          I=I+1
        ENDIF
        GO TO 10
      ENDIF
      GO TO 20

30    CONTINUE
*
* Pre-quoted parameter
*

40    CONTINUE
*
* Quoted parameter
*
      I=I+1
      IF (I.GT.L) GO TO 90
      IF (STRING(I:I).EQ.QUOTE2) GO TO 50
      GO TO 40

50    CONTINUE
*
* Post-quoted parameter
*
* Null string are not allowed
*
      IF (I.EQ.TOKVEC(1,NTOK)+1) GO TO 90
      IF (I.EQ.L) THEN
        TOKVEC(2,NTOK)=I
        GO TO 999
      ENDIF
      IF (STRING(I:I).EQ.SEPAR.OR.STRING(I:I+1).EQ.'//') THEN
        TOKVEC(2,NTOK)=I-1
        IF (STRING(I:I+1).EQ.'//') THEN
          TOKFLG(NTOK)=-2
          I=I+1
        ENDIF
        GO TO 10
      ENDIF
      IF (STRING(I:I+1).EQ.'//') GO TO 11
      IF (STRING(I+1:I+1).EQ.QUOTE1) GO TO 90
      GO TO 20

90    CONTINUE
      CALL KUALFA
      PRINT *,'*** Syntax error in token #',NTOK,' of line: ',
     +        STRING(1:LENOCC(STRING))
      NTOK=0
      GO TO 999

99    CONTINUE
      CALL KUALFA
      PRINT *,'*** Too many tokens (MAX=',MAXTOK,') in line: ',
     +        STRING(1:LENOCC(STRING))
      NTOK=0
      GO TO 999

999   CONTINUE
*
* Decode QUOTE1 from CTRL/A
*
      DO 100 III=1,L
        IF (STRING(III:III).EQ.CHAR(1)) STRING(III:III)=QUOTE1
100   CONTINUE
*
* Merge tokens to be concatenated
*
      IF (NTOK.LE.0) GO TO 9999

      DO 110 I=1,NTOK
        IF (STRING(TOKVEC(1,I):TOKVEC(1,I)).EQ.QUOTE1) THEN
          TOKFLG(I)=SIGN(2.,FLOAT(TOKFLG(I)))
        ELSE
          TOKFLG(I)=SIGN(1.,FLOAT(TOKFLG(I)))
        ENDIF
110   CONTINUE

120   CONTINUE
      II=NTOK-1
      DO 200 I=1,II
        IF (TOKFLG(I).LT.0) THEN
          IF (IABS(TOKFLG(I)).EQ.1.AND.IABS(TOKFLG(I+1)).EQ.1) THEN
            CHTEMP=STRING(TOKVEC(1,I+1):)
            STRING(TOKVEC(2,I)+1:)=CHTEMP
            NCH=2
            TOKFLG(I)=TOKFLG(I+1)
            TOKVEC(2,I)=TOKVEC(2,I+1)-NCH
          ELSE IF (IABS(TOKFLG(I)).EQ.2.AND.IABS(TOKFLG(I+1)).EQ.2) THEN
            CHTEMP=STRING(TOKVEC(1,I+1)+1:)
            STRING(TOKVEC(2,I):)=CHTEMP
            NCH=4
            TOKFLG(I)=TOKFLG(I+1)
            TOKVEC(2,I)=TOKVEC(2,I+1)-NCH
          ELSE IF (IABS(TOKFLG(I)).EQ.1.AND.IABS(TOKFLG(I+1)).EQ.2) THEN
            CHTEMP=STRING(TOKVEC(1,I+1)+1:)
            STRING(TOKVEC(2,I)+1:)=CHTEMP
            NCH=3
            TOKFLG(I)=SIGN(2.,FLOAT(TOKFLG(I+1)))
            TOKVEC(2,I)=TOKVEC(2,I+1)-NCH
*
* Case of CHOPT equal to '='
*
            IF (CHOPT.EQ.'=') THEN
              IEQ=INDEX(STRING(TOKVEC(1,I):TOKVEC(2,I)),CHOPT)
              IF (IEQ.GT.0) THEN
                IEQ=TOKVEC(1,I)+IEQ-1
                CHTEMP=STRING(IEQ+1:)
                STRING(IEQ+1:)=QUOTE1//CHTEMP
                GO TO 105
              ENDIF
            ENDIF
*
            CHTEMP=STRING(TOKVEC(1,I):)
            STRING(TOKVEC(1,I):)=QUOTE1//CHTEMP
105         CONTINUE
            TOKVEC(2,I)=TOKVEC(2,I)+1
            DO 130 J=I+1,NTOK
              TOKVEC(1,J)=TOKVEC(1,J)+1
              TOKVEC(2,J)=TOKVEC(2,J)+1
130         CONTINUE

          ELSE IF (IABS(TOKFLG(I)).EQ.2.AND.IABS(TOKFLG(I+1)).EQ.1) THEN
            CHTEMP=STRING(TOKVEC(1,I+1):)
            STRING(TOKVEC(2,I):)=CHTEMP
            NCH=3
            TOKFLG(I)=SIGN(2.,FLOAT(TOKFLG(I+1)))
            TOKVEC(2,I)=TOKVEC(2,I+1)-NCH

            CHTEMP=STRING(TOKVEC(2,I)+1:)
            STRING(TOKVEC(2,I)+1:)=QUOTE2//CHTEMP
            TOKVEC(2,I)=TOKVEC(2,I)+1
            DO 140 J=I+2,NTOK
              TOKVEC(1,J)=TOKVEC(1,J)+1
              TOKVEC(2,J)=TOKVEC(2,J)+1
140         CONTINUE

          ENDIF
          DO 150 J=I+1,NTOK-1
            TOKFLG(J)=TOKFLG(J+1)
150       CONTINUE
          DO 160 J=I+1,NTOK-1
            TOKVEC(1,J)=TOKVEC(1,J+1)-NCH
            TOKVEC(2,J)=TOKVEC(2,J+1)-NCH
160       CONTINUE
          NTOK=NTOK-1
          GO TO 120
        ENDIF
200   CONTINUE

9999  END



