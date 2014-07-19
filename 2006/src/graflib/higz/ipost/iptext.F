*
* $Id: iptext.F,v 1.5 2005/10/28 12:38:14 couet Exp $
*
* $Log: iptext.F,v $
* Revision 1.5  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.3  1996/03/08 16:53:41  couet
* Comment line added after \ at the end of the line
*
* Revision 1.2  1996/02/29 16:30:41  cernlib
* This had \\ at the end of some comments, which on osf disturbed the
*   following line-- was indented by a few blanks -- and the label ended
*   up beyond col. 6
*
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/02 28/08/95  14.31.19  by  O.Couet
*-- Author :    P.Juillot   13/08/92
      SUBROUTINE IPTEXT(XX,YY,CHARS)
*.===========>
*.
*.    This routine writes the a into a PostScript file
*.         according to the IGTEXT control characters.
*.
*. _Input parameters:
*.
*.  REAL XX,YY : Text position in world coordinates .
*.  CHARACTER CHARS : Text string .
*.
*..==========> (P.Juillot)
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
*
* npiece= max number of pieces of text ( separated by escape characters)
      PARAMETER (NPIECE=50)
      DIMENSION IFNB(NPIECE),IFNS(NPIECE),LEVEL(NPIECE),LBACK(NPIECE)

* maximum length of a (PostScript) string

      CHARACTER*(*) CHARS
      CHARACTER*512 NEWTXT
      CHARACTER*512 CHAR2
      CHARACTER*80 PIECE(NPIECE)

* this common is needed for some routines in CHPACK (KERNLIB M432)
      COMMON /SLATE/ND,NE,NF,NG,DUMMY(36)

      CHARACTER*30 PSFONT(42)
      DIMENSION PSRAP(30)
      CHARACTER*3 PSTMP
      CHARACTER*13 CFLIP
      CHARACTER*3  CFLIPC(13)

      LOGICAL UPPER,LOWER,ESCAPE
      LOGICAL ROMAN,GREEK,SPECIA,ZAPF,NORMAL,SUB,SUPER
      LOGICAL OSHOW

      DATA CFLIP /'["?^]#!~&$<>@'/
      DATA CFLIPC /'133','42','77','136','135','43','41',
     +             '176','46','44','74','76','100'/
      DATA PSRAP /1.031,1.000,1.026,0.932,0.931,0.931,0.930,1.204,1.204,
     +            1.168,1.166,1.007,1.026,0.964,
     +            16*1.0/

      DATA PSFONT /'/Times-Italic','/Times-Bold','/Times-BoldItalic'
     +,'/Helvetica','/Helvetica-Oblique','/Helvetica-Bold'
     +,'/Helvetica-BoldOblique','/Courier','/Courier-Oblique'
     +,'/Courier-Bold','/Courier-BoldOblique','/Symbol','/Times-Roman'
     +,'/ZapfDingbats','/Times-Italic','/Times-Bold','/Times-BoldItalic'
     +,'/Helvetica','/Helvetica-Oblique','/Helvetica-Bold'
     +,'/Helvetica-BoldOblique','/Symbol','/Times-Roman','/ZapfDingbats'
     +,'/Special','/ZapfChancery-MediumItalic','/AvantGarde-Book'
     +,'/AvantGarde-BookOblique','/AvantGarde-Demi'
     +,'/AvantGarde-DemiOblique','/Bookman-Demi','/Bookman-DemiItalic'
     +,'/Bookman-Light','/Bookman-LightItalic','/Palatino-Roman'
     +,'/Palatino-Italic','/Palatino-Bold','/Palatino-BoldItalic'
     +,'/NewCenturySchlbk-Roman','/NewCenturySchlbk-Italic'
     +,'/NewCenturySchlbk-Bold','/NewCenturySchlbk-BoldItalic'/
*.______________________________________
*
#include "higz/hiwcps.inc"
*
      X    = XX
      Y    = YY
      NOLD = MIN(LENOCC(CHARS),512)
*
* Text colour and vertical alignment
*
      CALL IPSCOL(ITXCI)
      IF((ITXALV.EQ.2).OR.(ITXALV.EQ.1))THEN
         Y = Y-RCHH*COS((3.14159/180.)*RANGLE)
         X = X+RCHH*SIN((3.14159/180.)*RANGLE)
      ELSEIF(ITXALV.EQ.3)THEN
         Y = Y-(RCHH/2.)*COS((3.14159/180.)*RANGLE)
         X = X+(RCHH/2.)*SIN((3.14159/180.)*RANGLE)
      ENDIF
*
* Compute the fonts size. Exit if it is 0
*
      NFON = ABS(IFONT)
      IF(NFON.GT.42.OR.NFON.LT.1)NFON = 1
      FONRAP = 75./51.
      IF(NFON.LE.14.AND.NFON.GE.1)FONRAP = FONRAP*PSRAP(NFON)
      IS = IYWCPS(RCHH*FONRAP)-IYWCPS(0.)
      IF(IS.LE.0)GOTO 999
      NP = IPREC
*
* The hollow fonts are set on by the roman font number
* The number of passes define the width of the hollow text
*
      OSHOW = .FALSE.
      IF (NFON.GT.14.AND.NFON.LT.25) THEN
         OSHOW = .TRUE.
         CALL IPIOUT(INPASS)
         CALL IPPSTR(' lw')
      ENDIF
*
* Start a first parsing:
*  - looking for the < and > escape characters,
*  - remove them and build a new string changing
*    upper <=> lower characters
*  - manage the '@' escape character
*  - check if the input string is not too long (J<=505)
*
      ESCAPE = .FALSE.
      UPPER  = .FALSE.
      LOWER  = .FALSE.
      NEWTXT = ' '
      PSTMP  = ' '
      J      = 0
      DO 10 I=1,NOLD
         IF(J.GE.505)GOTO 360
         IF(ESCAPE)THEN
            ESCAPE = .FALSE.
            GOTO 10
         ENDIF
         IF(CHARS(I:I).EQ.'@')THEN
            IFLIP=INDEX(CFLIP,CHARS(I+1:I+1))
            IF(IFLIP.NE.0)THEN
               J = J+1
               NEWTXT(J:J) = BSLASH
               J = J+1
               NEWTXT(J:) = CFLIPC(IFLIP)
               J = LENOCC(NEWTXT)
               ESCAPE = .TRUE.
               GOTO 10
            ENDIF
         ENDIF
         IF (CHARS(I:I).EQ.'<') THEN
            LOWER = .TRUE.
            UPPER = .FALSE.
         ELSEIF(CHARS(I:I).EQ.'>') THEN
            LOWER = .FALSE.
            UPPER = .TRUE.
         ELSE
            PSTMP(1:1)=CHARS(I:I)
            IF(LOWER)CALL CUTOL(PSTMP(1:1))
            IF(UPPER)CALL CLTOU(PSTMP(1:1))
            J = J+1
            NEWTXT(J:J) = PSTMP(1:1)
         ENDIF
   10 CONTINUE
      NCHP = LENOCC(NEWTXT)
*
* Now a second parsing to search for the PostScript
* characters (following a \) and ( , ), \
*
      CHAR2 = ' '
* loop on NCHP old characters and look for \
*
      IOLD=0
      INEW=0
   20 IOLD=IOLD+1
      IF(IOLD.EQ.NCHP+1) GO TO 70
*
*              1. find an antislash
*
      IF (NEWTXT(IOLD:IOLD).EQ.BSLASH) THEN
* if this \ is not the last character of the string, then
* study the character following this \
*
         IF(IOLD.EQ.NCHP) GO TO 60
*
*  1.1  the character following this \ is also an \
*
         IF (NEWTXT(IOLD+1:IOLD+1).EQ.BSLASH) THEN
*       copy both \\,
            INEW=INEW+1
            CHAR2(INEW:INEW)=BSLASH
            INEW=INEW+1
            CHAR2(INEW:INEW)=BSLASH
*       and go to the next character
            IOLD=IOLD+1
            GO TO 20
         ENDIF
*
*  1.2  the character following this \ is a parenthesis: ( or )
*
         IF(NEWTXT(IOLD+1:IOLD+1).EQ.'(' .OR.
     +       NEWTXT(IOLD+1:IOLD+1).EQ.')') THEN
*       copy the \ and the parenthesis,
            INEW=INEW+1
            CHAR2(INEW:INEW)=BSLASH
            INEW=INEW+1
            CHAR2(INEW:INEW)=NEWTXT(IOLD+1:IOLD+1)
*       and go to the following character
            IOLD=IOLD+1
            GO TO 20
         ENDIF
*
*  1.3  the character following this \ is also a special
*       PostScript character:
*      \n    linefeed (newline)
*      \r    carriage return
*      \t    horizontal tab
*      \f    form feed
*
         IF(NEWTXT(IOLD+1:IOLD+1).EQ.'n' .OR.
     +      NEWTXT(IOLD+1:IOLD+1).EQ.'r' .OR.
     +      NEWTXT(IOLD+1:IOLD+1).EQ.'t' .OR.
     +      NEWTXT(IOLD+1:IOLD+1).EQ.'f') THEN
*       copy nothing and go to the following character
            IOLD=IOLD+1
            GO TO 20
         ENDIF
*
*  1.4  the character following this \ is the special
*       PostScript character:
*      \b    back space
*
         IF(NEWTXT(IOLD+1:IOLD+1).EQ.'b') THEN
*       replace the sequence \b by the & escape character
            INEW=INEW+1
            CHAR2(INEW:INEW)='&'
*       and forget the b
            IOLD=IOLD+1
            GO TO 20
         ENDIF
*
*  1.5  the character following this \ is a digit between 0 and 7,
*  which means that the input text contains a string like \123 where
*  123 is an octal number the accepted ranges are 40-176 and 241-376
*  ( all others are ASCII control characters )
*
*    =>   first, study the range 40-77 (case of 2 digits after the \)
*
         PSTMP=' '
         DO 40 K=40,77
            WRITE(PSTMP(1:2),'(I2.2)') K
            IADD = 0
            IF(PSTMP(1:2).EQ.NEWTXT(IOLD+1:IOLD+2))GOTO 30
            IF(NEWTXT(IOLD+1:IOLD+1).EQ.'0'.AND.
     +         PSTMP(1:2).EQ.NEWTXT(IOLD+2:IOLD+3))THEN
               IADD = 1
               GOTO 30
            ENDIF
            GOTO 40
*   OK:  copy the \ and the 2 following digits and add a 0
   30       CHAR2(INEW+1:INEW+1) = BSLASH
            CHAR2(INEW+2:INEW+2) = '0'
            CHAR2(INEW+3:INEW+4) = NEWTXT(IOLD+1+IADD:IOLD+2+IADD)
            INEW = INEW+4
*       and go parsing the next following old character
            IOLD=IOLD+2+IADD
            GO TO 20
   40    CONTINUE
*
*    =>   then, study the ranges  100-177 and 241-377
*           (case of 3 digits after the \)
         PSTMP=' '
         DO 50 K=100,377
            IF(K.GE.178 .AND. K.LE.240) GO TO 50
            WRITE(PSTMP(1:3),'(I3.3)') K
            IF(PSTMP(1:3).EQ.NEWTXT(IOLD+1:IOLD+3))THEN
*   OK:  copy the \ and the 3 following digits
               CHAR2(INEW+1:) = NEWTXT(IOLD:IOLD+3)
               INEW = INEW+4
*       and go parsing the next following old character
               IOLD = IOLD+3
               GO TO 20
            ENDIF
   50    CONTINUE
*
*  1.6 this \ is followed by nothing understandable in PostScript,
*   it is an "isolated \" which will appear as \\ (leave one space for cpp)
*       copy two \\ (leave one space for cpp)
*
   60    INEW=INEW+1
         CHAR2(INEW:INEW)=BSLASH
         INEW=INEW+1
         CHAR2(INEW:INEW)=BSLASH
         GO TO 20
*
* 2. find ( or ) not preceeded by an \ : include one \
*
      ELSEIF(NEWTXT(IOLD:IOLD).EQ.'('
     +  .OR. NEWTXT(IOLD:IOLD).EQ.')') THEN
*   2.1 if this (,) is not preceeded by a \, include one \
*
         IF(I.EQ.1 .OR. NEWTXT(IOLD-1:IOLD-1).NE.BSLASH) THEN
            INEW=INEW+1
            CHAR2(INEW:INEW)=BSLASH
            INEW=INEW+1
            CHAR2(INEW:INEW)=NEWTXT(IOLD:IOLD)
            GO TO 20
         ENDIF
         GO TO 20
* 3. treat normal text
      ELSE
         INEW=INEW+1
         CHAR2(INEW:INEW)=NEWTXT(IOLD:IOLD)
      ENDIF
      GO TO 20
   70 CONTINUE

      NCHP=LENOCC(CHAR2)

** now a third parsing to cut the text into pieces
*** for each piece of text, I define
*      a. the string content = PIECE(I)
*      b. the font # = IFNB(I)= NFON: roman, 12: greek ,
*                         14: ZapfdingBats
*      c. the font size = IFNS(I)
*      d. a level flag = LEVEL(I) = 1: normal
*                                   2: superscript
*                                   3: subscript
*      e. a "backward" flag = LBACK(I) = 0: normal text ,
*                                      = 1: superscript and
*                                           subscript start at
*                                           the same x
*                                      = -n: for n backspaces
*
      DO 80 I=1,50
         PIECE(I)=' '
         IFNS(I)=0
         IFNB(I)=0
         LEVEL(I)=0
         LBACK(I)=0
   80 CONTINUE

      ROMAN=.TRUE.
      GREEK=.FALSE.
      SPECIA=.FALSE.
      ZAPF=.FALSE.
      NORMAL=.TRUE.
      SUPER=.FALSE.
      SUB=.FALSE.

*NT=number for pieces  of text
      NT=0
      I=0
   90 I=I+1
      IF(I.GT.NCHP) GO TO 140
      NBAS=0
*
*  read character number I and check if it is an escape character
*    using KERNLIB M432 with NG in /SLATE/ as return code
*
      JX=ICFMUL(CFLIP,CHAR2,I,I)
      IF(NG.GT.0) THEN
*
* find [ : go to greek
*
         IF(CHAR2(I:I).EQ.CFLIP(1:1)) THEN
            GREEK=.TRUE.
*
* find " : go to special
*
         ELSE IF(CHAR2(I:I).EQ.CFLIP(2:2)) THEN
            SPECIA=.TRUE.
* find ? : go to subscript
         ELSE IF(CHAR2(I:I).EQ.CFLIP(3:3)) THEN
            SUB=.TRUE.
            SUPER=.FALSE.
* find ^ : go to superscript
         ELSE IF(CHAR2(I:I).EQ.CFLIP(4:4)) THEN
            SUPER=.TRUE.
            SUB=.FALSE.
* find ] : end of greek or of Zapf=> go to roman
         ELSE IF(CHAR2(I:I).EQ.CFLIP(5:5)) THEN
            GREEK=.FALSE.
            ZAPF=.FALSE.
            ROMAN=.TRUE.
* find # : end of special or of Zapf => go to roman
         ELSE IF(CHAR2(I:I).EQ.CFLIP(6:6)) THEN
            ROMAN=.TRUE.
            ZAPF=.FALSE.
            SPECIA=.FALSE.
* find ! : go to normal level of script
         ELSE IF(CHAR2(I:I).EQ.CFLIP(7:7)) THEN
            NORMAL=.TRUE.
            SUPER=.FALSE.
            SUB=.FALSE.
* find ~ : go to ZapfDingbats
         ELSE IF(CHAR2(I:I).EQ.CFLIP(8:8)) THEN
            ZAPF=.TRUE.
* find & : backspace is required
         ELSE IF(CHAR2(I:I).EQ.CFLIP(9:9)) THEN
            NBAS=1
*       check if many backspaces are required:
*       compute how many consecutive backspaces
*       in the NCHP-I remaining characters
            DO 100 J=I+1,NCHP
               IF(CHAR2(J:J).EQ.CFLIP(9:9)) THEN
                  NBAS=NBAS+1
               ELSE
                  GO TO 110
               ENDIF
  100       CONTINUE
  110       CONTINUE
*
* Since I have to backspace some text, (part of the preceeding piece),
* I define two pieces:
*
* a. the string which follows normally the & ( i.e. up to the next
*    escape character)
*
* b. the string to be backspaced, i.e. a part of the preceeding piece so
*    I create a new piece which is a copy the preceeding with LBACK<0
*
            NT=NT+1
            LBACK(NT)=-NBAS
*     and the other parameters identical
            PIECE(NT)=PIECE(NT-1)
            IFNB(NT)=IFNB(NT-1)
            IFNS(NT)=IFNS(NT-1)
*      except the level, since the backspaced piece is not printed
            LEVEL(NT)=0
*       however, in case of multiple backspaces, take the &&&...&&& as a
*       whole
            IF(NBAS.GT.1) THEN
               I=I+NBAS-1
               GO TO 90
            ENDIF
* find $ : optional terminaison character
         ELSE IF(CHAR2(I:I).EQ.CFLIP(10:10)) THEN
            IF(I.EQ.NCHP) GO TO 140
         ENDIF
* the character is not a control character
      ELSE
*
* START of a new text: on the first character, or on the
*    first non escape char. which follows an escape char.
         IF(I.NE.1)JX=ICFMUL(CFLIP,CHAR2,I-1,I-1)
         IF(NG.NE.0.OR.I.EQ.1) THEN
            IDEB=I
            NT=NT+1
* set font # (IFNB)
            IF(ROMAN) IFNB(NT)=NFON
            IF(GREEK) IFNB(NT)=12
            IF(ZAPF) IFNB(NT)=14
            IF(SPECIA) IFNB(NT)=25
* set font size (IFNS)
            JSIZP=NINT(0.7*IS)
            JSIZB=NINT(0.7*IS)
            IFNS(NT)=IS
            IF(SUPER) IFNS(NT)=JSIZP
            IF(SUB) IFNS(NT)=JSIZB
* set level flag (LEVEL)
            NYPOS=0
            NYSUP=NINT(IS/2.)
            NYSUB=NINT(-IS/3.)
            LEVEL(NT)=NYPOS
            IF(SUPER) LEVEL(NT)=NYSUP
            IF(SUB) LEVEL(NT)=NYSUB
         ENDIF
*set LBACK flag (1 for sub/uperscript)
         IF(SUPER.OR.SUB) LBACK(NT)=1

* END of a this text: on the last character or on
*      the last non escape which preceeds an escape (but
*      the terminating escape character itself is not known)
         IF(I.NE.NCHP)JX=ICFMUL(CFLIP,CHAR2,I+1,I+1)
         IF(NG.NE.0.OR.I.EQ.NCHP) THEN
            IFIN = I
*      compute text length and make one piece if length <74
*      and not 80, because of the () and \040 on the PostScript file
            ILEN = IFIN-IDEB+1
            IF(ILEN.LT.74) THEN
               PIECE(NT) = CHAR2(IDEB:IFIN)
*      if the last character is ' ' it is replaced with \040
               IF(CHAR2(IFIN:IFIN).EQ.' ')THEN
                  PIECE(NT)(ILEN:ILEN) = BSLASH
                  PIECE(NT)(ILEN+1:)   = '040'
               ENDIF
*      make several pieces if length > 74
            ELSE
               I1  = IDEB
               I2  = I1+73
               NTS = NT
*      check if CHAR2 will not be cut in the middle of an octal code
  120          IB  = INDEX(CHAR2(I2-2:I2),BSLASH)
               IF (IB.NE.0.AND.I2-I1.EQ.73.AND.I2.NE.NCHP) I2 = I2-4+IB
*      copy CHAR2 in the piece number NT with I2 readjusted
               PIECE(NT) = CHAR2(I1:I2)
*      if the last character is ' ' it is replaced with \040
               IF(CHAR2(I2:I2).EQ.' ')THEN
                  ILP = LENOCC(PIECE(NT))+1
                  PIECE(NT)(ILP:ILP) = BSLASH
                  PIECE(NT)(ILP+1:)  = '040'
               ENDIF
               IF (I2.EQ.ILEN) GOTO 130
               I1        = I2+1
               I2        = MIN(ILEN,I1+73)
               NT        = NT+1
               IFNB(NT)  = IFNB(NTS)
               IFNS(NT)  = IFNS(NTS)
               LEVEL(NT) = LEVEL(NTS)
               LBACK(NT) = LBACK(NTS)
               GOTO 120
            ENDIF
  130       CONTINUE
         ENDIF
      ENDIF
      GO TO 90
  140 CONTINUE
*
* Finally, a fourth parsing for 3 reasons:
*
      DO 160 I=1,NT
*** 1. LEVEL of sub/superscript after a multiple backsp. text:
*      one has:
*      i-2: text normally output lback=0
*      i-1 : text following in superscript mode
*      i : part of the preceeding (not printed) in which one
*          computes the backspace
*      i+1: text following the backspace
*      =>  since PIECE(i-1) and PIECE(I+1) are superposed;
*      I increase the level such that LEVEL(I-1)=IFNS(I-1)

         IF(LBACK(I).LT.-1) THEN
            IF (I.GT.1) THEN
*   superscript
               IF(LEVEL(I-1).GT.0) LEVEL(I-1)=IFNS(I-1)
*   subscript
               IF(LEVEL(I-1).LT.0) LEVEL(I-1)=-IFNS(I-1)
            ENDIF
         ENDIF

*** 2. LEVEL of sub/ superscript after ONE backspaced text:
*   put the LEVEL to +(actual font size) for superscript
*    and to - (actuel font size) for subscript
         IF (I.GT.1) THEN
            IF(LBACK(I-1).EQ.-1) THEN
*   superscript
               IF(LEVEL(I).GT.0) LEVEL(I)=IFNS(I)
*   subscript
               IF(LEVEL(I).LT.0) LEVEL(I)=-IFNS(I)
            ENDIF
         ENDIF

*** 3. correct in the greek text the 4 characters in the /Symbol font
* which are not " at their correct place" w.r.t. the HIGZ official table
         IF(IFNB(I).EQ.12) THEN
            DO 150 J=1,LENOCC(PIECE(I))
               IF(PIECE(I)(J:J).EQ.'J') THEN
                  PIECE(I)(J:J)='I'
               ELSEIF(PIECE(I)(J:J).EQ.'V') THEN
                  PIECE(I)(J:J)='C'
               ELSEIF(PIECE(I)(J:J).EQ.'C') THEN
                  PIECE(I)(J:J)='H'
               ELSEIF(PIECE(I)(J:J).EQ.'H') THEN
                  PIECE(I)(J:J)='C'
*
               ELSEIF(PIECE(I)(J:J).EQ.'j') THEN
                  PIECE(I)(J:J)='i'
               ELSEIF(PIECE(I)(J:J).EQ.'v') THEN
                  PIECE(I)(J:J)='c'
               ELSEIF(PIECE(I)(J:J).EQ.'c') THEN
                  PIECE(I)(J:J)='h'
               ELSEIF(PIECE(I)(J:J).EQ.'h') THEN
                  PIECE(I)(J:J)='c'
               ENDIF
  150       CONTINUE
         ENDIF
  160 CONTINUE
*
* write PS
* position of text from arguments
*
      PSANGL=RANGLE
*
      CALL IPPSTR('@')

* 1. text is left aligned
      IF(ITXALH.LE.1) THEN
         CALL IPSVRT(1)
         CALL IPDRAW(0,X,Y)
         CALL IPPSTR('@')
         WRITE (LUNPS,10000,ERR=350) NINT(PSANGL)
10000    FORMAT(' t ',I3,' r 0 0 m')
*
*   2. the text is centered or right-adjusted => compute
*      the whole length
*
      ELSEIF(ITXALH.EQ.2.OR.ITXALH.EQ.3) THEN

*    initialize the variable containing the string length
         WRITE(LUNPS,10100,ERR=350)
10100    FORMAT(' /xs 0 def ')

*    loop on all pieces and add the length of each piece
         IPIECE=0
  170    IPIECE=IPIECE+1
         IF(IPIECE.GT.NT) GO TO 250
*
*   2.1. ONE bakspaced text: forget the piece to be backspaced
*        and the piece which follows
*
         IF(LBACK(IPIECE).EQ.-1) THEN
            IPIECE=IPIECE+1
            GO TO 170
         ENDIF
*
*   2.2  backspaced text by more than one backspace
*
         IF(LBACK(IPIECE).LT.-1) THEN
            ILEN=LENOCC(PIECE(IPIECE))
            WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE)(1:ILEN)
            WRITE(LUNPS,13400,ERR=350) ABS(LBACK(IPIECE))
     +,     PSFONT(IFNB(IPIECE))(1:LENOCC(PSFONT(IFNB(IPIECE))))
     +,     IFNS(IPIECE)
            GO TO 170
         ENDIF
*
*  2.3  many superscript and many subscript at the same x
*
         IF(LBACK(IPIECE).EQ.1.AND.LBACK(IPIECE+1).EQ.1) THEN
*
*   loop on pieces, computes how many pieces withe LBACK=1
*    and check if they are all at the same level;
*    if yes, this is "standart" text
            N1=0
            N2=0
            DO 180 J=IPIECE,NT
               IF(LBACK(J).NE.1) GO TO 190
               IF(LEVEL(J).EQ.LEVEL(IPIECE)) THEN
                  N1=N1+1
               ELSE
                  N2=N2+1
               ENDIF
  180       CONTINUE
  190       CONTINUE
            IF(N1.EQ.0.OR.N2.EQ.0) GO TO 240
*
*      since many fonts are possible in sub/superscript, we output all
*      the pieces in super/subscript then thoses in sub/superscript
*
*   a) first pieces subscript or superscript
            WRITE(LUNPS,10200)
10200       FORMAT(' /s1 0 def ')
            DO 200 J=IPIECE,NT
               IF(LEVEL(J).NE.LEVEL(IPIECE)) GO TO 210
               WRITE(LUNPS,13200,ERR=350)
     +         PSFONT(IFNB(J))(1:LENOCC(PSFONT(IFNB(J)))),IFNS(IPIECE)
               ILEN=LENOCC(PIECE(J))
               WRITE(LUNPS,10300,ERR=999) PIECE(J)(1:ILEN)
10300          FORMAT(' (',A,') sw pop s1 add /s1 exch def')
*
               NEW=J
  200       CONTINUE
  210       CONTINUE

*   b) then superscript or subscript
            WRITE(LUNPS,10400)
10400       FORMAT(' /s2 0 def ')
            NEW=NEW+1
            DO 220 J=NEW,NT
               IF(LEVEL(J).NE.LEVEL(NEW)) GO TO 230
               WRITE(LUNPS,13200,ERR=350)
     +         PSFONT(IFNB(J))(1:LENOCC(PSFONT(IFNB(J)))),IFNS(IPIECE)
               ILEN=LENOCC(PIECE(J))
               WRITE(LUNPS,10500,ERR=999) PIECE(J)(1:ILEN)
10500          FORMAT(' (',A,') sw pop s2 add /s2 exch def')
               NNEW=J
  220       CONTINUE
  230       CONTINUE
*
* between subscript and superscript, which one is the longest?
            WRITE(LUNPS,10600,ERR=350)
10600       FORMAT(' s1 s2 ge { xs s1 add /xs exch def}'
     +        ,' { xs s2 add /xs exch def} ifelse ')
*
* since many pieces are treated :increase piece counter accordingly
            IPIECE=NNEW
            GO TO 170
         ENDIF
*
* 2.4. "standart" text:
*
  240    CONTINUE
         IF(LBACK(IPIECE).EQ.0.OR.LBACK(IPIECE).EQ.1) THEN
            ILEN=LENOCC(PIECE(IPIECE))
            WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE)(1:ILEN)
            WRITE(LUNPS,10700,ERR=350)
     +      PSFONT(IFNB(IPIECE))(1:LENOCC(PSFONT(IFNB(IPIECE))))
     +,     IFNS(IPIECE)
10700       FORMAT(1X,A,1X,I6,' stwn ')
         ENDIF
         GO TO 170
  250    CONTINUE
*
*              Centered text
*
         IF(ITXALH.EQ.2) THEN
            CALL IPSVRT(1)
            CALL IPDRAW(0,X,Y)
            CALL IPPSTR('@')
            WRITE(LUNPS,10800,ERR=350) NINT(PSANGL)
10800       FORMAT(' t ',I3,' r ',' xs 2 div neg 0 t 0 0 m')
*
*              Right adjusted text
*
         ELSEIF(ITXALH.EQ.3) THEN
            CALL IPSVRT(1)
            CALL IPDRAW(0,X,Y)
            CALL IPPSTR('@')
            WRITE(LUNPS,10900,ERR=350) NINT(PSANGL)
10900       FORMAT(' t ',I3,' r ',' xs neg 0 t 0 0 m')
         ENDIF
      ENDIF

*   now output the pieces
      IPIECE=0
  260 IPIECE=IPIECE+1
      IF(IPIECE.GT.NT) GO TO 340
*
* make the PostScript file:
*
* 1. ONE bakspace: output "piece" to be backspaced AND following piece
*    first save current graphic state, compute backward distance,
*       and move to that point
*
      IF(LBACK(IPIECE).EQ.-1) THEN
         CALL IPSVRT(1)
         WRITE(LUNPS,11000,ERR=350)
     +   PSFONT(IFNB(IPIECE))(1:LENOCC(PSFONT(IFNB(IPIECE))))
     +,  IFNS(IPIECE)
11000    FORMAT(A,' findfont ',I4,' sf ')
         ILEN=LENOCC(PIECE(IPIECE))
         WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE)(1:ILEN)
         WRITE(LUNPS,11100,ERR=350)
11100    FORMAT( ' dup length 1 sub 1 getinterval ')
         WRITE(LUNPS,11200,ERR=350) LEVEL(IPIECE)
11200    FORMAT( ' stringwidth pop 2 div neg ',I4,' rm ')

* then, text following one backspace: backspace also 1/2 of text
*   ( normally one character) print and restore preceeding graphic state
         WRITE(LUNPS,11300,ERR=350)
     +   PSFONT(IFNB(IPIECE+1))(1:LENOCC(PSFONT(IFNB(IPIECE+1))))
     +,  IFNS(IPIECE+1),LEVEL(IPIECE+1)
11300    FORMAT(1X,A,' findfont ',I4,' sf 0 ',I4,' rm ')
         ILEN=LENOCC(PIECE(IPIECE+1))
         WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE+1)(1:ILEN)
         WRITE(LUNPS,11400,ERR=350)
11400    FORMAT(' stringwidth pop 2 div neg 0 rm ')
         ILEN=LENOCC(PIECE(IPIECE+1))
         WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE+1)(1:ILEN)
         IF(OSHOW) THEN
            WRITE(LUNPS,'('' oshow'')',ERR=350)
            CALL IPSVRT(-1)
         ELSE
            WRITE(LUNPS,'('' show'')',ERR=350)
            CALL IPSVRT(-1)
         ENDIF
* since two pieces are treated increase piece counter
         IPIECE=IPIECE+1
         GO TO 260
      ENDIF
*
*   2. Many Backspaces
*
      IF(LBACK(IPIECE).LT.-1) THEN
*       first, protect against a number of bakspaces larger than
*       the total number of characters in the string to be
*       backspaced
         ILEN=LENOCC(PIECE(IPIECE))
         WRITE(LUNPS,13300,ERR=350) ABS(LBACK(IPIECE))
     +,  PIECE(IPIECE)(1:ILEN)
*       then output
         WRITE(LUNPS,13200,ERR=350)
     +   PSFONT(IFNB(IPIECE))(1:LENOCC(PSFONT(IFNB(IPIECE))))
     +,  IFNS(IPIECE)
         WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE)(1:ILEN)
         WRITE(LUNPS,11700,ERR=350)
11700    FORMAT(' dup length nbas sub nbas getinterval stringwidth '
     +   ,'pop neg 0 t ')
         GO TO 260
      ENDIF
*
*  3.  superscript and subscript at the same x
      IF(LBACK(IPIECE).EQ.1.AND.LBACK(IPIECE+1).EQ.1) THEN
*
*   loop on pieces, computes how many pieces withe LBACK=1
*    and check if they are all at the same level;
*    if yes, this is "standart" text
         N1=0
         N2=0
         DO 270 J=IPIECE,NT
            IF(LBACK(J).NE.1) GO TO 280
            IF(LEVEL(J).EQ.LEVEL(IPIECE)) THEN
               N1=N1+1
            ELSE
               N2=N2+1
            ENDIF
  270    CONTINUE
  280    CONTINUE
         IF(N1.EQ.0.OR.N2.EQ.0) GO TO 330
*
*      since many fonts are possible in sub/superscript, we output all
*      the pieces in super/subscript then thoses in sub/superscript
*
*   a) first pieces subscript or superscript
         CALL IPSVRT(1)
         CALL IPSVRT(1)
         WRITE(LUNPS,11900,ERR=350) LEVEL(IPIECE)
11900    FORMAT(' 0 ',I4,' rm ')
*
         DO 290 J=IPIECE,NT
            IF(LEVEL(J).NE.LEVEL(IPIECE)) GO TO 300
            WRITE(LUNPS,13200,ERR=350)
     +      PSFONT(IFNB(J))(1:LENOCC(PSFONT(IFNB(J)))),IFNS(IPIECE)
            ILEN=LENOCC(PIECE(J))
            WRITE(LUNPS,13100,ERR=999) PIECE(J)(1:ILEN)
            IF(OSHOW) THEN
               WRITE(LUNPS,12000,ERR=350)
12000          FORMAT(' dup oshow  true charpath currentpoint pop'
     +               ,' /s1 exch def')
            ELSE
               WRITE(LUNPS,12100,ERR=350)
12100          FORMAT(' show currentpoint pop /s1 exch def')
            ENDIF
*
            NEW=J
  290    CONTINUE
  300    CONTINUE
         CALL IPSVRT(-1)
*
*   b) then superscript or subscript
         NEW=NEW+1
*
         WRITE(LUNPS,11900,ERR=350) LEVEL(NEW)
         DO 310 J=NEW,NT
            IF(LEVEL(J).NE.LEVEL(NEW)) GO TO 320
            WRITE(LUNPS,13200,ERR=350)
     +      PSFONT(IFNB(J))(1:LENOCC(PSFONT(IFNB(J)))),IFNS(IPIECE)
            ILEN=LENOCC(PIECE(J))
            WRITE(LUNPS,13100,ERR=999) PIECE(J)(1:ILEN)
            IF(OSHOW) THEN
               WRITE(LUNPS,12300,ERR=350)
12300          FORMAT(' dup oshow  true charpath currentpoint pop '
     +                , ' /s2 exch def')
            ELSE
               WRITE(LUNPS,12400,ERR=350)
12400          FORMAT(' show currentpoint pop /s2 exch def')
            ENDIF
            NNEW=J
  310    CONTINUE

  320    CONTINUE
         CALL IPSVRT(-1)
*
* at which x- value, should one translate the current state?
         WRITE(LUNPS,12500,ERR=350)
12500    FORMAT(' s1 s2 ge {s1 0 t} {s2 0 t} ifelse ')
*
* since many pieces are treated :increase piece counter accordingly
         IPIECE=NNEW
         GO TO 260
      ENDIF
*
* 4. "standart" text: output current "piece" of text
*
  330 CONTINUE
      IF(LBACK(IPIECE).EQ.0.OR.LBACK(IPIECE).EQ.1) THEN
         WRITE(LUNPS,12600,ERR=350)
     +   PSFONT(IFNB(IPIECE))(1:LENOCC(PSFONT(IFNB(IPIECE))))
     +,  IFNS(IPIECE),LEVEL(IPIECE)
12600    FORMAT(1X,A,' findfont ',I4,' sf 0 ',I4,' m ')
         ILEN=LENOCC(PIECE(IPIECE))
         WRITE(LUNPS,13100,ERR=999) PIECE(IPIECE)(1:ILEN)
         IF(OSHOW) THEN
            WRITE(LUNPS,12700,ERR=350)
12700       FORMAT(' dup oshow')
*           move currentpoint ( if not last piece of text)
            IF(IPIECE.NE.NT) WRITE(LUNPS,12800,ERR=350)
12800       FORMAT(' true charpath currentpoint pop 0 t ')
         ELSE
            WRITE(LUNPS,12900,ERR=350)
12900       FORMAT(' show ')
*           move currentpoint ( if not last piece of text)
            IF(IPIECE.NE.NT) WRITE(LUNPS,13000,ERR=350)
13000       FORMAT(' currentpoint pop 0 t ')
         ENDIF
         GO TO 260
      ENDIF

*  end of loop on pieces
  340 CONTINUE

* restore graphic state after the last piece of text
      CALL IPSVRT(-1)
      GOTO 999
*
  350 CALL IGERR('Cannot write in the PostScript file','IPTEXT')
      GOTO 999
*
  360 CALL IGERR('Too many characters in input string','IPTEXT')
13100 FORMAT('(',A,')')
13200 FORMAT(1X,A,' findfont ',I4,' sf')
13300 FORMAT(1X,I4,' /nbas exch def ',/,1X,'(',A,')',/,
     +' length /tlen exch def nbas tlen gt { /nbas tlen def } if ')
13400 FORMAT(' ',I2,' ',A,I4,' stwb ')
*
  999 END
