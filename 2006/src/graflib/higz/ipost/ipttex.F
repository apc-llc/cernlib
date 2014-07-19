*
* $Id: ipttex.F,v 1.1.1.1 1996/02/14 13:11:08 mclareni Exp $
*
* $Log: ipttex.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:08  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/16 14/08/91  16.21.01  by  O.Couet
*-- Author :
      SUBROUTINE IPTTEX(X,Y,CHARS)
      CHARACTER*(*) CHARS
*.===========>
*.
*.    This routine translates text to TeX format
*.    according to the IGTEXT control characters.
*.
*. _Input parameters:
*.
*.  CHARACTER CHARS : Text string .
*.
*..==========> (A.Nathaniel)
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
      PARAMETER(MFONTS=8)
      CHARACTER CH*1,FONTS(MFONTS)*2
      LOGICAL LOWER,LGREEK,LSPEC,LSUPER
      CHARACTER LETTER(26,4)*12
*--- LaTeX names for greek letters and special symbols
*        '123456789012','123456789012','123456789012','123456789012'
      DATA (LETTER(I,1),I=1,26)/
     +   '|alpha      ','|beta       ','|eta        ','|delta      ',
     +   '|epsilon    ','|phi        ','|gamma      ','|chi        ',
     +   '|iota       ','|iota       ','|kappa      ','|lambda     ',
     +   '|mu         ','|nu         ','o           ','|pi         ',
     +   '|theta      ','|rho        ','|sigma      ','|tau        ',
     +   '|upsilon    ','|chi        ','|omega      ','|xi         ',
     +   '|psi        ','|zeta       '/
      DATA (LETTER(I,2),I=1,26)/
     +   'A           ','B           ','H           ','|Delta      ',
     +   'E           ','|Phi        ','|Gamma      ','X           ',
     +   'I           ','I           ','K           ','|Lambda     ',
     +   'M           ','N           ','O           ','|Pi         ',
     +   '|Theta      ','R           ','|Sigma      ','T           ',
     +   '|Upsilon    ','X           ','|Omega      ','|Xi         ',
     +   '|Psi        ','Z           '/
      DATA (LETTER(I,3),I=1,26)/
     +   '|pm         ','|mid        ','|ast        ','|$          ',
     +   '!           ','|#          ','>           ','?           ',
     +   '|int        ',':           ',';           ','<           ',
     +   '[           ',']           ','|leq        ','|{          ',
     +   '|}          ','|surd       ','|cap        ','|dagger     ',
     +   '|ddagger    ','|cup        ','|&          ','|times      ',
     +   '|partial    ','|infty      '/
      DATA (LETTER(I,4),I=1,26)/
     +   '|pm         ','|mid        ','|div        ','|$          ',
     +   '!           ','|#          ','>           ','?           ',
     +   '|int        ',':           ',';           ','<           ',
     +   '[           ',']           ','|geq        ','|{          ',
     +   '|}          ','|surd       ','|spadesuit  ','|heartsuit  ',
     +   '|diamondsuit','|clubsuit   ','|&          ','|times      ',
     +   '|%          ','|infty      '/

      DATA FONTS/'rm','em','bf','it','sl','sf','sc','tt'/
*.______________________________________
*
#include "higz/hiwcps.inc"
      IF(X.LT.X1W .OR. X.GT.X2W .OR. Y.LT.Y1W .OR. Y.GT.Y2W) RETURN
      CALL IPPSTR(BSLASH//'put')
      IXD=IXWCPS(X)
      IYD=IYWCPS(Y)
      CALL IPJOUT(IXD,IYD)
      CALL IPPSTR('{'//BSLASH//'makebox(0,0)')
      IF(ITXALH.NE.2 .OR. ITXALV.NE.3) THEN
         CALL IPPSTR('[')
         IF(ITXALV.EQ.1 .OR. ITXALV.EQ.2) THEN
            CALL IPPSTR('t')
         ELSEIF(ITXALV.NE.3) THEN
            CALL IPPSTR('b')
         ENDIF
         IF(ITXALH.EQ.3) THEN
            CALL IPPSTR('r')
         ELSEIF(ITXALH.NE.2) THEN
            CALL IPPSTR('l')
         ENDIF
         CALL IPPSTR(']')
      ENDIF
      IFNT=ABS(IFONT)
      IF(IFNT.LT.1 .OR. IFNT.GT.MFONTS) IFNT=1
      CALL IPPSTR('{$'//BSLASH//FONTS(IFNT)//'{}')

      LOWER=.FALSE.
      LGREEK=.FALSE.
      LSPEC=.FALSE.
      LSUPER=.FALSE.

      DO 10  I=1,LENOCC(CHARS)
         CH=CHARS(I:I)
         LLOW=ICHAR(CH)-ICHAR('a')+1
         LCAP=ICHAR(CH)-ICHAR('A')+1
         IF(LOWER .AND. LCAP.GE.1 .AND. LCAP.LE.26) THEN
            LLOW=LCAP
            LCAP=0
            CH=CHAR(LLOW+ICHAR('a')-1)
         ENDIF
         IF(LLOW.GE.1 .AND. LLOW.LE.26 .OR.
     +      LCAP.GE.1 .AND. LCAP.LE.26) THEN
            IF(LLOW.GE.1 .AND. LLOW.LE.26) THEN
               IF(LGREEK) THEN
                  ICASE=1
               ELSEIF(LSPEC) THEN
                  ICASE=3
               ELSE
                  ICASE=0
               ENDIF
            ELSE
               LLOW=LCAP
               IF(LGREEK) THEN
                  ICASE=2
               ELSEIF(LSPEC) THEN
                  ICASE=4
               ELSE
                  ICASE=0
               ENDIF
            ENDIF
            IF(ICASE.EQ.0) THEN
               CALL IPPSTR(CH)
            ELSE
               L=INDEX(LETTER(LLOW,ICASE),' ')
               IF(L.EQ.0) L=LEN(LETTER(LLOW,ICASE))+1
               IF(LETTER(LLOW,ICASE)(1:1).EQ.'|') THEN
                  CALL IPPSTR(BSLASH//LETTER(LLOW,ICASE)(2:L-1)//'{}')
               ELSE
                  CALL IPPSTR(LETTER(LLOW,ICASE)(:L-1))
               ENDIF
            ENDIF
         ELSEIF(CH.EQ.' ') THEN
            CALL IPPSTR(BSLASH//' {}')
         ELSEIF(CH.EQ.'<') THEN
            LOWER=.TRUE.
         ELSEIF(CH.EQ.'>') THEN
            LOWER=.FALSE.
         ELSEIF(CH.EQ.'[') THEN
            LGREEK=.TRUE.
            LSPEC=.FALSE.
         ELSEIF(CH.EQ.']') THEN
            LGREEK=.FALSE.
         ELSEIF(CH.EQ.'"') THEN
            LSPEC=.TRUE.
            LGREEK=.FALSE.
         ELSEIF(CH.EQ.'#') THEN
            LSPEC=.FALSE.
         ELSEIF(CH.EQ.'^') THEN
            IF(LSUPER) CALL IPPSTR('}')
            CALL IPPSTR('^{')
            LSUPER=.TRUE.
         ELSEIF(CH.EQ.'?') THEN
            IF(LSUPER) CALL IPPSTR('}')
            CALL IPPSTR('_{')
            LSUPER=.TRUE.
         ELSEIF(CH.EQ.'!') THEN
            IF(LSUPER) CALL IPPSTR('}')
            LSUPER=.FALSE.
         ELSEIF(CH.EQ.'$') THEN
            IF(LSUPER) CALL IPPSTR('}')
            LSUPER=.FALSE.
            GOTO 20
         ELSEIF(CH.EQ.'*') THEN
            CALL IPPSTR(BSLASH//'ast{}')
         ELSEIF(INDEX('%}_',CH).GT.0) THEN
*--- TeX special characters which need to be escaped
            CALL IPPSTR(BSLASH//CH)
         ELSEIF(CH.EQ.'{' .OR. CH.EQ.BSLASH) THEN
*--- enter TeX mode
            IF(LSUPER) CALL IPPSTR('}')
            LSUPER=.FALSE.
            CALL IPPSTR(CHARS(I:))
            GOTO 20
         ELSE
            CALL IPPSTR(CH)
         ENDIF
   10 CONTINUE
   20 CONTINUE
      IF(LSUPER) CALL IPPSTR('}')
      CALL IPPSTR('$}}')
      END
