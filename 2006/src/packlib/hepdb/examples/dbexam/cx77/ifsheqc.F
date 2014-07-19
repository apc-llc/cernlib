*
* $Id: ifsheqc.F,v 1.1.1.1 1996/02/28 16:23:43 mclareni Exp $
*
* $Log: ifsheqc.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:43  mclareni
* Hepdb, cdlib, etc
*
*
      FUNCTION IFSHEQC(STR1,STR2)
*     ---------------------------
*--                              Author :    Boris Khomenko
*********************************************************
*     The function IFSHEQC gives the result of          *
*     the comparision of two character strings          *
*     ignoring their trailing blanks. The length        *
*     of comparision will be equal to the minimal       *
*     length of significant parts of STR1,STR2.         *
*     But if STR2 contains an asterisk, dividing        *
*     the string on two parts, the first one is         *
*     respected as the minimal portion for comparision. *
*     If STR1 is longer, the comrarision will be done   *
*     for full its significant length.                  *
*      Examples:                                        *
*      ---------                                        *
*         STR1      STR2     IFSHEQC                    *
*         abcd       abc        1      (OK)             *
*         ab         abc        1      (OK)             *
*         ad         abc        0      (NOK)            *
*         a         ab*cd       0      (NOK)            *
*         ab        ab*cd       1      (OK)             *
*         abcd      ab*cd       1      (OK)             *
*         abcde     ab*cd       0      (NOK)            *
*         <blank>   *ab         1      (OK)             *
*         ab        *ab         1      (OK)             *
*         b         *ab         0      (NOK)            *
*********************************************************
      IMPLICIT NONE
*
*       -- fun & arguments --
      INTEGER IFSHEQC
      CHARACTER*(*) STR1,STR2
*
*     -- externals --
      INTEGER LNBLNK
*
*     -- local variables --
      INTEGER LEN1,LEN2,LEN3,LL
*
      IFSHEQC=0
      LEN1=LNBLNK(STR1)
      LEN2=LNBLNK(STR2)
      LEN3=INDEX(STR2,'*')-1
      IF(LEN3.GE.0) THEN
        LL=MAX0(LEN1,LEN3)
        IF(LL.EQ.0) THEN
          IFSHEQC=1
        ELSE IF(LL.LE.LEN3) THEN
          IF(STR1(:LL).EQ.STR2(:LL)) IFSHEQC=1
        ELSE IF(LL.LE.LEN2-1) THEN
          IF((LEN3.EQ.0.OR.STR1(:LEN3).EQ.STR2(:LEN3)).AND.
     +       STR1(LEN3+1:LL).EQ.STR2(LEN3+2:LL+1)) IFSHEQC=1
        ENDIF
      ELSE
        LL=MIN0(LEN1,LEN2)
        IF(LL.GT.0) THEN
          IF(STR1(:LL).EQ.STR2(:LL)) IFSHEQC=1
        ELSE
          IFSHEQC=1
        ENDIF
      ENDIF
      END
