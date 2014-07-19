*
* $Id: encodr.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: encodr.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE ENCODR(FNUM,MODE,STRN,LENR)
*     ----------------------------====-====-
*
*    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*   $   To encode FNUM, according with MODE,   $
*   $   into STRN, with the effective code     $
*   $   length in LENR.                        $
*   $                                          $
*   $              Created by Boris Khomenko   $
*    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
*
      IMPLICIT NONE
*     *************
*     --- arguments ----
      REAL*4  FNUM
      INTEGER MODE,LENR
      CHARACTER*(*) STRN
*     ------------------
*
*     ---- local variables ----
      LOGICAL LEFT,FXPN
      INTEGER MMOD,NSGD,MXFR,MXCH
      INTEGER NNNN,LSGN,LINT,LFRZ,LFRX,LPNT,MXDG,NDIG
      INTEGER IEX0,IEXP,LEXP
      INTEGER IFRP,LNNN,JJ,LL
      REAL*4  PLOG,PNUM,RNNN
      CHARACTER ENCOD*16
      REAL*4 REAL4MX,REAL4MN
*     -------------------------
      DATA REAL4MX,REAL4MN/1E37,1E-37/
*
      PNUM=ABS(FNUM)
      LSGN=0
      IF(FNUM.LT.0.) LSGN=1
      MXCH=LEN(STRN)
      IF(MXCH-LSGN.LE.0) THEN
        STRN='*'
        RETURN
      ENDIF
*
      LEFT=MODE.LT.0
      MMOD=IABS(MODE)
      FXPN=MMOD.GT.99
      MMOD=MOD(MMOD,100)
      IF(MMOD.EQ.0) MMOD=69
      IF(MMOD.GE.10) THEN
        NSGD=MIN0(MMOD/10,9)
        MXFR=MOD(MMOD,10)
      ELSE
        NSGD=MMOD
        MXFR=9
      ENDIF
      IF(MXFR.LT.9) THEN
        IF(PNUM.LT.10.**(-MXFR-1)) PNUM=0
      ELSE IF(PNUM.NE.0) THEN
        PNUM=AMAX1(PNUM,REAL4MN)
      ENDIF
      PNUM=AMIN1(PNUM,REAL4MX)
*
      ENCOD=' '
      IF(LSGN.GT.0) ENCOD(1:1)='-'
      LENR=LSGN
*
      IEXP=0
      LEXP=0
      IF(PNUM.GT.0) THEN
        PLOG=ALOG10(PNUM)
        IEX0=IFIX(PLOG)-8
        IF(PLOG.LT.0) IEX0=IEX0-1
        RNNN=PNUM
        IF(IEX0.GE. 30) RNNN=RNNN*1E-30
        IF(IEX0.LE.-30) RNNN=RNNN*1E+30
        NNNN=RNNN/10.**MOD(IEX0,30)+.5
*
        IF(NNNN.GE.1000000000) THEN
          NNNN=NNNN/10
          IEX0=IEX0+1
        ELSE IF(NNNN.LT.100000000) THEN
          NNNN=NNNN*10
          IEX0=IEX0-1
        ELSE IF(NNNN/1000.EQ.999999.AND.MOD(NNNN/100,10).GE.5) THEN
          NNNN=100000000
          IEX0=IEX0+1
        ENDIF
*
        LINT=9+IEX0
        LPNT=1
        MXDG=MIN0(MXCH-LSGN-LPNT,NSGD+2,9)
        IF(LINT.EQ.MXDG+1.AND.LINT.LE.3) THEN
          LPNT=0
          MXDG=MXDG+1
        ELSE IF(LINT.GT.MXDG) THEN
          IEXP=LINT-1
          LINT=1
          IF(NSGD.EQ.9) NSGD=6
        ENDIF
*
        IF(LINT.LT.0) THEN
          LFRZ=-LINT
          IF(MXFR.LT.9.OR.LINT.GE.-3.AND.-LINT.LT.MXDG) THEN
            NNNN=NNNN/10**IABS(LINT)
            LINT=0
          ELSE
            IEXP=LINT
            LINT=0
            IF(IEXP.NE.-9) THEN
              IEXP=IEXP-1
              LINT=1
            ENDIF
          ENDIF
        ENDIF
*
        IFRP=0
   11   CONTINUE
        IFRP=-IABS(IFRP)
        IF(IEXP.NE.0) THEN
          LEXP=2
          IF(IABS(IEXP).GE.10) LEXP=3
          IF(IEXP.LT.0) LEXP=LEXP+1
          NDIG=MIN0(MXCH-LSGN-LPNT-LEXP,NSGD)
          IF(NDIG.EQ.0.AND.LPNT.GT.0) THEN
            IF(LINT.EQ.1) THEN
              LPNT=0
              NDIG=1
            ELSE IF(LINT.EQ.0) THEN
              NNNN=NNNN/10
              LINT=1
              LPNT=0
              NDIG=1
            ENDIF
          ENDIF
          IF(NDIG.GE.0.AND.NNNN+5*10**(8-NDIG).GT.999999999) THEN
            NNNN=NNNN/10
            IF(LINT.EQ.0.OR.IEXP.EQ.9) THEN
              LINT=LINT+1
            ELSE
              IEXP=IEXP+1
            ENDIF
            IFRP=IABS(IFRP)+1
          ENDIF
          IF(IABS(IEXP).LE.10.AND.LPNT.GT.0) THEN
            LL=((IEXP+ISIGN(1,IEXP))/3)*3-IEXP
            IF(LL.EQ.1.AND.LINT.GT.0 .OR.
     +         LL.EQ.-1.AND.NDIG-LINT.GT.0) THEN
              IEXP=IEXP+LL
              LINT=LINT-LL
              IFRP=IABS(IFRP)+1
            ENDIF
          ENDIF
        ELSE
*         -- to encode without exponent
          MXDG=MXCH-LSGN-LPNT
          IF(LINT.GT.0) THEN
            NDIG=MIN0(MXDG,LINT+MXFR,MAX0(NSGD,LINT))
          ELSE
            NDIG=MIN0(MXDG,LINT+MXFR,LFRZ+NSGD)
          ENDIF
          IF(NDIG.GE.0) THEN
            IF(NNNN+5*10**(8-NDIG).GT.999999999) THEN
              NNNN=NNNN/10
              LINT=LINT+1
              IF(LINT.GT.MXDG) THEN
                IEXP=3
                LINT=LINT-3
              ENDIF
              IFRP=IABS(IFRP)+1
            ELSE IF(NDIG.EQ.LINT-1) THEN
              LPNT=0
              IFRP=IABS(IFRP)+1
            ENDIF
          ENDIF
        ENDIF
*
        IF(IFRP.GT.0) GOTO 11
*
        IF(NDIG.GT.0) THEN
          IF(NDIG.LT.9) THEN
            NNNN=NNNN/10**(8-NDIG)
            NNNN=(NNNN+5)/10
          ENDIF
          CALL ENCODI(NNNN,-NDIG,ENCOD(LENR+1:),LNNN)
          IF(LPNT.GT.0) THEN
            JJ=LENR+LINT+1
            LL=MAX0(LNNN-LINT,0)
            IF(LL.GT.0) ENCOD(JJ+1:JJ+LL)=ENCOD(JJ:JJ+LL-1)
            ENCOD(JJ:JJ)='.'
            LFRX=LL
          ELSE
            LFRX=0
          ENDIF
          LENR=LENR+LNNN+LPNT
*
          IF(IEXP.NE.0) THEN
            JJ=LENR+1
            ENCOD(JJ:JJ)='E'
            CALL ENCODI(IEXP,-1,ENCOD(JJ+1:JJ+3),LL)
            LENR=LENR+1+LL
            LFRX=     1+LL
          ENDIF
        ELSE
*       -- Field overflow, RETURN
          DO 199 JJ=1,MXCH
  199     STRN(JJ:JJ)='*'
*
          IF(LSGN.GT.0.AND.MXCH.GE.2) STRN(1:1)='-'
          IF(MXCH.GE.LSGN+2) THEN
            IF(PNUM.LT.1) THEN
              STRN(LSGN+1:LSGN+1)='.'
            ELSE
              STRN(MXCH:MXCH)='.'
            ENDIF
          ENDIF
*
          LENR=MXCH
          RETURN
        ENDIF
*
      ELSE IF(FNUM.NE.0.) THEN
*       -- a QUAZY null
      ENCOD(LENR+1:)='0.000000000'
        LL=3
        IF(MXFR.LT.9) LL=MXFR+2
        LENR=MIN0(LENR+LL,MXCH)
*
      ELSE
*       -- a TRUE null
        ENCOD='0'
        LENR=1
        LFRX=-1
      ENDIF
*
      STRN=' '
      IF(LEFT) THEN
        STRN(1:LENR)=ENCOD
      ELSE
        JJ=MXCH-LENR+1
        IF(FXPN.AND.MXFR.LT.9) JJ=MAX0(MIN0(JJ,JJ-MXFR+LFRX),1)
        STRN(JJ:MXCH)=ENCOD
        LENR=MXCH-JJ+1
      ENDIF
      END
