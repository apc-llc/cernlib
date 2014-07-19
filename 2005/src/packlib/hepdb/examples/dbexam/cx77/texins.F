*
* $Id: texins.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: texins.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE TEXINS(STRN,JPAK,IFTX,PRFX,INUM,FNUM)
*     ---15.09.88------------****-====-====-====-====-
*
      CHARACTER*(*) STRN,PRFX
*
      PARAMETER (MAXPOS=2147483647)
      EQUIVALENCE(IN,FN)
      CHARACTER*1 CH,C2,C3
*
*   Get info from JPAK
      JNX=MAX0(MOD(JPAK/200,200),1)
      JMX=MOD(JPAK,200)
      IF(JMX.EQ.0) JMX=MIN0(LEN(STRN),199)
*
*   Init pointers,prefix and numbers
      JTX=0
      JNM=0
      PRFX=' '
      JEP=0
      INUM=0
      FNUM=0
*
*    Init IFTX components
      IFNM=0
      IFPR=0
      IFNX=0
*
*   Return with IFTX=0 if string is already finished
      IF(JNX.GT.JMX) GOTO 90
*   Else go on
*    Init number attributes
      ISN=0
      IJS=0
      FRAC=-1.
*
*     They will be very useful
      IC0=ICHAR('0')
      ICA=ICHAR('A')-10
      LCA=ICHAR('a')-10
      LIMTST=(MAXPOS-9)/10
      LIMHEX=MAXPOS/16+1
*
*     Get starting
      J=JNX
      JNX=0
*          JNX >0 will force end of scan
   10 CONTINUE
*
      CH=STRN(J:J)
      C2=' '
      C3=' '
      IF(J.LT.JMX) C2=STRN(J+1:J+1)
      IF(J.LT.JMX-1) C3=STRN(J+2:J+2)
*
      IF(CH.EQ.' ') THEN
*      no actoin with leading blanc
*      end of level=1 by blanc after text
      IF(JTX.GT.0) THEN
        JNX=J+1
        IFNX=1
      ENDIF
*
      ELSE IF(CH.EQ.',') THEN
*      end of level=2 by comma
      IF(JTX.LE.0)  JTX=J
      JNX=J+1
      IFNX=2
      IF(C2.EQ.' ') IFNX=1
*
      ELSE IF(CH.EQ.'/') THEN
*      end of level=3 by slash
      IF(JTX.LE.0) JTX=J
      JNX=J+1
      IFNX=3
      IF(C2.EQ.' ') IFNX=1
*
      ELSE
*     =================================
*     ===== It isn't a delimiter ======
*
      IF(IFNM.LE.0) THEN
*     ------- Non number stage --------
        IF(JTX.LE.0) THEN
*          Something is encountered
          JTX=J
          IFTX=0
        ENDIF
*
        C2=' '
        C3=' '
        IF(J.LT.JMX) C2=STRN(J+1:J+1)
        IF(J.LT.JMX-1) C3=STRN(J+2:J+2)
*
        IF(CH.GE.'0'.AND.CH.LE.'9') THEN
*          Int.number start by a digit
          IFNM=1
        ELSE IF(CH.EQ.'+'.OR.CH.EQ.'-') THEN
          IF(C2.EQ.'.') THEN
            IF(C3.GE.'0'.AND.C3.LE.'9') THEN
*            Flt.number start by sign, point and digit
            IFNM=3
            IF(CH.EQ.'-') ISN=1
            FRAC=.1
            IJS=2
            ENDIF
*
          ELSE IF(C2.GE.'0'.AND.C2.LE.'9') THEN
*            Int.number start by sign and digit
            IFNM=1
            IF(CH.EQ.'-') ISN=1
            IJS=1
          ENDIF
        ELSE IF(CH.EQ.'.') THEN
          IF(C2.GE.'0'.AND.C2.LE.'9') THEN
*            Flt.number start by point and digit
            IFNM=3
            IJS=1
            FRAC=.1
          ENDIF
        ELSE IF(CH.EQ.'#'.AND.C2.EQ.''''.AND.
     +         (C3.GE.'0'.AND.C3.LE.'9'.OR.
     +          C3.GE.'A'.AND.C3.LE.'F'.OR.
     +          C3.GE.'a'.AND.C3.LE.'f')     ) THEN
*          Hex.number start by #'h...
          IFNM=2
          IJS=2
        ENDIF
        IF(IFNM.GT.0) THEN
*          Mark start of number and correct
*             J position to its 1st digit
          JNM=J
          J=J+IJS
        ELSE
*          Not a number, joint this char to prefix
          JEP=J
        ENDIF
      ENDIF
*
*   ---------number stage-------------
      IF(IFNM.GT.0) THEN
        CH=STRN(J:J)
        ICH=ICHAR(CH)
*
*        If char is a digit, get in IV its value, else IV=-1
*        But if char is the flt.point, IV=99 and IFNM=3 are forced
        IV=-1
        IF(CH.GE.'0'.AND.CH.LE.'9') THEN
          IV=ICH-IC0
        ELSE IF(CH.EQ.'.'.AND.IFNM.NE.2.AND.FRAC.LT.0.) THEN
          IF(IFNM.EQ.1) FNUM=INUM
          IFNM=MAX0(IFNM,3)
          FRAC=.1
          IV=99
        ELSE IF(IFNM.EQ.2) THEN
          IF(CH.GE.'A'.AND.CH.LE.'F') IV=ICH-ICA
          IF(CH.GE.'a'.AND.CH.LE.'f') IV=ICH-LCA
        ENDIF
*                IV is defined.
*
*
        IF(IV.GE.0) THEN
*        Here we have a legal digit or, may be, flt.point
*        - - - - - - - - - - - - - - - - - - - - - - - - -
*
          IF(IFNM.EQ.1.AND.INUM.GT.LIMTST) THEN
*            Test on possibility of integer overflow
            IF(INUM.GT.(MAXPOS-IV)/10) THEN
*            if it will occur, mark it in IFNM
*            and turn to the float mode
            IFNM=7
            FNUM=INUM
            ENDIF
          ENDIF
*
          IF(IFNM.EQ.1) THEN
*            Integer
            INUM=INUM*10+IV
*
          ELSE IF(IFNM.EQ.2) THEN
*            Hexadecimal
            IF(INUM.LT.LIMHEX) THEN
            INUM=INUM*16+IV
            ELSE
            INUM=(INUM-LIMHEX)*16-MAXPOS-1+IV
            ENDIF
*
          ELSE IF(IFNM.GE.3) THEN
*             Flt.point
            IF(IV.NE.99) THEN
            IF(FRAC.LE.0.) THEN
*               Entiere part
              FNUM=FNUM*10.+FLOAT(IV)
            ELSE
*               Fraction part
              FNUM=FNUM+FLOAT(IV)*FRAC
              FRAC=FRAC/10.
            ENDIF
            ENDIF
          ENDIF
*        - - - - - - - - - - - - - - - - - - - - - - - - -
        ELSE
*         The char isn't acceptable for the number,the scan
*         be stopped with IFNX=3. But .'. may conclude
*         the hexa number and be included into number text.
          JNX=J
          IF(IFNM.EQ.2.AND.CH.EQ.'''') JNX=J+1
          IFNX=3
*        - - - - - - - - - - - - - - - - - - - - - - - - -
        ENDIF
*
      ENDIF
      ENDIF
*
*     Next char position and repeate or stop
      J=J+1
      IF(J.GT.JMX.AND.JNX.LE.0) JNX=J
      IF(JNX.LE.0) GOTO 10
*
   88 CONTINUE
      IF(JNX.GT.JMX) GOTO 89
      IF(STRN(JNX:JNX).NE.' ') GOTO 89
      JNX=JNX+1
      GOTO 88
   89 CONTINUE
*
      IF(JNX.GT.JMX) IFNX=0
*
*    ============ Stop =================
   90 CONTINUE
*
      JPAK=((JTX*200+JNM)*200+JNX)*200+JMX
*
      IF(JEP.GT.0) PRFX=STRN(JTX:JEP)
      IF(PRFX.NE.' ') IFPR=1
*
      IFTX=IFNM*8+IFPR*4+IFNX
*
      IF(ISN.NE.0) THEN
*      Is negative
      INUM=-INUM
      FNUM=-FNUM
      ENDIF
*
      IF(IFNM.EQ.1.OR.IFNM.EQ.2) FNUM=INUM
*
      END
