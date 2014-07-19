*
* $Id: hexenc.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: hexenc.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE HEXENC(NUMB,LENG,CODE,LRES)
*     ----------------------------====-====-
      CHARACTER CODE*(*)
      CHARACTER CC*8
*
      IS0=ICHAR('0')
      ISA=ICHAR('A')-10
*
      LL=IABS(LENG)
      IF(LL.EQ.0) LL=8
      LL=MIN0(LL,LEN(CODE),8)
      JH=0
      JL=0
      JB=1
      DO J=8,1,-1
        ID=JBYT(NUMB,JB,4)
        IF(ID.LE.9) THEN
          CC(J:J)=CHAR(IS0+ID)
        ELSE
          CC(J:J)=CHAR(ISA+ID)
        ENDIF
        IF(ID.GT.0) THEN
          IF(JH.LE.0) JL=J
          JH=J
        ENDIF
        JB=JB+4
      ENDDO
      IF(JH.LE.0) JH=8
      IF(JL.LE.0) JL=1
      CODE=' '
      IF(LENG.GE.0) THEN
        IF(LENG.EQ.0) LL=MIN0(LL,9-JH)
        JA=9-LL
        JC=MAX0(LEN(CODE)-LL+1,1)
        CODE(JC:)=CC(JA:)
        IF(JH.LT.JA.AND.JC.GT.1) THEN
          CODE(JC-1:JC-1)='~'
          LL=LL+1
        ENDIF
      ELSE
        CODE(:LL)=CC
        IF(LL.LT.JL.AND.LEN(CODE).GT.LL) THEN
          LL=LL+1
          CODE(LL:LL)='~'
        ENDIF
      ENDIF
      LRES=LL
      END
*
      FUNCTION IHEXAN(CODE,LCOD)
*     --------------------------
      CHARACTER CODE*(*)
*
      CHARACTER*1 C
*
      IS0=ICHAR('0')
      ISA=ICHAR('A')-10
*
      NN=0
      JN=0
*
      DO 111 JC=1,LCOD
      C=CODE(JC:JC)
      N=-1
      IF(C.GE.'0'.AND.C.LE.'9') N=ICHAR(C)-IS0
      IF(C.GE.'A'.AND.C.LE.'F') N=ICHAR(C)-ISA
      IF(N.GE.0) THEN
        JN=JN+1
        NN=ISHFT(NN,4)+N
      ELSE IF(JN.GT.0) THEN
        GOTO 60
      ENDIF
  111 CONTINUE
*
   60 CONTINUE
      IHEXAN=NN
      END
