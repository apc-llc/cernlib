*
* $Id: texnme.F,v 1.1.1.1 1996/02/28 16:23:42 mclareni Exp $
*
* $Log: texnme.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:42  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE TEXNME(STRN,JPAK,IFTX,NAME)
*     ---15.09.88------------****-====-====-
*
      CHARACTER*(*) STRN,NAME
*
      EQUIVALENCE(IN,FN)
*
      CHARACTER*1 CH,C2,C3
*
*   Get info from JPAK
      JNX=MAX0(MOD(JPAK/200,200),1)
      JMX=MOD(JPAK,200)
      IF(JMX.EQ.0) JMX=MIN0(LEN(STRN),199)
*
*   Init pointers & prefix
      JTX=0
      JNM=0
      NAME=' '
      JEP=0
*
*    Init IFTX components
      IFNM=0
      IFPR=0
      IFNX=0
*
*   Return with IFTX=0 if string is already finished
      IF(JNX.GT.JMX) GOTO 90
*   Else go on
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
        IF(JTX.LE.0) THEN
*          Something is encountered
          JTX=J
          IFTX=0
        ENDIF
        JEP=J
*
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
      JPAK=JTX*8000000+JNX*200+JMX
*
      IF(JEP.GT.0) NAME=STRN(JTX:JEP)
      IF(NAME.NE.' ') IFPR=1
*
      IFTX=IFPR*4+IFNX
*
      END
