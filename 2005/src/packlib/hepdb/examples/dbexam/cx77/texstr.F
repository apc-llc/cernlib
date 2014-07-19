*
* $Id: texstr.F,v 1.1.1.1 1996/02/28 16:23:43 mclareni Exp $
*
* $Log: texstr.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:43  mclareni
* Hepdb, cdlib, etc
*
*
      SUBROUTINE TEXSTR(LINE,JPAK,IFTX,STRN)
*     ---10.03.94------------****-====-====-
*                       -- Author : Boris Khomenko
*       This routine selects any text limited
*       by blanks and/or commas within a LINE's
*       segment defined by pointers inside JPAK.
*       JPAK returns new values of pointers;
*       IFTX contains result flags;
*       STRN returns the found text or " ".
*
      IMPLICIT NONE
      CHARACTER*(*) LINE , STRN
      INTEGER       JPAK , IFTX
*
      INTEGER  IFPR , IFNX
      INTEGER  JTX  , JNX , JMX
      INTEGER  J    , JEP
*
      CHARACTER*1 CH
*
*   Get info from JPAK
      JNX=MAX0(MOD(JPAK/200,200),1)
      JMX=MOD(JPAK,200)
      IF(JMX.EQ.0) JMX=MIN0(LEN(LINE),199)
*
*   Init pointers & prefix
      JTX=0
      STRN=' '
      JEP=0
*
*    Init IFTX components (no number flag here)
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
      CH=LINE(J:J)
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
      IF(LINE(JNX:JNX).NE.' ') GOTO 89
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
      IF(JEP.GT.0) STRN=LINE(JTX:JEP)
      IF(STRN.NE.' ') IFPR=1
*
      IFTX=IFPR*4+IFNX
*
      END
