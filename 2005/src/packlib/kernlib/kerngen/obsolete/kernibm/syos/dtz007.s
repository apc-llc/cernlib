*
* $Id: dtz007.s,v 1.1.1.1 1996/02/15 17:53:21 mclareni Exp $
*
* $Log: dtz007.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:21  mclareni
* Kernlib
*
*
DTZ007   CSECT
*
* CERN PROGLIB# Z007    DTZ007          .VERSION KERNIBM  1.11  820328
*
*     CALL DTZ007 (DATIME) RETURNS THE BCD DATE/TIME
*     IN THE 16-BYTE VECTOR DATIME THUS  DD/MM/YYHH.MM.SS
*                  HAMBURG, AUGUST 1973  (H.BUTENSCHOEN)
*                  JAN 82 : CHANGED ENTRY-NAME 'DATE' TO 'DTZ007'
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
DTZ007   AMODE ANY
DTZ007   RMODE ANY
#endif
         STM      14,6,12(13)
         BALR  2,0
         USING *,2
         L     4,0(1)
         LA    6,8(4)
         LA    1,2(0,0) LOAD REG 1 TO SPECIFY UNIT
         SVC   11 ISSUE TIME SVC
         ST    1,ZW2
         CLC   ZW2+2(2),SIGN+2
         BC    8,NULL
         UNPK  DECMAL(8),ZW2(4)
         MVC   6(2,4),DECMAL+3
         MVI   5(4),X'61'
         MVI   2(4),X'61'
         N     1,NOSIGN
         O     1,SIGN
         STH   1,DAY2+2
         SRL   1,12
         N     1,NOSIGN
         O     1,SIGN
         MVI   YEAR,X'00'
         STH   1,YEAR+1
         DP    YEAR(3),FOUR(1)
         CP    YEAR+2(1),SIGN+3(1)
         BC    7,NOSW
         CP    DAY2+2(2),SIXTY(2)
         BC    4,NOSW
         BC    8,FEB29
         SP    DAY2+2(2),ONE(1)
NOSW     ZAP   DAY1(10),DAY2(4)
         DP    DAY1(10),THIRTY(2)
         CVB   3,DAY1
         LR    5,3
         SLL   5,1
         LH    5,TABEL(5)
         CH    5,DAY2+2
         BC    10,SETDAY
         LA    3,1(3)
SETDAY   LR    5,3
         S     5,RONE
         CVD   3,DAY1
         UNPK  DECMAL(8),DAY1+6(2)
         OI    DECMAL+7,X'F0'
         MVC   3(2,4),DECMAL+6
         SLL   5,1
         LH    5,TABEL(5)
         STH   5,ZW
         SP    DAY2+2(2),ZW(2)
         LH    5,DAY2+2
         ST    5,ZW2
         UNPK  DECMAL(8),ZW2(4)
         OI    DECMAL+7,X'F0'
         MVC   0(2,4),DECMAL+6
END      ST    0,ZW2
         UNPK  DECMAL(8),ZW2(4)
         MVC   0(2,6),DECMAL+1
         MVI   2(6),X'4B'
         MVC   3(2,6),DECMAL+3
         MVI   5(6),X'4B'
         MVC   6(2,6),DECMAL+5
         LM    2,6,28(13)
         MVI   12(13),X'FF'
         BCR   15,14
FEB29    MVC   0(5,4),SONDAY
         BC    15,END
NULL     MVC   0(8,4),NULLD
         B     END
NULLD    DC    C'00/00/00'
DECMAL   DS    1D
DAY1     DS    1D
         DS    1H
         DS    0F
RONE     DC    FL4'1'
ZW2      DS    1F
NOSIGN   DC    X'00FFFFF0'
SIGN     DC    X'0000000C'
DAY2     DC    X'00000000'
         DC    X'00'
YEAR     DC    X'000000'
FOUR     DC    X'4C'
SIXTY    DC    X'060C'
ONE      DC    X'1C'
THIRTY   DC    X'030C'
ZW       DS    1H
TABEL    DC    X'000C'
         DC    X'031C'
         DC    X'059C'
         DC    X'090C'
         DC    X'120C'
         DC    X'151C'
         DC    X'181C'
         DC    X'212C'
         DC    X'243C'
         DC    X'273C'
         DC    X'304C'
         DC    X'334C'
         DC    X'400C'
SONDAY   DC    X'F2F961F0F2'
         END
