*
* $Id: incbyt.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: incbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
INCBYT   CSECT
*
* CERN PROGLIB# M423    INCBYT          .VERSION KERNIBX  1.01  900523
*
*    CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*    Modified for AIX, Roger Howard, January 1990
         USING *,15
         ENTRY _incbyt_
_incbyt_ STM   2,9,16(13)
         LR    4,3             AIX: address of arg4 = MPACK
         L     7,0(,2)         AIX: value of arg3 = JX
         LR    2,1             AIX: address of arg2 = X
         LR    1,0
         L     1,0(1)          AIX: value of arg1 = INC
         SR    0,0
         L     3,0(4)          R3=NBITS
         LR    9,7             R9=JX
         BCTR  7,0             R7=JX-1
         SR    6,6
         D     6,4(4)          R7=JWORD-1
         LR    5,7
         SLL   5,2
         SR    6,6
         M     6,4(4)          R7=(JWORD-1)*INWORD
         SR    9,7             R9=JPOS
         SR    8,8
         MR    8,3
         L     6,0(2,5)        R6=X(JWORD)
         SRDL  6,0(9)
         SR    6,6
         SLDL  6,0(3)
         LR    7,6
         ALR   6,1
         C     6,8(4)
         BH    OVFL
GOBACK   SR    9,3
         SLL   1,0(9)
         AL    1,0(2,5)
         ST    1,0(2,5)
RETURN   LM    2,9,16(13)
         BR    14
OVFL     S     6,8(4)
         LR    0,6
         L     1,8(4)
         SR    1,7
         B     GOBACK
         END
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
