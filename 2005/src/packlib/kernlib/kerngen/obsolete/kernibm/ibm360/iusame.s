*
* $Id: iusame.s,v 1.1.1.1 1996/02/15 17:53:14 mclareni Exp $
*
* $Log: iusame.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:14  mclareni
* Kernlib
*
*
IUSAME   CSECT
*
* CERN PROGLIB# M501    IUSAME          .VERSION KERNIBM  0.1   720321
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUSAME   AMODE ANY
IUSAME   RMODE ANY
#endif
         ENTRY IUSAME                  IUS = IUSAME(IA,JST,N,MIN,JSAME)
         BC    15,12(15)
         DC    X'07'
         DC    CL7'IUSAME '
         STM   14,12,12(13)
         LR    11,15
         USING IUSAME,11
         ST    13,SA+4
         LR    12,13
         LA    13,SA
         ST    13,8(12)
         LM    2,6,0(1)
         L     3,0(3)
         BCTR  3,0
         SLA   3,2
         L     4,0(4)
         BCTR  4,0
         SLA   4,2
         L     5,0(5)
ZWEI1    SR    8,8
         LR    9,3
ZWEI4    LA    8,1(8)
         CR    3,4
         BE    DREI1
         L     7,0(3,2)
         LA    3,4(3)
         C     7,0(3,2)
         BE    ZWEI4
         CR    8,5
         BL    ZWEI1
         B     DREI2
DREI1    CR    8,5
         BNL   DREI2
         SR    8,8
         LA    9,4(4)
DREI2    LR    0,8
         SRA   9,2
         LA    9,1(9)
         ST    9,0(6)
         L     13,SA+4
         LM    14,15,12(13)
         LM    1,12,24(13)
         BCR   15,14
SA       DS    18F
         END
#ifdef CERNLIB_TCGEN_IUSAME
#undef CERNLIB_TCGEN_IUSAME
#endif
