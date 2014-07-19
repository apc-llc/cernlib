*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
IUFIND   CSECT
*
* CERN PROGLIB# V304    IUFIND          .VERSION KERNIBM  0.1   741009
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUFIND   AMODE ANY
IUFIND   RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'6'
         DC    CL6'IUFIND'
BEGIN    STM   1,6,24(13)
         LM    1,4,0(1)                 R1->IT,R2->IVEC,R3->JL,R4->JR
         L     3,0(3)
         L     4,0(4)
         LA    0,1(4)                   R0=JR+1
         BCTR  4,0
         SLL   4,2                      R4=4*(JR-1)
         LA    5,0(4,2)
         LR    6,2                      SAVE ADDRESS OF IVEC
         BCTR  3,0
         SLL   3,2                      R3=4*(JL-1)
         LA    2,0(2,3)                 R2->IVEC(JL)
         SR    4,3
         BM    RET
         L     1,0(1)
         LA    4,4
LOOP     C     1,0(2)
         BE    FOUND
         BXLE  2,4,LOOP
RET      LM    1,6,24(13)
         SR    15,15
         MVI   12(13),X'FF'
         BR    14
FOUND    SR    2,6
         SRL   2,2
         LA    2,1(2)
         LR    0,2
         B     RET
         END
#ifdef CERNLIB_TCGEN_IUFIND
#undef CERNLIB_TCGEN_IUFIND
#endif
