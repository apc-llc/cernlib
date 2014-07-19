*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:53:13 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:13  mclareni
* Kernlib
*
*
IUCOMP   CSECT
*
* CERN PROGLIB# V304    IUCOMP          .VERSION KERNIBM  0.1   741009
*
*        CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
IUCOMP   AMODE ANY
IUCOMP   RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'6'
         DC    CL6'IUCOMP'
BEGIN    STM   1,6,24(13)
         LM    1,3,0(1)                 R1->IT,R2->IVEC,R3->N
         L     3,0(3)
         LTR   3,3
         BNP   ZERO
         L     1,0(1)                   R1=IT
         LA    4,4(0)                   R4=4
         LR    6,2                      SAVE ADDRESS OF IVEC
         SLL   3,2
         SR    3,4
         LA    5,0(3,2)                 R5->IVEC(N)
LOOP     C     1,0(2)
         BE    FOUND
         BXLE  2,4,LOOP
ZERO     SR    0,0
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
#ifdef CERNLIB_TCGEN_IUCOMP
#undef CERNLIB_TCGEN_IUCOMP
#endif
