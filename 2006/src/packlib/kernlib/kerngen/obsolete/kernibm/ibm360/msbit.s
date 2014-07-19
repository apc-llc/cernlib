*
* $Id: msbit.s,v 1.1.1.1 1996/02/15 17:53:15 mclareni Exp $
*
* $Log: msbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:15  mclareni
* Kernlib
*
*
MSBIT    CSECT
*
* CERN PROGLIB# M421    MSBIT           .VERSION KERNIBM  2.14  851016
* ORIG.  SEPT 85, HRR
*
#if defined(CERNLIB_QMIBMXA)
*        SPLEVEL  SET=2
MSBIT    AMODE ANY
MSBIT    RMODE ANY
#endif
         USING *,15
         B     BEGIN
         DC    X'4'
         DC    CL7'MSBIT  '
BEGIN    STM   2,5,28(13)
         LM    2,4,0(1)                 R2->A,R3->X,R4->LX
         L     4,0(4)                   R4=LX
         LTR   4,4
         BNP   RET2                     RETURN IF LX.LE.0
         L     2,0(2)
         LA    5,1
         NR    5,2                      R5 HAS SAME LOW ORDER BIT AS A
         LTR   5,5
         BZ    BIT0
BIT1     SLL   5,63(4)
         O     5,0(3)
         B     RET
BIT0     LA    5,1
         SLL   5,63(4)
         X     5,FF
         N     5,0(3)
RET      LR    0,5
RET2     LM    2,5,28(13)
         MVI   12(13),X'FF'
         SR    15,15
         BR    14
FF       DC    F'-1'
         END
#ifdef CERNLIB_TCGEN_MSBIT
#undef CERNLIB_TCGEN_MSBIT
#endif
