*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
IUFIND   CSECT
*
* CERN PROGLIB# V304    IUFIND          .VERSION KERNIBX  1.01  900524
*
*    CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*    Modified for AIX, Roger Howard, January 1990
         USING *,15
         ENTRY _iufind_
_iufind_ STM   2,6,16(13)
         L     4,0(,3)                  AIX: value of arg4 = JR
         L     3,0(,2)                  AIX: value of arg3 = JL
         LR    2,1                      AIX: address of arg2 = IVEC
         LR    1,0                      AIX: address of arg1 = IT
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
RET      LM    2,6,16(13)
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
