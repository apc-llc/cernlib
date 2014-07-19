*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
IUCOMP   CSECT
*
* CERN PROGLIB# V304    IUCOMP          .VERSION KERNIBX  1.01  900524
*
*   CODED FOR FAST EXECUTION       H.WATKINS,LIVERPOOL JULY 1974
*   Modified for AIX, Roger Howard, January 1990
         USING *,15
         ENTRY _iucomp_
_iucomp_ STM   2,6,16(13)
         L     3,0(,2)                  AIX: value of arg3 = N
         LR    2,1                      AIX: address of arg2 = IVEC
         LR    1,0                      AIX: address of arg1 = IT
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
RET      LM    2,6,16(13)
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
