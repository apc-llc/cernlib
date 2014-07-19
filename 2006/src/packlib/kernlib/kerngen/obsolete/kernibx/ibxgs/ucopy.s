*
* $Id: ucopy.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ucopy.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UCOPY    CSECT
*
* CERN PROGLIB# V301    UCOPY           .VERSION KERNIBX  1.01  900524
*
*     CALL UCOPY  (A,B,N)
*     CALL UCOPY2 (A,B,N)
*
*        USES THE MVCL INSTR.
*        MVCL CHECKS ON DESTRUCTIVE OVERLAP OF A AND B
*        ON THAT CONDITION (3), A REVERSE LOOP IS ENTERED.
*        P. M. HEINEN NIJMEGEN SEPT. 73
*        Modified for AIX, Roger Howard, January 1990
*
         USING *,15
         ENTRY _ucopy_
         ENTRY _ucopy2_
_ucopy_  DS    0H
_ucopy2_ STM   2,5,16(13)
         LR    3,0          AIX: address of arg1 = A
         LR    4,1          AIX: address of arg2 = B
         L     5,0(,2)      AIX: value of arg3 = N
         LPR   5,5
         BC    8,MVR
         SLL   5,2          N
         LR    2,4          B
         LR    4,3          A
         LR    3,5          N
         MVCL  2,4
         BC    1,MV10  DESTR. OVERLAP
MVR      LM    2,5,16(13)
         BR    14
*        REVERSE LOOP / A(N) TO B(N), A(N-1) TO B(N-1) , ETC.
MV10     LA    3,4
         LA    2,0(5,2)     B(N+1)
         LA    4,0(5,4)     A(N+1)
         SRL   5,2          N
MV12     SR    2,3          B(N)
         SR    4,3          A(N)
         MVC   0(4,2),0(4)  A(N) TO B(N)
         BCT   5,MV12
         B     MVR
         END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
