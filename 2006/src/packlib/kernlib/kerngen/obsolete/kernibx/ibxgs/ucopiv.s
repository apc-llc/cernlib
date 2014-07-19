*
* $Id: ucopiv.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ucopiv.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UCOPIV   CSECT
*
* CERN PROGLIB# V301    UCOPIV          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _ucopiv_
_ucopiv_ STM   2,7,16(13)
         L     5,0(,2)         AIX: value of arg3 = N
         LTR   5,5
         BC    8,RETURN
         LR    2,0             AIX: address of arg1 = A
         LA    4,4(0)
         SLA   5,2
         SR    5,4
         SR    6,6
         LA    7,COPY
COPY     LE    0,0(6,2)
         STE   0,0(5,1)        AIX: address of arg2 in GR1
         LA    6,4(6)
         SR    5,4
         BCR   10,7
RETURN   LM    2,7,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_UCOPIV
#undef CERNLIB_TCGEN_UCOPIV
#endif
