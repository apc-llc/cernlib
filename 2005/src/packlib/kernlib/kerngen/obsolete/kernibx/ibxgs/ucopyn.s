*
* $Id: ucopyn.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ucopyn.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UCOPYN   CSECT
*
* CERN PROGLIB# V301    UCOPYN          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _ucopyn_
_ucopyn_ STM   2,7,16(13)
         L     5,0(,2)                 AIX: value of arg3
         LR    2,0                     AIX: address of arg1
         LTR   5,5
         BC    8,RETURN
         LA    4,4(0)
         SLA   5,2
         SR    5,4
         SR    6,6
COPY     L     0,0(6,2)
         LCR   0,0
         ST    0,0(6,1)               AIX: arg2 address in GR1
         BXLE  6,4,COPY
RETURN   LM    2,7,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_UCOPYN
#undef CERNLIB_TCGEN_UCOPYN
#endif
