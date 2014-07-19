*
* $Id: uswop.s,v 1.1.1.1 1996/02/15 17:54:43 mclareni Exp $
*
* $Log: uswop.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:43  mclareni
* Kernlib
*
*
USWOP    CSECT
*
* CERN PROGLIB# V303    USWOP           .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _uswop_
_uswop_  STM   2,8,16(13)
         L     4,0(,2)          AIX: value of arg3
         LR    2,0              AIX: address of arg1
         LTR   4,4
         BZ    RETURN
         LA    6,4
         SR    5,5
NOCHMAL  L     7,0(2,5)
         L     8,0(1,5)         AIX: address of arg2 in GR1
         ST    7,0(1,5)
         ST    8,0(2,5)
         AR    5,6
         BCT   4,NOCHMAL
RETURN   DS    0H
         LM    2,8,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_USWOP
#undef CERNLIB_TCGEN_USWOP
#endif
