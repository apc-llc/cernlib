*
* $Id: iunext.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: iunext.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
IUNEXT   CSECT
*
* CERN PROGLIB# M410    IUNEXT          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _iunext_
_iunext_ STM   2,7,16(13)
         LR    2,0                AIX: address of arg1
         L     7,0(,1)            AIX: value of arg2
         LR    3,7
         LA    4,4
         LA    1,1
         SR    6,6
         MR    6,4
         SR    2,4
         L     5,BLANKS
ST10     L     6,0(2,7)
         CR    5,6
         BNE   ST12
         AR    7,4
         AR    3,1
         B     ST10
ST12     LR    0,3
         LM    2,7,16(13)
         BCR   15,14
         CNOP  0,4
BLANKS   DC    CL4'    '
         END
#ifdef CERNLIB_TCGEN_IUNEXT
#undef CERNLIB_TCGEN_IUNEXT
#endif
