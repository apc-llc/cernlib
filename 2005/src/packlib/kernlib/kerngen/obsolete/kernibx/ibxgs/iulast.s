*
* $Id: iulast.s,v 1.1.1.1 1996/02/15 17:54:37 mclareni Exp $
*
* $Log: iulast.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:37  mclareni
* Kernlib
*
*
IULAST   CSECT
*
* CERN PROGLIB# V305    IULAST          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _iulast_
_iulast_ STM   2,8,16(13)
         L     5,0(,2)            AIX: value of arg2
         LR    2,0                AIX: address of arg1
         L     2,0(2)
         SR    4,4
         LTR   6,5
         BZ    ST20
         LA    7,4
         MR    4,7
ST12     SR    5,7
         L     8,0(1,5)           AIX: arg2 address in GR1
         CR    2,8
         BNE   ST20
         BCT   6,ST12
ST20     LR    0,6
         LM    2,8,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_IULAST
#undef CERNLIB_TCGEN_IULAST
#endif
