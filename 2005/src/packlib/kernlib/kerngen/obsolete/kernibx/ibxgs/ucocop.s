*
* $Id: ucocop.s,v 1.1.1.1 1996/02/15 17:54:42 mclareni Exp $
*
* $Log: ucocop.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:42  mclareni
* Kernlib
*
*
UCOCOP   CSECT
*
* CERN PROGLIB# V302    UCOCOP          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _ucocop_
_ucocop_ STM   2,1,16(13)
         LM    6,7,88(13)     AIX: addresses of arg5=NA,arg6=NB
         L     4,0(,2)        AIX: value of arg3 = IDO
         LTR   4,4
         BZ    RETURN
         L     5,0(,3)        AIX: value of arg4 = IW
         LTR   5,5
         BZ    RETURN
         LR    3,1            AIX: address of arg2
         LR    2,0            AIX: address of arg1
         L     9,0(7)              NB
         L     7,0(6)              NA
         SR    7,5                 KK
         SR    9,5                 LL
         SR    6,6
         SR    8,8
         LA    10,4
         MR    6,10
         MR    8,10
         SR    6,6
         SR    8,8
         CNOP  0,8
LOOP1    LR    11,5
LOOP2    L     12,0(2,6)
         ST    12,0(3,8)
         AR    6,10
         AR    8,10
         BCT   11,LOOP2
         AR    6,7
         AR    8,9
         BCT   4,LOOP1
RETURN   LM    2,12,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_UCOCOP
#undef CERNLIB_TCGEN_UCOCOP
#endif
