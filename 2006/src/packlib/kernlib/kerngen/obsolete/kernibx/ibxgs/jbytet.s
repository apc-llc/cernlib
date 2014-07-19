*
* $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:54:40 mclareni Exp $
*
* $Log: jbytet.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:40  mclareni
* Kernlib
*
*
JBYTET   CSECT
*
* CERN PROGLIB# M421    JBYTET          .VERSION KERNIBX  1.01  900524
*
         USING *,15
         ENTRY _jbytet_
_jbytet_ STM   2,4,16(13)
         LR    4,0              AIX: address or arg1
         L     0,0(,4)          AIX: value of arg1
         L     1,0(,1)          AIX: value of arg2
         L     2,0(,2)          AIX: value of arg3
         L     3,0(,3)          AIX: value of arg4
         BCTR  2,0
         SRL   1,0(2)
         LA    4,32
         SR    4,3
         SLL   0,0(4)
         SRL   0,0(4)
         NR    0,1
         LM    2,4,16(13)
         BCR   15,14
         END
#ifdef CERNLIB_TCGEN_JBYTET
#undef CERNLIB_TCGEN_JBYTET
#endif
