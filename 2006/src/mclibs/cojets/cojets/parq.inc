*
* $Id: parq.inc,v 1.1.1.1 1996/01/11 14:14:54 mclareni Exp $
*
* $Log: parq.inc,v $
* Revision 1.1.1.1  1996/01/11 14:14:54  mclareni
* Cojets
*
*
#ifndef CERNLIB_COJETS_PARQ_INC
#define CERNLIB_COJETS_PARQ_INC
*
*
* parq.inc
*
      PARAMETER (MXINQ=150)
      COMMON/PARQ/PARACT(MXINQ,9,2),PARSPE(MXINQ,6,2),SHARD
     1,IBEAM,NBOOK(2),JBOOK,MBOOK(2),MXSPE
      COMMON/CANC/ICANC(2),NBKCNC(MXINQ,2)
      COMMON/NENT2/NENT2(MXINQ)

#endif
