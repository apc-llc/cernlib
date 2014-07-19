*
* $Id: mdpool.inc,v 1.1.1.1 1996/02/26 17:16:34 mclareni Exp $
*
* $Log: mdpool.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:34  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_MDPOOL_INC
#define CERNLIB_COMIS_MDPOOL_INC
*
*
* mdpool.inc
*
      COMMON/MDPOOL/IQ(LHP)
      INTEGER ISEM(LHP),KD(LHP),IDA(LHP),KD1(99),KD2(99),KD3(99)
      EQUIVALENCE (IQ,ISEM),(IQ,KD),(IQ,IDA)
        EQUIVALENCE (KD1(1),KD(2)),(KD2(1),KD(3)),(KD3(1),KD(4))

#endif
