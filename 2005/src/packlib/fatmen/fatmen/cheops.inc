*
* $Id: cheops.inc,v 1.1.1.1 1996/03/07 15:17:52 mclareni Exp $
*
* $Log: cheops.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:52  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_CHEOPS_INC
#define CERNLIB_FATMEN_CHEOPS_INC
*
*
* cheops.inc
*
*
*     CHEOPS error messages
*
      PARAMETER    (NKEOPS=5)
      CHARACTER*80 MKEOPS(NKEOPS)
      DATA         MKEOPS(1)/'Inquire on dataset failed'/,
     +             MKEOPS(2)/'Dataset not regular'/,
     +             MKEOPS(3)/'Cannot open dataset'/,
     +             MKEOPS(4)/'Cannot close dataset'/,
     +             MKEOPS(5)/'Transaction expired'/
*define CC_OK                   0
*define CC_STATONDATASETFAILS   1
*define CC_DATASETNOTREGULAR    2
*define CC_CANNOTOPENDATASET    3
*define CC_CANNOTCLOSEDATASET   4
*define CC_TRANSACTIONEXPIRED   5

#endif
