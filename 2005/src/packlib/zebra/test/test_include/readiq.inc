*
* $Id: readiq.inc,v 1.1.1.1 1996/03/06 10:47:03 mclareni Exp $
*
* $Log: readiq.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:03  mclareni
* Zebra
*
*
*
* readiq.inc
*
#include "test_include/rspace.inc"
      COMMON //    NO(40), IFLXQ(10), IXSTOR, IXHOLD
     +,            FENCE(10), LQUSER(7), LQMAIN
     +,            LA1, LA2, LA3, LA4, LA5, LA6
     +,            LX1, LX2, LX3, LX4, LX5, LX6
     +,            M(100), DLAST
                     DIMENSION    LQ(NQN), IQ(NQN), Q(NQN)
                     EQUIVALENCE (LQ(1),LQUSER(1)),(IQ(1),Q(1),LQ(9))
