*
* $Id: testiq.inc,v 1.1.1.1 1996/03/06 10:47:03 mclareni Exp $
*
* $Log: testiq.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:03  mclareni
* Zebra
*
*
*
* testiq.inc
*
#if !defined(CERNLIB_QMVDS)
                     DIMENSION    LQ(NQN), IQ(NQN), Q(NQN)
                     EQUIVALENCE (LQ(1),LQUSER(1)),(IQ(1),Q(1),LQ(9))
#else
                     DIMENSION    LQ(NQN), IQ(NQN), Q(NQN)
                     EQUIVALENCE (LQ(1),LQUSER(1))
                     EQUIVALENCE (IQ(1),LQ(9)), (Q(1),IQ(1))
*    ------   to span // in the MAIN programs    ---------------
#endif
