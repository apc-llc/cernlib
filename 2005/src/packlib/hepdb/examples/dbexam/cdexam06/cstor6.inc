*
* $Id: cstor6.inc,v 1.1.1.1 1996/02/28 16:23:49 mclareni Exp $
*
* $Log: cstor6.inc,v $
* Revision 1.1.1.1  1996/02/28 16:23:49  mclareni
* Hepdb, cdlib, etc
*
*
*
* cstor6.inc
*
      COMMON /GCBANK/ FENCCS(22), LQ(9)
      DIMENSION       IQ(2), Q(2)
      EQUIVALENCE     (IQ(1),Q(1),LQ(9))
*
      COMMON /QUEST/  IQUEST(100)
*
