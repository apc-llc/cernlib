*
* $Id: cstor2.inc,v 1.1.1.1 1996/02/28 16:23:44 mclareni Exp $
*
* $Log: cstor2.inc,v $
* Revision 1.1.1.1  1996/02/28 16:23:44  mclareni
* Hepdb, cdlib, etc
*
*
*
* cstor2.inc
*
      COMMON /GCBANK/ FENCCS(22), LQ(9)
      DIMENSION       IQ(2), Q(2)
      EQUIVALENCE     (IQ(1),Q(1),LQ(9))
*
      COMMON /QUEST/  IQUEST(100)
*
