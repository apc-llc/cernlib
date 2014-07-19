*
* $Id: cstor8.inc,v 1.1.1.1 1996/02/28 16:23:51 mclareni Exp $
*
* $Log: cstor8.inc,v $
* Revision 1.1.1.1  1996/02/28 16:23:51  mclareni
* Hepdb, cdlib, etc
*
*
*
* cstor8.inc
*
      COMMON /CSTOR8/ FENCCS(22), LQ(9)
      DIMENSION       IQ(2), Q(2)
      EQUIVALENCE     (IQ(1),Q(1),LQ(9))
*
      COMMON /QUEST/  IQUEST(100)
*
