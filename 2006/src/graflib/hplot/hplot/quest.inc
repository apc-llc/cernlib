*
* $Id: quest.inc,v 1.1.1.1 1996/01/19 10:50:14 mclareni Exp $
*
* $Log: quest.inc,v $
* Revision 1.1.1.1  1996/01/19 10:50:14  mclareni
* Hplot
*
*
#ifndef CERNLIB_HPLOT_QUEST_INC
#define CERNLIB_HPLOT_QUEST_INC
*
*
* quest.inc
*
*CMZ :  5.16/00 20/04/93  13.13.43  by  O.Couet
*-- Author :
      COMMON /QUEST/IQUEST(100)
      DIMENSION RQUEST(100)
      EQUIVALENCE(IQUEST(1),RQUEST(1))

#endif
