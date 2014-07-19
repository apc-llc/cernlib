*
* $Id: quest.inc,v 1.2 1996/03/01 16:50:30 cernlib Exp $
*
* $Log: quest.inc,v $
* Revision 1.2  1996/03/01 16:50:30  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_QUEST_INC
#define CERNLIB_PAW_QUEST_INC
*
*
* quest.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      COMMON /QUEST/IQUEST(100)
      DIMENSION RQUEST(100)
      EQUIVALENCE(IQUEST(1),RQUEST(1))
*

#endif
