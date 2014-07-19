*
* $Id: hwdtop.F,v 1.1.1.1 1996/03/08 17:02:11 mclareni Exp $
*
* $Log: hwdtop.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:11  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.46  by  Unknown
*-- Author :
CDECK  ID>, HWDTOP.
*CMZ :-        -09/12/92  11.03.46  by  Bryan Webber
*-- Author :    Bryan Webber
C------------------------------------------------------------------------
      SUBROUTINE HWDTOP(DECAY)
C     DECIDES WHETHER TO DO TOP QUARK
C     DECAY BEFORE HADRONIZATION
#include "herwig58/herwig58.inc"
      LOGICAL DECAY
      DECAY=RMASS(6).GT.130D0
      END
