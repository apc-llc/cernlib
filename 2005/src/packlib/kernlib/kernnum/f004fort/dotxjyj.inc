*
* $Id: dotxjyj.inc,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: dotxjyj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
*
* dotxjyj.inc
*
                DO 11 J  =  1, N
                   SUM   =  DOTF(X(LXIJ),Y(LYJL),SUM)
                   LXIJ  =  LXIJ + JX
                   LYJL  =  LYJL + JY
  11               CONTINUE
