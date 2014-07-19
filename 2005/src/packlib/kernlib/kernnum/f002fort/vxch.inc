*
* $Id: vxch.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vxch.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vxch.inc
*
          LXJ  =  1
          LYJ  =  1
          DO 10     J  =  1, N
             T       =  X(LXJ)
             X(LXJ)  =  Y(LYJ)
             Y(LYJ)  =  T
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
  10      CONTINUE
          RETURN
          END
