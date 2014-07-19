*
* $Id: vdiv.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vdiv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vdiv.inc
*
          LXJ  =  1
          LYJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             IF(REALF(Y(LYJ)) .EQ. 0.)  GOTO 20
             Z(LZJ)  =  X(LXJ) / Y(LYJ)
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
             LZJ     =  LZJ + JZ
  10      CONTINUE
          J  =  0
  20      IFAIL  =  J
          RETURN
          END
