*
* $Id: vmula.inc,v 1.1.1.1 1996/02/15 17:48:51 mclareni Exp $
*
* $Log: vmula.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:51  mclareni
* Kernlib
*
*
*
* vmula.inc
*
          LXJ  =  1
          LYJ  =  1
          LZJ  =  1
          DO 10     J  =  1, N
             Z(LZJ)  =  MULA( X(LXJ),Y(LYJ),Z(LZJ) )
             LXJ     =  LXJ + JX
             LYJ     =  LYJ + JY
             LZJ     =  LZJ + JZ
  10      CONTINUE
          RETURN
          END
