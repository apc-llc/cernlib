*
* $Id: mcpy.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mcpy.inc
*
          MM  =  M
          NN  =  N
          IF(MM .GT. NN)  THEN
             MN  =  NN
             NN  =  MM
             MM  =  MN
             IJ  =  JX
             JX  =  IX
             IX  =  IJ
             IJ  =  JZ
             JZ  =  IZ
             IZ  =  IJ
          ENDIF
          LXI1  =  1
          LZI1  =  1
          DO 12     I  =  1, MM
             LXIJ  =  LXI1
             LZIJ  =  LZI1
             DO 11     J  =  1, NN
                Z(LZIJ)  =  FUNCT( X(LXIJ) )
                LXIJ  =  LXIJ + JX
                LZIJ  =  LZIJ + JZ
  11         CONTINUE
             LXI1  =  LXI1 + IX
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
