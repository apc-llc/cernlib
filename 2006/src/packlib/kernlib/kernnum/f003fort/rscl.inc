*
* $Id: rscl.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: rscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* rscl.inc
*
          MM  =  M
          NN  =  N
          LDI   =  1
          LXI1  =  1
          LZI1  =  1
          DO 12     I  =  1, MM
             LXIJ  =  LXI1
             LZIJ  =  LZI1
             DO 11     J  =  1, NN
                Z(LZIJ)  =  D(LDI)*X(LXIJ)
                LXIJ  =  LXIJ + JX
                LZIJ  =  LZIJ + JZ
  11         CONTINUE
             LDI   =  LDI + ID
             LXI1  =  LXI1 + IX
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
