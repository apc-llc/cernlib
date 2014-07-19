*
* $Id: mset.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mset.inc
*
          MM  =  M
          NN  =  N
          IF(MM .GT. NN)  THEN
             MN  =  NN
             NN  =  MM
             MM  =  MN
             IJ  =  JZ
             JZ  =  IZ
             IZ  =  IJ
          ENDIF
          LZI1  =  1
          DO 12     I  =  1, MM
             LZIJ  =  LZI1
             DO 11  J  =  1, NN
                Z(LZIJ)  =  S
                LZIJ     =  LZIJ + JZ
  11            CONTINUE
             LZI1  =  LZI1 + IZ
  12         CONTINUE
          RETURN
          END
