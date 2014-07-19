*
* $Id: mbil.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mbil.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mbil.inc
*
          LXI  =  1
          LYI  =  1
          DO 12     I  =  1, N
             SYZ   =  ZERO
             LYIJ  =  LYI
             LZJ   =  1
             DO 11  J  =  1, N
                SYZ   =  F(Y(LYIJ),Z(LZJ),SYZ)
                LYIJ  =  LYIJ + JY
                LZJ   =  LZJ + JZ
  11            CONTINUE
             SXYZ  =  G(SYZ,X(LXI),SXYZ)
             LXI   =  LXI + IX
             LYI   =  LYI + IY
  12         CONTINUE
