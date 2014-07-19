*
* $Id: mutl.inc,v 1.1.1.1 1996/02/15 17:48:56 mclareni Exp $
*
* $Log: mutl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:56  mclareni
* Kernlib
*
*
*
* mutl.inc
*
          LXII  =  1
          DO 12     IP1  =  2, N
             LXIJ  =  LXII
             LXJI  =  LXII
             DO 11  J  =  IP1, N
                LXIJ  =  LXIJ + JX
                LXJI  =  LXJI + IX
                X(LXJI)  =  X(LXIJ)
  11            CONTINUE
             LXII  =  LXII + IX + JX
  12         CONTINUE
          RETURN
          END
