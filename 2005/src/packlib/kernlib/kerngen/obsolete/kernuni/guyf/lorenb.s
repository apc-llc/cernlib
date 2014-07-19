*
* $Id: lorenb.s,v 1.1.1.1 1996/02/15 17:54:03 mclareni Exp $
*
* $Log: lorenb.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:03  mclareni
* Kernlib
*
*
. **************************************
.
.     SUBROUTINE LORENB( U, PS, PI, PF )         *** U102 ***
. *   NOT PROTECTED AGAINST FO OR DF
.
. **************************************
 
          AXR$
          FORT$
$(1)
LORENB*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A3,0,XARG          . LOC(U)
          DL        A1,1,XARG          . LOC(PS), LOC(PI)
          DS        A6,R2              . SAVE A6,A7
          L         R1,0,A3            . U
          L         A3,0,A1            . PS(1)
          DL        A5,1,A2            . PI(2), PI(3)
          FM        A3,0,A2            . PS(1)*PI(1)
          FM        A6,2,A1            . PI(3)*PS(3)
          FA        A3,A6
          FM        A5,1,A1            . PI(2)*PS(2)
          L         A0,3,XARG          . LOC(PF)
          L         A6,3,A1            . PS(4)
          FA        A3,A5
          TNE       A6,R1
          J         LOR4               . IF( PS(4).EQ.U )
          FM        A6,3,A2            . PS(4)*PI(4)
          L         A5,3,A1            . PS(4)
          FA        A3,A6
          FA        A5,R1              . PS(4) + U
          FD        A3,R1              . PF4
          SA        A3,3,A0            . PF(4)
          FA        A3,3,A2            . PF4 + PI(4)
          L         A6,2,A1            . PS(3)
          FD        A3,A5              . FN
          L         A4,0,A1            . PS(1)
          FM        A6,A3              . PS(3)*FN
          FM        A4,A3              . PS(1)*FN
          FA        A6,2,A2            . PF(3)
          FA        A4,0,A2            . PF(1)
          S         A6,2,A0
          L         A5,1,A1            . PS(2)
          FM        A5,A3              . PS(2)*FN
          FA        A5,1,A2            . PF(2)
LOR2
          DL        A6,R2
          DS        A4,0,A0
          J         RETUR4
LOR4                                   . IF( PS(4).EQ.U )
          DL        A6,2,A1
          DL        A4,0,A1
          DS        A6,2,A0
          J         LOR2
          END
#ifdef CERNLIB_TCGEN_LORENB
#undef CERNLIB_TCGEN_LORENB
#endif
