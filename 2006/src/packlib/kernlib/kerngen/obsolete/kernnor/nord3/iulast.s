*
* $Id: iulast.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iulast.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IULAST
%
% CERN PROGLIB# V305    IULAST          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% JX = IULAST(CM,AVM,N)         LAST WORD IN AV NOT CONTAINING C
%
       EXPORT IULAST
       ROUTINE IULAST
       LIB IULAST
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
IULAST:  ENTF VBAS
       W1:=IND(B.PAR+8)
       IF=GO OUT
       W1-1
       W3:=IND(B.PAR)
BEGLP: W3 COMP IND(B.PAR+4)(R1)
       IF><GO DIFF
       W LOOPD R1,0,BEGLP
       W1:=0
OUT:   RET
DIFF:  W1+1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IULAST
#undef CERNLIB_TCGEN_IULAST
#endif
