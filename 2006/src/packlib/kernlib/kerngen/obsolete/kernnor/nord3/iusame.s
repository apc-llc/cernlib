*
* $Id: iusame.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iusame.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUSAME
%
% CERN PROGLIB# M501    IUSAME          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% NX = IUSAME(AVM,JL,JR,MIN,JXS)  FIND STRING OF MIN SAME ELEMENTS
%
       EXPORT IUSAME
       ROUTINE IUSAME
       LIB IUSAME
VBAS:  STACK FIXED
PAR:   W BLOCK 5
MIN:   W BLOCK 1
       ENDSTACK
IUSAME:  ENTF VBAS
       W1:=IND(B.PAR+4:B)
       W1-1
       W2:=IND(B.PAR+8:B)
       W MOVE IND(B.PAR+12:B),B.MIN:S
RSTRT: W4:=1
       W3:=IND(B.PAR:B)(R1)
BEGLP: W1+1
       W1 COMP R2
       IF=GO AFTLP
       W3 COMP IND(B.PAR:B)(R1)
       IF><GO DIFF
       W4+1
       GO BEGLP
DIFF:  W4 COMP B.MIN:S
       IF>=GO OUT
       GO RSTRT
AFTLP: W4 COMP B.MIN:S
       IF>=GO OUT
       W1+1
       W1=:IND(B.PAR+16:B)
       W1:=0
       RET
OUT:   W1+1
       W SUB3 R1,R4,IND(B.PAR+16:B)
       W1:=W4
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUSAME
#undef CERNLIB_TCGEN_IUSAME
#endif
