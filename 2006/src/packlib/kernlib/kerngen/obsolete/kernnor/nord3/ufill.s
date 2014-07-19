*
* $Id: ufill.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ufill.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UFILL
%
% CERN PROGLIB# V300    UFILL           .VERSION KERNNOR  1.05  810521
% ORIG.  H.OVERAS, CERN, 810330
%
% CALL UFILL(XVM,JL,JR,CM)      XVM(J)=CM
%
       EXPORT UFILL
       ROUTINE UFILL
       LIB UFILL
VBAS:  STACK FIXED
PAR:   W BLOCK 4
       ENDSTACK
UFILL:  ENTF VBAS
       W SUB3 IND(B.PAR+8),IND(B.PAR+4),R1
       W1+1
       W2:=IND(B.PAR+4)
       W2-1
       W3:=IND(B.PAR+12)
       W BMOVE R3,IND(B.PAR)(R2),R1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
