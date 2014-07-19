*
* $Id: ublank.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ublank.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UBLANK
%
% CERN PROGLIB# V300    UBLANK          .VERSION KERNNOR  1.05  810521
% ORIG.  H.OVERAS, CERN, 810330
%
% CALL UBLANK(XVM,JL,JR)       XVM(J)=BLANK  J=(JL/JR)
%
       EXPORT UBLANK
       ROUTINE UBLANK
       LIB UBLANK
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
UBLANK:  ENTF VBAS
       W SUB3 IND(B.PAR+8), IND(B.PAR+4),R1
       W1+1
       W2:=IND(B.PAR+4)
       W2-1
       W BMOVE 4010020040B,IND(B.PAR)(R2),R1
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
