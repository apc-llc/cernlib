*
* $Id: sbit1.s,v 1.1.1.1 1996/02/15 17:54:49 mclareni Exp $
*
* $Log: sbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:49  mclareni
* Kernlib
*
*
       MODULE M_SBIT1
%
% CERN PROGLIB# M421    SBIT1           .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% CALL SBIT1(XM,J)       SET BIT J TO 1
%
       EXPORT SBIT1
       ROUTINE SBIT1
       LIB SBIT1
VBAS:  STACK FIXED
PAR:   W BLOCK 2
       ENDSTACK
SBIT1: ENTF VBAS
       W1:=1
       W2:=IND(B.PAR+4)
       W2-1
       W1 PUTBI IND(B.PAR),BY2
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_SBIT1
#undef CERNLIB_TCGEN_SBIT1
#endif
