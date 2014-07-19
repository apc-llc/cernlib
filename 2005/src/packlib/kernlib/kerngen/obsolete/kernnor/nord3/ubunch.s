*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UBUNCH
%
% CERN PROGLIB# M409    UBUNCH          .VERSION KERNNOR  1.12  811126
% ORIG.  H.OVERAS, CERN, 791213
%
% CALL UBUNCH(AVM,XVM,N)       CONVERT HOLLERITH STRING A1 TO A4
%
       EXPORT UBUNCH
       ROUTINE UBUNCH
       LIB UBUNCH
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
UBUNCH:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF<=GO OUT
       W2-1
       W4:=R2
       W4/4
       W3:=10020040B
       W3 PUTBF IND(B.PAR+4)(R4),0,24
BEGLP: W3:=IND(B.PAR)(R1)
       W SHR R3,8
       BY3=:IND(B.PAR+4)(R1)
       W LOOPI R1,R2,BEGLP
OUT:   RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
