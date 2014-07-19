*
* $Id: ublow.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ublow.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UBLOW
%
% CERN PROGLIB# M409    UBLOW           .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% CALL UBLOW(AVM,XVM,N)        CONVERT HOLLERITH STRING A4 TO A1
%
       EXPORT UBLOW
       ROUTINE UBLOW
       LIB UBLOW
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
UBLOW:  ENTF VBAS
       W1:=0
       W2:=IND(B.PAR+8)
       IF<=GO OUT
       W2-1
BEGLP: BY3:=IND(B.PAR)(R1)
       W SHR R3,-8
       W3+10020040B
       W3=:IND(B.PAR+4)(R1)
       W LOOPI R1,R2,BEGLP
OUT:   RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
