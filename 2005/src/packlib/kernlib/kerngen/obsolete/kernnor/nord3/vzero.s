*
* $Id: vzero.s,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: vzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
       MODULE M_VZERO
%
% CERN PROGLIB# F121    VZERO           .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 810330
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL VZERO(XVM,N)             XVM(I)=0.
%
       EXPORT VZERO
       ROUTINE VZERO
       LIB VZERO
VBAS:  STACK FIXED
PAR:   W BLOCK 2
       ENDSTACK
VZERO:  ENTF VBAS
       W3 := IND(B.PAR+4)                        % Load N
       IF <= GO OUT                              % If <= zero, exit
       W BMOVE 0B,IND(B.PAR),W3                  % Fill with zero
OUT:   RET                                       % Exit
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
