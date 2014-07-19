*
* $Id: vfill.s,v 1.1.1.1 1996/02/15 17:54:51 mclareni Exp $
*
* $Log: vfill.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:51  mclareni
* Kernlib
*
*
       MODULE M_VFILL
%
% CERN PROGLIB# F121    VFILL           .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 810330
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL VFILL(XVM,N,CM)          XVM(I)=CM
%
       EXPORT VFILL
       ROUTINE VFILL
       LIB VFILL
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
VFILL:  ENTF VBAS
       W2 := IND(B.PAR+4)                        % Load N
       IF <= GO OUT                              % If <= zero, exit
       W3 := IND(B.PAR+8)                        % Fill pattern
       W BMOVE W3,IND(B.PAR),W2                  % Fill words
OUT:   RET                                       % Exit
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
