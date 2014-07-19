*
* $Id: ucopy.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ucopy.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UCOPY
%
% CERN PROGLIB# V301    UCOPY           .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 791213
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL UCOPY(AVM,XVM,N)         COPY A TO X
%
       EXPORT UCOPY
       ROUTINE UCOPY
       LIB UCOPY
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
UCOPY:  ENTF VBAS
       W3:=IND(B.PAR+8)
       IF <= GO OUT                              % If N <= zero, exit
       W BMOVE IND(B.PAR),IND(B.PAR+4),W3        % Move words
OUT:   RET                                       % Exit
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
