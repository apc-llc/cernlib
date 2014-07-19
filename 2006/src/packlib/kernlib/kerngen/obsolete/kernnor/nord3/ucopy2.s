*
* $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:54:50 mclareni Exp $
*
* $Log: ucopy2.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:50  mclareni
* Kernlib
*
*
       MODULE M_UCOPY2
%
% CERN PROGLIB# V301    UCOPY2          .VERSION KERNNOR  2.05  880419
% ORIG.  H.OVERAS, CERN, 791213
% MODIF. P.S.Iversen, 8-dec-87
%
% CALL UCOPY2(AVM,XVM,N)       COPY A TO X , ANY OVERLAP
%
       EXPORT UCOPY2
       ROUTINE UCOPY2
       LIB UCOPY2
VBAS:  STACK FIXED
PAR:   W BLOCK 3
       ENDSTACK
UCOPY2:  ENTF VBAS
       W3 := IND(B.PAR+8)                        % Load N
       IF <= GO OUT                              % If <= zero, exit
       W BMOVE IND(B.PAR),IND(B.PAR+4),W3        % Move words
OUT:   RET                                       % Exit
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
