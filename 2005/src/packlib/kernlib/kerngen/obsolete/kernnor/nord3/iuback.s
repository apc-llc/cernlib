*
* $Id: iuback.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iuback.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUBACK
%
% CERN PROGLIB# M410    IUBACK          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% IX = IUBACK(AVM,JL,JR)         READ A1-SECTION BACKWARD FOR INTEGER
%
       IMPORT-D SLATE
       EXPORT IUBACK
       ROUTINE IUBACK
       LIB IUBACK
VBAS:  STACK FIXED
PAR:   W BLOCK 3
BLANK: W DATA '    '
TENTON:W DATA 1
       ENDSTACK
IUBACK:  ENTF VBAS
       W1 CLR
       W2:=IND(B.PAR+8)
       W2-1
       W4:=IND(B.PAR+4)
       W4-2
       W SET1 B.TENTON
       W STZ SLATE
L12:   W2 COMP R4
       IF=GO L20
       W3:=IND(B.PAR)(R2)
       W3 COMP B.BLANK
       IF=GO L19
       W SHL R3,-24
       BY3 COMP 60B:B
       IF < GO L20
       BY3 COMP 71B:B
       IF > GO L20
       BY3-60B:B
       W1 PSUM B.TENTON,R3
       W MUL2 B.TENTON,10
       W INCR SLATE
L19:   W2-1
       GO L12
L20:   W ADD3 R2,1,SLATE+4
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUBACK
#undef CERNLIB_TCGEN_IUBACK
#endif
