*
* $Id: iuforw.s,v 1.1.1.1 1996/02/15 17:54:48 mclareni Exp $
*
* $Log: iuforw.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:48  mclareni
* Kernlib
*
*
       MODULE M_IUFORW
%
% CERN PROGLIB# M410    IUFORW          .VERSION KERNNOR  1.01  800718
% ORIG.  H.OVERAS, CERN, 791213
%
% IX = IUFORW(AVM,JL,JR)         READ A1-SECTION FORWARD FOR INTEGER
%
       IMPORT-D SLATE
       EXPORT IUFORW
       ROUTINE IUFORW
       LIB IUFORW
VBAS:  STACK FIXED
PAR:   W BLOCK 3
BLANK: W DATA '    '
       ENDSTACK
IUFORW:  ENTF VBAS
       W1 CLR
       W2:=IND(B.PAR+4)
       W2-1
       W4:=IND(B.PAR+8)
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
       W1 MULAD 10,R3
       W INCR SLATE
L19:   W2+1
       GO L12
L20:   W ADD3 R2,1,SLATE+4
       RET
       ENDROUTINE
       ENDMODULE
#ifdef CERNLIB_TCGEN_IUFORW
#undef CERNLIB_TCGEN_IUFORW
#endif
