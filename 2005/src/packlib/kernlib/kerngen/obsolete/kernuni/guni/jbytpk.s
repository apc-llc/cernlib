*
* $Id: jbytpk.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: jbytpk.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION JBYTPK(A,JTH,NBITS)
             AXR$
$(1)
JBYTPK*      S       A6,REGS         . SAVE REGISTER.
             L       A0,2,X11        . A0=ADR(NBITS)
             L       A4,0,A0         . A4=NBIT
             L       A5,1,A0         . A5=INWORD
             L       A3,*1,X11       . A3=JTH
             SZ      A2              .
             L       A6,A3           . A6=JTH
             L       A1,0,X11        . A1=ADR(A)
             AN,U    A3,1            . A3=JTH-1
             TP      A4              . IS NBIT 1
             J       $+3             .
             TZ      A4              .
             J       $+3             . NO
             L,U     A5,36           . YES-INWORD=36
             L,U     A4,1            . AND NBIT=1
             DI      A2,A5           . A2=JA-1=(JTH-1)/INWORD
             A       A1,A2           . A1=ADR(A)+JA-1
             MSI     A2,A5           . A2=(JA-1)*INWORD
             AN      A6,A2           . A6=JPOS=JTH-(JA-1)*INWORD
             L,U     A2,36           .
             AN,U    A6,1            .
             AN      A2,A4           . A2=36-NBIT
             MSI     A6,A4           .
             L       A0,0,A1         . PU PACKED WORD
             A       A6,A4           . LOC=(JPOS-1)*NBIT+NBIT
             L       A3,A6           . A3=LOC
             SSC     A0,0,A3         . ROT.CIRC.LOC PLACES RIGHT.
             SSL     A0,0,A2         . SHIFT LOG. 36-NBIT RIGHT.
             L       A6,REGS         . RESTORE REGISTER.
             J       4,X11           . RETURN
$(2)
REGS         +       0
             END
#ifdef CERNLIB_TCGEN_JBYTPK
#undef CERNLIB_TCGEN_JBYTPK
#endif
