*
* $Id: pkbyt.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: pkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . PKBYT(M,A,JTH,N,NBITS)
           AXR$
$(1)
PKBYT*     L      A4,*3,X11       . A4=N
           TZ     A4              .
           TP     A4              .
           J      PKBYT5          . N =0,RETURN
           DS     A6,REGS         . SAVE REGISTERS.
           DS     A8,REGS+2       .
           A,U    A4,1            . A4=N+1
           L,U    A5,1            . A5=1
           L      A0,0,X11        .
           LXI,U  A0,1            . A0=1,ADR(M)
           L      A1,4,X11        . A1=ADR(NBITS)
           L      A2,0,A1         . A2=NBIT
           L      A6,1,A1         . A6=INWORD
           L      A1,1,X11        .
           LXI,U  A1,1            . A1=1,ADR(A)
           TP     A2              .
           J      $+3             .
           TZ     A2              .
           J      PKBYT1          . NBIT:0
           L,U    A2,1            . A2=NBIT=1
           L,U    A6,36           . A6=INWORD=36
PKBYT1     L      A7,*2,X11       . A7=JTH
           TLE,U  A7,2            .
           J      PKBYT2          . JTH 2
           L      A9,A7           .
           AN,U   A9,1            .
           SZ     A8              .
           DI     A8,A6           . A8=(JTH-1)/INWORD
           A      A1,A8           . FIND ADR.OF WORD IN A.
           LN     A9,A8           . A9=-(JA-1)
           MSI    A9,A6           .
           A      A9,A7           . A9=JTH-(JA-1)*INWORD
           L      A3,A9           .
           AN,U   A3,1            .
           MSI    A3,A2           . A3=LOC=((A9)-1)*NBIT
           L      A7,A6           .
           A,U    A7,2            .
           AN     A7,A9           . A7=JH=INWORD+2-(A9)
           J      $+4             .
PKBYT2     L      A7,A5           . JTH 2
           A      A7,A6           . A7=JH=JM+INWORD
           SZ     A3              . LOC=0
           TG     A7,A4           .
           L      A7,A4           . JH=MIN0(JH,N1)
PKBYT4     L      A8,0,*A0        . A8=M(JM)
           L      A9,0,A1         . A9=A(JA)
           SSC    A9,0,A3         . SHIFT RIGHT CIRC. LOC PLACES.
           DSL    A8,0,A2         . DOUB.SHIFT LOG. NBIT
           A      A3,A2           . LOC=LOC+NBIT
           LSSC   A9,0,A3         . SHIFT LEFT CIRC. LOC+NBIT
           S      A9,0,A1         . REPLACE WORD IN A
           A,U    A5,1            . BUMP JM
           TLE    A5,A7           . MORE TO DO<
           J      PKBYT4          . YES-DO NEXT BYTE.
           TG     A5,A4           . NEXT WORD IN A<
           J      PKBYT5          . NO - RETURN
           L      A9,0,*A1        . YES - INCREMENT A1.
           J      PKBYT2          .
PKBYT5     DL     A6,REGS         . RESTORE REGISTERS
           DL     A8,REGS+2       .
           J      6,X11           . AND RETURN.
$(2)
REGS       RES    4
           END
#ifdef CERNLIB_TCGEN_PKBYT
#undef CERNLIB_TCGEN_PKBYT
#endif
