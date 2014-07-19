*
* $Id: incbyt.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: incbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION INCBYT(INC,IA,JTH,NBITS)
            AXR$
$(1)
INCBYT*     DS     A6,REGS        . SAVE REGISTERS.
            L      A1,3,X11       . A1=ADR(NBITS)
            L      A4,0,A1        . A4=NBIT
            L      A6,1,A1        . A6=INWORD
            L      A5,2,A1        . A5=MAX
            L      A1,*2,X11      . A1=JTH
            SZ     A0             .
            L      A2,A1          . A2=JTH
            AN,U   A1,1           . A1=JTH-1
            DI     A0,A6          . A0=JA-1=(JTH-1)/INWORD
            L      A1,1,X11       . A1=ADR(A)
            A      A1,A0          . A1=ADR(A)+JA-1
            MSI    A0,A6          . A0=(JA-1)*INWORD
            AN     A2,A0          . A2=JPOS=JTH-(JA-1)*INWORD
            L      A6,0,A1        . A6=A(JA)
            AN,U   A2,1           .
            L,U    A3,36          .
            MSI    A2,A4          . A2=(JPOS-1)*NBIT
            AN     A3,A4          . A3=36-NBIT
            A      A2,A4          . A2=LOC+NBIT
            SSC    A6,0,A2        . ROT.RIGHT.CIRC.LOC+NBIT
            DSL    A6,0,A3        . SHIFT LOG.RIGHT 36-NBIT
            L      A3,A4          . A3=NBIT
            A      A6,*0,X11      . A6=BYTE+INC
            TG     A5,A6          . A6 =MAX<
            J      $+5            . YES
            L      A0,A6          . NO
            AN     A0,A5          . A0=OVERFLOW=A6-MAX.
            L      A6,A5          . BYTE=MAX
            J      $+2            .
            SZ     A0             . OVERFLOW=0
            DSL    A6,0,A3        . SHIFT LOG.RIGHT NBIT
            LSSC   A7,0,A2        . ROT.LEFT.CIRC. LOC
            S      A7,0,A1        . REPLACE A(JA)
            DL     A6,REGS        . RESTORE REGISTERS.
            J      5,X11          . RETURN
$(2)
REGS        RES    2
            END
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
