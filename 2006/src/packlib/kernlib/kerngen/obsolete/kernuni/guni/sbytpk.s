*
* $Id: sbytpk.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: sbytpk.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . SUBROUTINE SBYTPK(M,A,JTH,NBITS)
             AXR$
$(1)
SBYTPK*     L      A0,3,X11       . A0=ADR(NBITS)
            L      A2,0,A0        . A2=NBIT
            L      A5,1,A0        . A5=INWORD
            L      A4,*2,X11      . A4=JTH
            SZ     A3             .
            L      A1,A4          . SAVE VALUE OF JTH
            L      A0,1,X11       . A0=ADR(A)
            AN,U   A4,1           . A4=JTH-1
            TP     A2             . IS NBIT   1<
            J      $+3            .
            TZ     A2             .
            J      $+3            .
            L,U    A5,36          . YES-INWORD=36
            L,U    A2,1           . AND NBIT=1
            DI     A3,A5          . A3=JA-1=(JTH-1)/INWORD
            A      A0,A3          . A0=ADR(A)+JA-1
            MSI    A3,A5          . A3=(JA-1)*INWORD
            AN     A1,A3          . A1=JPOS=JTH-(JA-1)*INWORD
            L      A5,0,A0        . PU CONTENTS OF A(JA)
            AN,U   A1,1           .
            L      A4,*0,X11      . PU M TO A4
            MSI    A1,A2          . A1=LOC=(JPOS-1)*NBIT
            SSC    A5,0,A1        . ROT.RIGHT LOC PLACES
            DSL    A4,0,A2        . SHIFT LOGICAL RIGHT NBIT PLACES
            A      A1,A2          . A1=LOC+NBIT
            LSSC   A5,0,A1        . ROT LEFT LOC+NBIT PLACES
            S      A5,0,A0        . REPLACE A(JA)
            J      5,X11          . RETURN
            END
#ifdef CERNLIB_TCGEN_SBYTPK
#undef CERNLIB_TCGEN_SBYTPK
#endif
