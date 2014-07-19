*
* $Id: upkbyt.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: upkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . SUBROUTINE UPKBYT(A,JTH,M,N,NBITS)
            AXR$
$(1)
UPKBYT*     L      A4,*3,X11     . A4=N
            TZ     A4            .
            TP     A4            .
            J      6,X11         . RETURN,N =0
            DS     A6,REGS       . SAVE REGISTERS.
            DS     A8,REGS+2     .
            S      A10,REGS+4    .
            A,U    A4,1          . A4=N1=N+1
            L      A0,2,X11      .
            L,U    A5,1          . A5=JM=1
            LXI,U  A0,1          . A0=1,ADR(M)
            L      A1,4,X11      . A1=ADR(NBITS)
            L      A2,0,A1       . A2=NBIT
            L      A6,1,A1       . A6=INWORD
            L      A1,0,X11      .
            LXI,U  A1,1          . A1=1,ADR(A)
            TP     A2            .
            J      $+3           .
            TZ     A2            .
            J      UPKBT1        . NBIT:0
            L,U    A2,1          . NBIT=1
            L,U    A6,36         . INWORD=36
UPKBT1      L      A7,*1,X11     . A7=JTH
            L      A10,A2        . SAVE VALUE OF NBIT.
            TLE,U  A7,2          .
            J      UPKBT2        . JTH 2
            L      A9,A7         .
            SZ     A8            .
            AN,U   A9,1          . A9=JTH-1
            DI     A8,A6         . A8=JA-1=(JTH-1)/INWORD
            A      A1,A8         . MODIFY ADR. IN ARRAY A.
            LN     A9,A8         . A9=-(JA-1)
            MSI    A9,A6         .
            A      A9,A7         . A9=JPOS=JTH-(JA-1)*INWORD
            L      A3,A9         .
            L      A7,A6         .
            AN,U   A3,1          .
            A,U    A7,2          .
            MSI    A3,A2         . A3=LOC=(JPOS-1)*NBIT
            AN     A7,A9         . A7=JH=INWORD+2-JPOS
            A      A3,A2         . A3=LOC+NBIT
            J      $+4           .
UPKBT2      L      A7,A5         . HERE IF JTH 2.
            L      A3,A10        . A3=LOC=NBIT
            A      A7,A6         . A7=JH=JM+INWORD
            TG     A7,A4         .
            L      A7,A4         . JH=MINO(JH,N1)
            L,U    A2,36         .
            AN     A2,A10        .
UPKBT4      L      A8,0,A1       . A8=A(JA)
            SSC    A8,0,A3       . RIGHT CIRC. ROTATE LOC+NBIT
            A      A3,A10        . A3=A3+NBIT
            SSL    A8,0,A2       . RIGHT LOG. SHIFT 36-NBIT
            A,U    A5,1          . BUMP JM
            S      A8,0,*A0      . RESULT TO M(JM)
            TLE    A5,A7         . MORE TO DO<
            J      UPKBT4        . YES - LOOP.
            TG     A5,A4         . NEXT WORD IN A<
            J      UPKBT5        . NO - RETURN.
            L      A8,0,*A1      . YES - INCREMENT ADR(A)
            J      UPKBT2        . RESET AND CONTINUE.
UPKBT5      DL     A6,REGS       . RESTORE REGISTERS.
            DL     A8,REGS+2     .
            L      A10,REGS+4    .
            J      6,X11         . RETURN
$(2)
REGS        RES    5             . REGISTER STORAGE.
            END                  .
#ifdef CERNLIB_TCGEN_UPKBYT
#undef CERNLIB_TCGEN_UPKBYT
#endif
