*
* $Id: ucopy.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: ucopy.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . ROUTINES FOR TRANSFERRING DATA FROM ONE VECTOR TO
 . ANOTHER.
 . ENTRY UCOPY(A,B,N)
            AXR$
$(1)
UCOPY*    L      A0,*2,X11      . PICK UP N
          TNG,U  A0,1           . AND TEST IT
          J      4,X11          . RETURN@-N =0.
          S      A0,R1          . SET UP COUNTER
         L      A0,0,X11       . GET ADDRESS OF A
          L      A1,1,X11       . GET ADDRESS OF B
          LXI,U  A0,1           .
          LXI,U  A1,1           .
          J      NORMAL         . GO AND MOVE 'EM.
 . ENTRY UCOPY2(A,B,N)
UCOPY2*   L      A2,*2,X11      . PICK UP N
          TNG,U  A2,1           . AND TEST IT.
          J      4,X11          . RETURN
          S      A2,R1          . SET COUNTER
          L      A0,0,X11       .
          L      A1,1,X11       .
          LXI,U  A0,1           .
          LXI,U  A1,1           .
          TNE    A0,A1          .
          J      4,X11          . RETURN IF TOTAL OVERLAP
          TG     A0,A1          . WHICH STARTS FIRST
          J      NORMAL         . A BEFORE B
          LXI,U  A0,-1          . A BEFORE B
          LXI,U  A1,-1          . GO BACKWARDS.
          AN,U   A2,1           . A2=N-1
          A      A0,A2          . GET LAST ADDRESS IN A
          A      A1,A2          . AND IN B
NORMAL    BT     A1,0,*A0       . MOVE N WORDS.
          J      4,X11          . RETURN@-ALL DONE.
 . ENTRY UCOPIV(A,B,N)
UCOPIV*   L      A2,*2,X11      . PICK UP N AND
          TNG,U  A2,1           . TEST IT.
          J      4,X11          . RETURN.
          L      A0,0,X11       .
          LR     R1,A2          . SET UP COUNTER
          L      A1,1,X11       .
          AN,U   A2,1           . A2=N-1
          LXI,U  A0,1           .
          A      A1,A2          . BACKWRDS THROUGH B
          LXI,U  A1,-1          .
          J      NORMAL         . GO MOCE 'EM.
 . ENTRY UCOPYN(A,B,N)
UCOPYN*   L      A2,*2,X11      . PICK UP N
          TNG,U  A2,1           .
          J      4,X11          .
          AN,U   A2,1           . A2=N-1
          LR     R3,A2          .
          L      A0,0,X11       .
          L      A1,1,X11       .
          LXI,U  A0,1           .
          LXI,U  A1,1           .
LODNEG    LN     A3,0,*A0       . P.U. (-B)
          S      A3,0,*A1       . AND MOVE IT.
          JGD    R3,LODNEG      . LOOP FOR NEXT.
          J      4,X11          . RETURN
          END
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
#ifdef CERNLIB_TCGEN_UCOPIV
#undef CERNLIB_TCGEN_UCOPIV
#endif
#ifdef CERNLIB_TCGEN_UCOPYN
#undef CERNLIB_TCGEN_UCOPYN
#endif
