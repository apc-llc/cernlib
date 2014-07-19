*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION IUCOMP(T,A,N)
           AXR$
$(1)
IUCOMP*    LM     A2,*2,X11    . A2=/N/
           SZ     A0
           TNZ    A2           .
           J      4,X11        . N=0@@RETURN.
           L      A1,1,X11     .
           LXI,U  A1,1         . A1=1,AD(A)
           L      A3,*0,X11    . A3=T
           AN,U   A2,1         . A2=N-1
           L      R3,A2        . R3=N-1
           SZ     A5           .
           L      A4,0,*A1     . PU ELEMENT AND
           A,U    A5,1         .
           TNE    A4,A3        . TEST IT.
           J      $+3          . MATCH@-@LEAVE LOOP.
           JGD    R3,$-4       . NO MATCH@-@LOOP FOR NEXT.
           J      4,X11        . NOT FOUND@-@RETURN
           L      A0,A5        . IUCOMP=J
           J      4,X11        . FOUND@-@RETURN.
           END
#ifdef CERNLIB_TCGEN_IUCOMP
#undef CERNLIB_TCGEN_IUCOMP
#endif
