*
* $Id: iufind.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: iufind.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION IUFIND(T,A,JL,JR)
           AXR$
$(1)
IUFIND*    L      A2,*2,X11      . A2=JL
           L      A3,*3,X11      . A3=JR
           AN     A3,A2          . A3-JR-JL
           L      R3,A3          . SET LOOP COUNTER.
           AN,U   A2,1           . A2=JL-1
           L      A1,1,X11       . PU AD(A)
           LXI,U  A1,1           .
           L      A3,*0,X11      . A3=T
           A      A1,A2          . A1=1,AD(A)+JL-1
OMIGEN     A,U    A2,1           . INCREMENT J
           L      A4,0,*A1       . PU ELEMENT
           TNE    A4,A3          . AND TEST IT.
           J      $+3            . MATCH@-@LEAVE LOOP.
           JGD    R3,OMIGEN      . NO MATCH@-@GET NEXT.
           A,U    A2,1           . NOT FOUND@-@J=JR+1.
           L      A0,A2          . IUFIND=J
           J      5,X11          . RETURN
           END
#ifdef CERNLIB_TCGEN_IUFIND
#undef CERNLIB_TCGEN_IUFIND
#endif
