*
* $Id: iuhunt.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: iuhunt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION IUHUNT(T,A,N,INC)
          AXR$
$(1)
IUHUNT*   SZ        A0             .
          L         A5,*2,X11      . A5=N
          TNZ       A5             .
          J         5,X11          . N=0@-@RETURN
          AN,U      A5,1           . A5=N-1
          SZ        A4             . PREPARE TO DIVIDE.
          L         A3,*3,X11      . A3=INC
          DI        A4,A3          . A4=(N-1)/INC
          L         R3,A4          . SET LOOP COUNTER.
          L         A1,1,X11       . PU AD(A)
          LXI       A1,A3          . A1=INC,AD(A)
          L         A2,*0,X11      . A2=T
          L,U       A0,1           . START VALUE OF IUHUNT.
PING      L         A5,0,*A1       . PU WORD AND
          TNE       A5,A2          . TEST IT.
          J         PONG           . MATCH@-@LEAVE LOOP.
          A         A0,A3          . INCREMENT J.
          JGD       R3,PING        . NO MATCH@-@LOOP.
          SZ        A0             . NOT FOUND@-@IUHUNT=0.
PONG      J         5,X11          . RETURN
          END
#ifdef CERNLIB_TCGEN_IUHUNT
#undef CERNLIB_TCGEN_IUHUNT
#endif
