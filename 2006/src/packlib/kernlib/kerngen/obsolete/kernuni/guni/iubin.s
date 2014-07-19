*
* $Id: iubin.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: iubin.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTIONS IUBIN(X,PAR,SPILL) AND IUHIST(X,PAR,SPILL)
             AXR$
$(1)
IUHIST*     L     A0,1,X11       . PU ADDRESS OF PAR
            L     A2,0,A0        . A2=PAR(1)=NX (INTEGER)
            L     A3,1,A0        . A3=PAR(2)=1./DX
            L     A4,2,A0        . A4=PAR(3)=XLOW
            SZ    A5             . SPILL=.FALSE.
            DS    A6,XREG6       . SAVE TWO REGISTERS.
            L     A6,*0,X11      . A6=X
            FAN   A6,A4          .
            FM    A6,A3          . A6=(X-XLOW)*(1./DX)
            TP    A6             .
            J     OVERFL         . A6 0.
            FA    A6,(1.0)       . A6=(X-XLOW)*(1./DX)+1.
            LUF   A6,A6          . CONVERT A6 TO INTEGER.
            AN,XU A6,163         .
            JP    A6,OVER             .
            SMA,H2  A6,$+2       . STORE SIZE OF RIGHT SHIFT.
            LSSC  A7,8           . LEFT SHIFT 8 PLACES.
            SSA   A7,0           . SHIFT LOADED FROM ABOVE.
TEST        L     A0,A7          . IUHIST=N
            TG    A2,A7          .
            J     SLUTIT         .
            L,U   A5,1           . SPILL=.TRUE.
            A,U   A2,1           .
            L     A0,A2          . IUHIST=NX+1
            J     SLUTIT         .
OVERFL      L,U   A5,1           . SPILL=.TRUE.
            SZ    A0             . IUHIST=0
SLUTIT      DL    A6,XREG6       . RESTORE REGISTERS
            S     A5,*2,X11      . SET SPILL
            J     4,X11          . RETURN
IUBIN*      L     A0,1,X11       . PU ADR.OF PAR
            L     A2,0,A0        . A2=NX
            L     A3,(1.0)       .
            FD    A3,1,A0        . A3=1./DX
            J     IUHIST+3       .
OVER        L     A7,(0377777777777)  .
            J     TEST                .
$(2)
XREG6       +     0
XREG7       +     0
            END
#ifdef CERNLIB_TCGEN_IUBIN
#undef CERNLIB_TCGEN_IUBIN
#endif
