*
* $Id: iuchan.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: iuchan.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  09/10/74
 .                                      FROM COPENHAGEN  3/05/72
 . FUNCTION IUCHAN(X,XLOW,DX,NX)
            AXR$
$(1)
IUCHAN*   L       A0,*0,X11     . A0=X
          L       A2,*1,X11     . A2=XLOW
          L       A3,*2,X11     . A3=DX
          L       A4,*3,X11     . A4=NX
          FAN     A0,A2         .
          FD      A0,A3         .
          FA      A0,(1.0)      . A0=AN=(X-XLOW)/DX+1.0
          TP      A0            .
          SZ      A0            .
          LUF     A0,A0         . CONVERT TO INTEGER.
          AN,XU   A0,163        .
            JP    A0,OVER             .
          SMA,H2  A0,$+2        .
          LSSC    A1,8          .
          SSA     A1,0          .
TEST      L       A5,A4         . PRESET A5 FOR OVERFLOW.
          A,U     A5,1          .
          L       A0,A1         . IUCHAN=N
          TLE     A4,A1         . N:NX<<
          L       A0,A5         . YES - IUCHAN=NX+1
          J       5,X11         . RETURN
OVER        L     A1,(0377777777777)  .
            J     TEST                .
          END
#ifdef CERNLIB_TCGEN_IUCHAN
#undef CERNLIB_TCGEN_IUCHAN
#endif
