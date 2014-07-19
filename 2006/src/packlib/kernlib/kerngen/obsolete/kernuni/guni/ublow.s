*
* $Id: ublow.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: ublow.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  03/05/72
 .                                      FROM COPENHAGEN  3/05/72
          AXR$
$(2)      LIT
 .    CALL UBLOW (B,A,N)
$(1)
UBLOW*    L,H2      A1,0,X11            . A1=ADR(B)
          L,H2      A0,1,X11
          LXI,U     A0,1                . A0=  (1, ADR(A))
          L         A2,*2,X11           . A2= N
          L         R2,(007777777777)
          TNG,U     A2,6
          J         SHORT
.
          LXI,U     A1,1
NEXT      L         A3,0,*A1
          MLU       A3,BLANK
          S         A4,0,*A0
          LSSL      A3,6
CH5       MLU       A3,BLANK
          S         A4,0,*A0
          LSSL      A3,6
CH4       MLU       A3,BLANK
          S         A4,0,*A0
          LSSL      A3,6
CH3       MLU       A3,BLANK
          S         A4,0,*A0
          LSSL      A3,6
CH2       MLU       A3,BLANK
          S         A4,0,*A0
          LSSL      A3,6
CH1       MLU       A3,BLANK
          S         A4,0,*A0
.
          AN,U      A2,6
          TG,U      A2,6
          J         NEXT
SHORT     TNG,U     A2,1
          J         4,X11               . EXIT
          L         A3,0,A1
          J         *KEY-1,A2
$(2)
BLANK     +         '      '
KEY       +         CH1
          +         CH2
          +         CH3
          +         CH4
          +         CH5
          END
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
