*
* $Id: ubunch.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: ubunch.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  03/05/72
 .                                      FROM COPENHAGEN  3/05/72
          AXR$
$(2)      LIT
 .    CALL UBUNCH (A,B,N)
$(1)
UBUNCH*   L,H2      A0,0,X11            . A0= ADR(A)
          LXI,U     A0,1
          L,H2      A1,1,X11            . A1= ADR(B)
          L         A2,*2,X11           . A2= N
          TNG,U     A2,6
          J         SHORT               . LESS THAN 6 CHAR'S
.
          LXI,U     A1,1
NEXT      L,S1      A4,0,*A0
          LSSL      A4,30               . CHAR 1
          L,S1      A3,0,*A0
          LSSL      A3,24               . CHAR 2
          OR        A3,A4
          L,S1      A3,0,*A0
          LSSL      A3,18               . CHAR 3
          OR        A3,A4
          L,S1      A3,0,*A0
          LSSL      A3,12               . CHAR 4
          OR        A3,A4
          L,S1      A3,0,*A0
          LSSL      A3,6                . CHAR 5
          OR        A3,A4
          L,S1      A3,0,*A0
          OR        A3,A4
          S         A4,0,*A1
.
          AN,U      A2,6
          TG,U      A2,6
          J         NEXT
.
SHORT     TNG,U     A2,1
          J         4,X11               . EXIT FOR  NO TAIL
          L         A4,('      ')
          L,H1      A3,KEY-1,A2
          J         0,A3
 .
CH5       LSSL      A4,6
          L,S1      A3,0,*A0
          OR        A3,A4
CH4       LSSL      A4,6
          L,S1      A3,0,*A0
          OR        A3,A4
CH3       LSSL      A4,6
          L,S1      A3,0,*A0
          OR        A3,A4
CH2       LSSL      A4,6
          L,S1      A3,0,*A0
          OR        A3,A4
CH1       LSSL      A4,6
          L,S1      A3,0,*A0
          OR        A3,A4
.
          L,H2      A3,KEY-1,A2
          LSSC      A4,0,A3
          S         A4,0,A1
          J         4,X11
$(2)
KEY       +         CH1,30
          +         CH2,24
          +         CH3,18
          +         CH4,12
          +         CH5,6
          END
#ifdef CERNLIB_TCGEN_UBUNCH
#undef CERNLIB_TCGEN_UBUNCH
#endif
