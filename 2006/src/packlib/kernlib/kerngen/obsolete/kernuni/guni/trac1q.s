*
* $Id: trac1q.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: trac1q.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      FROM COPENHAGEN  3/05/72
          AXR$
.  COMMON/SLATE/    NAME,LINE,LADR,LOK,LASTWB,DUMMY(35)
SLATE     INFO      2     4
$(4).
NAME      RES       1
LINE      RES       1
LADR      RES       1
LOK       RES       1
LASTWB    RES       1
DUMMY     RES      35
$(2)      LIT
$(1)
TRAC1Q*   L,H2      A0,0,X11
          S         A0,LASTWB
          J         1,X11
.
TRAC2Q*   L         A0,LASTWB
          TNZ       A0
          J         MAINEX
          L,XH2     A0,1,A0
          S         A0,LADR             . STORE LADR
.   *****************   VERIFY LADR LEGAL
          LMJ       A1,CHECK
FIND      L,XH1     A1,0,A0
          TZ        A1
          J         GOTIT
          L,U       A0,1,A0
          J         FIND
.
GOTIT     S         A1,LINE             .STORE LINE
          L,XH2     A0,0,A0
          S         A0,LASTWB           . STORE LWBP
          TNZ       A0
          J         MAIN
.   ****************    VERIFY  LWBP  LEGAL
          LMJ       A1,CHECK
          L         A1,0,A0
HOME      S         A1,NAME             . STORE NAME
          J         1,X11
MAIN      L         A1,('MAIN  ')
          J         HOME
.
CHECK     TNG,U     A0,01000
          J         BAD
          TNG,U     A0,LASTI$
          J         0,A1
          TNG,U     A0,040000
          J         BAD
          TNG,U     A0,LASTD$
          J         0,A1
BAD       L         A12,X11             . LOK= -VE
          SN        A12,LOK
          J         1,X11
MAINEX    S         X11,LOK             . LOK= +VE
          J         1,X11
          END
