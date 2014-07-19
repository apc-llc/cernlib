*
* $Id: ublank.s,v 1.1.1.1 1996/02/15 17:53:58 mclareni Exp $
*
* $Log: ublank.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:58  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE UBLANK(A,JL,JR)                 *** V300 ***
.     SUBROUTINE UZERO(A,JL,JR)
.     SUBROUTINE UFILL(A,JL,JR,STUFF)
 
          AXR$
BLANK     '      '
$(1)
UFILL*
          L         A5,*3,X11     . STUFF
          L,U       A1,1
          J         UZR2
UBLANK*
          L         A5,BLANK
          J         UZR1
UZERO*
          SZ        A5
UZR1      L,U       A1,0
UZR2
          L         A2,*1,X11          . JL
          L,U       A0,A5
          JGD       A2,$+2
          SZ        A2                 . MAX0(1,JL)-1
          ANU       A2,*2,X11          . -JR+JL-1
          A,H2      A2,0,X11           . LOC(A(JL))
          TN        A3
          J         EX                 . DO NOTHING FOR JL.GT.JR
          SMA       A3,R1
          LXI,U     A2,1
          BT        A2,0,*A0           . TRANSFER (A5) TO ARRAY A
EX        A         X11,A1
          J         4,X11
 END
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
#ifdef CERNLIB_TCGEN_UFILL
#undef CERNLIB_TCGEN_UFILL
#endif
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
