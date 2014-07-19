*
* $Id: vblank.s,v 1.1.1.1 1996/02/15 17:53:59 mclareni Exp $
*
* $Log: vblank.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:59  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.     SUBROUTINE  VBLANK( A,N )                  *** F 121 ***
.     SUBROUTINE VZERO(A,N)
.     SUBROUTINE VFILL(A,N,STUFF)
          AXR$
BLANK     '      '
$(1)
VFILL*
          L         A5,*2,X11
          L,U       A1,1
          J         VZR2
VBLANK*
          L         A5,BLANK
          J         VZR1
VZERO*
          SZ        A5
VZR1      L,U       A1,0
VZR2
          L         A4,*1,X11          . N
          L,H2      A2,0,X11           . LOC( A )
          L,U       A0,A5
          JZ        A4,VZR3            . DO NOTHING IF N.EQ.0
          L         R1,A4
          LXI,U     A2,1
          BT        A2,0,*A0
VZR3      A         X11,A1
          J         3,X11
          END
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
