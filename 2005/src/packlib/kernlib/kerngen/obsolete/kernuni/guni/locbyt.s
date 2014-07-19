*
* $Id: locbyt.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: locbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.
 .                                      ADAPTED FROM ORSAY, 1/10/76
.         FUNCTION LOCBYT( IT,VEC,N,INC,L,NBITS )***M 428 ***
          AXR$
$(1)
LOCBYT*
          L         A5,*2,X11          . N
          L         A4,*3,X11          . INC
          SZ        A0
          LMA       A1,A5              . IABS(N)
          JZ        A4,IUH1            . IF INC=0 USE N=1
          DI        A0,A4              . IABS(N)/INC
IUH1      L,H2      A2,1,X11           . LOC(VEC)
          LN        A3,*5,X11          . -NBITS
          TZ        A1
          A,U       A0,1               . COVERED DIVIDE
          LN        A1,*4,X11          . -L
          SMA       A0,R1              . NSCAN
          AN,U      A0,1               . NSCAN-1
          JN        A5,IUH3
IUH2
          LN,XU     A5,0               . MASK = -0
          LXI       A2,A4
          SSL       A5,36,A3           . KEEP ONLY NBITS BITS=1
          SSC       A5,37,A1           . LEFT SHIFT L-1 PLACES TO GET THE MASK
          S         A5,R2
          L         A5,*0,X11          . IT
          SSC       A5,37,A1           . LEFT SHIFT IT L-1 PLACES
          MSE       A5,0,*A2           . SEARCH
          J         IUH4
          AN        A0,R1
          MSI       A0,A4
          A,U       A0,1
          J         7,X11
.                                      . N.LT.0
IUH3      MSI       A0,A4
          A         A2,A0              . LOC(VEC(1+(NSCAN-1)*INC))
          LN        A4,A4
          SZ        A0
          J         IUH2
IUH4      SZ        A0
          J         7,X11
          END
#ifdef CERNLIB_TCGEN_LOCBYT
#undef CERNLIB_TCGEN_LOCBYT
#endif
