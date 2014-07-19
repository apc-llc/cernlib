*
* $Id: csnifn.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csnifn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.14/04 27/05/93  11.29.32  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSNIFN(JID,NPAR,ITFUN)
      PARAMETER (KLENID=8,KNIFUN=90)
      INTEGER NTIFUN(2,KNIFUN)
      CHARACTER *6 NMFUN(KNIFUN),NAME*8
      DATA NMFUN/'SQRT','DSQRT', 'EXP','DEXP','LOG','ALOG','DLOG',
*                  1       2       3     4      5     6      7
     + 'LOG10','ALOG10','DLOG10','SIN','DSIN','COS','DCOS',
*        8        9       10      11     12    13    14
     + 'TAN', 'DTAN','ASIN','DASIN','ACOS','DACOS','ATAN','DATAN',
*        15    16      17     18      19     20      21     22
     + 'ATAN2','DATAN2','ABS','IABS','DABS','MOD','AMOD','DMOD',
*        23      24       25    26     27    28     29     30
     + 'MAX','MAX0','AMAX1','DMAX1','AMAX0','MAX1','MIN','MIN0',
*        31    32      33     34      35      36    37     38
     + 'AMIN1','DMIN1','AMIN0','MIN1','LEN','INDEX','INT','IFIX',
*        39      40      41      42     43    44     45    46
     + 'IDINT','REAL', 'FLOAT','SNGL','DBLE'   ,
*        47     48       49      50     51
     + 'CSQRT','CEXP', 'CLOG', 'CSIN','CCOS','SINH','DSINH','COSH',
*        52      53      54      55     56     57     58      59
     + 'DCOSH','TANH', 'DTANH','CABS','AINT','DINT','ANINT',
*        60      61      62      63     64     65     66
     + 'CMPLX','AIMAG','CONJG','DPROD','DIM','DDIM','IDIM',
*        67      68      69      70     71     72     73
     + 'SIGN', 'DSIGN','ISIGN','ICHAR','IOR','IAND','IEOR',
*        74      75      76      77     78     79     80
     + 'ISHFT', 'BTEST','IBSET','IBCLR','NOT','NINT','ISHFTC',
*        81       82     83      84      85    86     87
     + 'IBITS', 'LOCF','LOCB'  /
*        88       89     90
      DATA NTIFUN/1,-2, 1,5, 1,-2, 1,5, 1,-2, 1,2, 1,5, 1,-2, 1,2,
*                  1     2    3     4    5     6    7    8     9
     + 1,5, 1,-2, 1,5, 1,-2, 1,5, 1,-2, 1,5, 1,-2, 1,5, 1,-2, 1,5,
*       10   11    12   13    14   15    16   17    18   19    20
     + 1,-2, 1,5, 2,-2, 2,5, 1,-2, 1,1, 1,5, 2,-1, 2,2, 2,5, 3,-1,
*       21    22   23    24   25    26   27   28    29   30   31
     + 3,1, 3,2, 3,5, 3,2, 3,1, 3,-1, 3,1, 3,2, 3,5, 3,2, 3,1,
*       32   33   34   35   36   37    38   39   40   41   42
     + 1,1, 2,1, 1,1, 1,1, 1,1, 1,2, 1,2, 1,2, 1,5    ,
*       43   44   45   46   47   48   49   50   51
     + 1,7, 1,7, 1,7, 1,7, 1,7, 1,-2,1,5, 1,-2,1,5, 1,-2, 1,5,
*       52   53   54   55   56   57   58   59   60   61    62
     + 1,2, 1,-2,1,5, 1,-2,1,7, 1,2, 1,7, 2,5, 2,-1,2,5,  2,1,
*       63   64   65   66   67   68   69   70   71   72    73
     + 2,-1,2,5, 2,1, 1,1, 2,1, 2,1, 2,1, 2,1, 2,1, 2,1, 2,1, 1,1,
*       74   75   76   77   78  79    80   81   82   83   84   85
     + 1,1, 3,1, 3,1, 1,1, 1,1  /
*       86   87   88   89   90
**************************
      J=MJSCHA(NAME)
      CALL CCOPYS(JID,J,KLENID)
      DO 1 I=1,KNIFUN
       IF(NAME.EQ.NMFUN(I))GO TO 2
 1    CONTINUE
      CSNIFN=0
      RETURN
 2    CSNIFN=I
      NPAR=NTIFUN(1,I)
      ITFUN=NTIFUN(2,I)
      END
