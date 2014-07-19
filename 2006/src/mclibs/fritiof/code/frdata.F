*
* $Id: frdata.F,v 1.1.1.1 1996/01/11 14:05:22 mclareni Exp $
*
* $Log: frdata.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:22  mclareni
* Fritiof
*
*
C*********************************************************************
 
C*********************************************************************
C********************************* DATA FRDATA ***********************
 
      BLOCK DATA FRDATA
      PARAMETER (KSZ1=20)
      CHARACTER*4 PACD
      COMMON/FRPARA1/KFR(KSZ1),VFR(KSZ1)
      COMMON/FRCODES/IPT(2),PACD(27),NNUC(27),NPROT(27),KCD(27)
     >           ,RO1(27,2),EXMA(9,2)
      SAVE /FRPARA1/,/FRCODES/
 
      DATA KFR/1,1,0,1,0, 2,1,1,1,1, 4,2,0,0,0, 5*0/
 
      DATA VFR/0.,0.2,0.8,0.2,0.1, 0.01,0.30,.75,.75,0.,
     >         0.,1.0,0.167,.333,.5, .5,4*0./
 
C.....The following are particles in store.  In particulare RO1 are the
C.....parameters for nuclei density.  For A<,=16, shell model harmonic
C.....oscilator density, RO1(j,1) gives the nuclear root-mean-square-
C.....(charge) radius.  For A>16 RO1(j,2) gives the two parameters r0 and C
C.....to the Wood-Saxon density.
C.....EXMA(J,1) AND EXMA(J,2) correspond to the "minimum excitation mass"
C.....and "diffractive mass" respectively.
 
      DATA PACD/'NEW1','NEW2','PI+ ','PI- ','K+  ','K-  ','N   ','P   '
     >         ,'PBAR','D   ','HE  ','BE  ','B   ','C   ','O   ','AL  '
     >         ,'SI  ','S   ','AR  ','CA  ','CU  ','AG  ','XE  ','W   '
     >         ,'AU  ','PB  ','U   '/
      DATA NNUC/  1,     1,     1,     1,     1,     1,     1,     1,
     >            1,     2,     4,     9,    11,    12,    16,    27,
     >           28,    32,    40,    40,    64,   108,   131,   184,
     >          197,   207,   238/
      DATA NPROT/ 0,    0,      1,    -1,     1,    -1,     0,     1,
     >           -1,    1,      2,     4,     5,     6,     8,    13,
     >           14,   16,     18,    20,    29,    47,    54,    74,
     >           79,    82,    92 /
      DATA KCD /  0,    0,    211,  -211,   321,   -321, 2112,  2212,
     >        -2212,  18*0 /
 
C.....Source for RO1: RO(j,1) for A<17 is from ref. BJ; and RO(j,1-2) for
C.....A>17 are taken from FRITIOF 6.0, where RO(j,1)=r0=1.16*(1.-1.16/A**(2/3)):
 
      DATA RO1 / 0.,    0.,    0.,    0.,     0.,    0.,    0.,    0.,
     1           0., 2.095,  1.74, 2.519,  2.37, 2.446, 2.724,   1.01,
     1        1.014, 1.027, 1.045, 1.045, 1.076, 1.101, 1.108,  1.118,
     1        1.120, 1.122, 1.125,
     2           0.,    0.,    0.,    0.,     0.,    0.,    0.,    0.,
     2           0.,    0.,    0.,    0.,     0.,    0.,    0., 0.478,
     2        0.480, 0.490, 0.490, 0.490,  0.490, 0.495,  0.52, 0.530,
     2        0.540, 0.545,  0.55 /
 
C.....RO1 follows are taken from experimental measurements from ref. BJ,
C.....and some unfound in the book are hand-picked. Not going to be used.
C
C      DATA RO1 / 0.,    0.,    0.,    0.,     0.,    0.,    0.,    0.,
C     1           0., 2.095,  1.74, 2.519,  2.37, 2.446, 2.724,  0.947,
C     1        1.035, 1.016,  1.00, 1.023, 1.068, 1.119, 1.114,  1.116,
C     1        1.109, 1.121, 1.123,
C     2           0.,    0.,    0.,    0.,     0.,    0.,    0.,    0.,
C     2           0.,    0.,    0.,    0.,     0.,    0.,    0., 0.569,
C     2        0.537, 0.540, 0.543,  0.55,  0.579, 0.523,  0.52, 0.525,
C     2        0.535, 0.535,  0.55 /
 
      DATA EXMA/0.00,  0.00,   0.14,   0.14,  0.50,  0.50,  0.94,  0.94,
     1          0.94,
     2          0.00,  0.00,   0.40,   0.40,  0.75,  0.75,  1.20,  1.20,
     2          1.20 /
 
C.....Reference. BJ:  R.C. Barrett and D.F.Jackson,
C.....Nuclear Sizes and Structure. However Ca, S, Ar, Xe, W can not be found
C.....in the book.
 
      END
