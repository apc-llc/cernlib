*
* $Id: ubunch.F,v 1.2 2006/09/15 09:35:24 mclareni Exp $
*
* $Log: ubunch.F,v $
* Revision 1.2  2006/09/15 09:35:24  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:20  mclareni
* Kernlib
*
*
      SUBROUTINE UBUNCH (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBUNCH          .VERSION KERNLNX  1.02  940511
C ORIG. 03/02/89 K.M.STORR
C

      DIMENSION    MS(99), MT(99), NCHP(9)
      data iblan1/x'20202020'/
      data mask1/x'000000ff'/

      NCH = NCHP(1)
      IF   (NCH)             91,39,11
#if !defined(CERNLIB_GFORTRAN)
   11 NWT    = ishftr (NCH,2)
      NTRAIL = AND (NCH,3)
#else
   11 NWT    = ishft (NCH,-2)
      NTRAIL = iAND (NCH,3)
#endif
      JS     = 0
      IF (NWT.EQ.0)          GO TO 31

C--                Pack the initial complete words

      DO 24  JT=1,NWT
#if !defined(CERNLIB_GFORTRAN)
      MT(JT) = OR (OR (OR (
     +                  AND(MS(JS+1),MASK1),
     +          LSHIFT (AND(MS(JS+2),MASK1), 8)),
     +          LSHIFT (IAND(MS(JS+3),MASK1),16)),
     +          LSHIFT      (MS(JS+4),       24) )
#else
      MT(JT) = iOR (iOR (iOR (
     +                  iAND(MS(JS+1),MASK1),
     +          iSHFT  (iAND(MS(JS+2),MASK1), 8)),
     +          iSHFT  (IAND(MS(JS+3),MASK1),16)),
     +          iSHFT       (MS(JS+4),       24) )
#endif
   24 JS = JS + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Pack the trailing word

   31 MWD = IBLAN1
      JS  = NCH

      DO 34 JT=1,NTRAIL
#if !defined(CERNLIB_GFORTRAN)
      MWD = OR (LSHIFT(MWD,8), AND(MS(JS),MASK1))
#else
      MWD = iOR (iSHFT(MWD,8), iAND(MS(JS),MASK1))
#endif
   34 JS  = JS - 1
      MT(NWT+1) = MWD
   39 RETURN

   91 CALL ABEND
      END
