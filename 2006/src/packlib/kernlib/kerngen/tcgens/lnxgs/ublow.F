*
* $Id: ublow.F,v 1.2 2006/09/15 09:35:24 mclareni Exp $
*
* $Log: ublow.F,v $
* Revision 1.2  2006/09/15 09:35:24  mclareni
* Submitted mods for gcc4/gfortran and MacOSX, corrected to work also on slc4 with gcc3.4 and 4.1
*
* Revision 1.1.1.1  1996/02/15 17:50:19  mclareni
* Kernlib
*
*
      SUBROUTINE UBLOW (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBLOW           .VERSION KERNLNX  1.02  940511
C ORIG. 03/02/89  K.M.STORR
C

      DIMENSION    MS(99), MT(99), NCHP(9)
      data iblan1/x'20202000'/
      data mask1/x'000000ff'/

      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
#if !defined(CERNLIB_GFORTRAN)
   11 NWS    = ishftr (NCH,2)
      NTRAIL = AND (NCH,3)
#else
   11 NWS    = ishft (NCH,-2)
      NTRAIL = iAND (NCH,3)
#endif
      JT     = 0
      IF (NWS.EQ.0)          GO TO 26

C--                Unpack the initial complete words

      DO 24 JS=1,NWS
      MWD      = MS(JS)
#if !defined(CERNLIB_GFORTRAN)
      MT(JT+1) = OR (IBLAN1,AND(MASK1,MWD))
      MT(JT+2) = OR (IBLAN1,AND(MASK1,ishftr(MWD,8)))
      MT(JT+3) = OR (IBLAN1,AND(MASK1,ishftr(MWD,16)))
      MT(JT+4) = OR (IBLAN1,          ishftr(MWD,24) )
#else
      MT(JT+1) = iOR (IBLAN1,iAND(MASK1,MWD))
      MT(JT+2) = iOR (IBLAN1,iAND(MASK1,ishft(MWD,-8)))
      MT(JT+3) = iOR (IBLAN1,iAND(MASK1,ishft(MWD,-16)))
      MT(JT+4) = iOR (IBLAN1,           ishft(MWD,-24) )
#endif
   24 JT = JT + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Unpack the trailing word

   26 MWD = MS(NWS+1)

      DO 28 JS=1,NTRAIL
#if !defined(CERNLIB_GFORTRAN)
      MT(JT+1) = OR (IBLAN1,AND(MASK1,MWD))
      MWD = ishftr (MWD,8)
#else
      MT(JT+1) = iOR (IBLAN1,iAND(MASK1,MWD))
      MWD = ishft (MWD,-8)
#endif
   28 JT = JT + 1
   29 RETURN

   91 CALL ABEND
      END
