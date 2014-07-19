*
* $Id: l3bits.inc,v 1.1.1.1 1996/02/28 16:24:06 mclareni Exp $
*
* $Log: l3bits.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:06  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_L3BITS_INC
#define CERNLIB_HEPDB_L3BITS_INC
*
*
* l3bits.inc
*
#if defined(CERNLIB_APOLLO)
      IBITS (I,N,L)      = RSHFT (LSHFT(I,32-N-L), 32-L)
*
      JBIT (IZW,IZP)     = AND (RSHFT(IZW,IZP-1), 1)
      JBYT (IZW,IZP,NZB) = IBITS (IZW,IZP-1,NZB)
*
      MSBIT0 (IZW,IZP)   = AND (IZW, NOT(LSHFT(1,IZP-1)) )
      MSBIT1 (IZW,IZP)   = OR  (IZW,     LSHFT(1,IZP-1)  )
      MSBYT (MZ,IZW,IZP,NZB) = OR ( AND (IZW, NOT(
     +                   LSHFT (RSHFT(NOT(INT4(0)),32-NZB), IZP-1)))
     +                     , RSHFT (LSHFT(MZ,32-NZB), 33-IZP-NZB) )
#endif
*
* l3bits.inc
*
#if defined(CERNLIB_CRAY)
      JBIT (IZW,IZP)     = SHIFTR(IZW,IZP-1) .AND. MASK(127)
      JBYT (IZW,IZP,NZB) = SHIFTR(IZW,IZP-1) .AND. MASK(128-NZB)
*
      MSBIT0 (IZW,IZP)       =  IZW .AND. SHIFT(MASK(65),IZP)
      MSBIT1 (IZW,IZP)       =  IZW .OR.  SHIFT(MASK(1), IZP)
      MSBYT (MZ,IZW,IZP,NZB) = (IZW .AND. SHIFT(MASK(64-NZB),IZP-1))
     +                       .OR. SHIFT(MZ.AND.MASK(128-NZB),IZP-1)
#endif
*
* l3bits.inc
*
#if defined(CERNLIB_HPUX)
      JBIT (IZW,IZP)     = IBITS (IZW,IZP-1,1)
      JBYT (IZW,IZP,NZB) = IBITS (IZW,IZP-1,NZB)
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_IBMRT)||defined(CERNLIB_VAX)
      IBITS (I,N,L)      = ISHFT(ISHFT(I,32-L-N),L-32)
*
      JBIT (IZW,IZP)     = IBITS (IZW,IZP-1,1)
      JBYT (IZW,IZP,NZB) = IBITS (IZW,IZP-1,NZB)
*
      MSBIT0 (IZW,IZP)   = IAND (IZW, NOT(ISHFT(1,IZP-1)) )
      MSBIT1 (IZW,IZP)   = IOR  (IZW,     ISHFT(1,IZP-1)  )
      MSBYT (MZ,IZW,IZP,NZB) = IOR ( IAND (IZW, NOT(
     +                   ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +                     , ISHFT (ISHFT(MZ, 32-NZB), -33+IZP+NZB) )
#endif
*
* l3bits.inc
*
#if defined(CERNLIB_F2C)
C--
C--    Note: F2C-ish RSHIFT is arithmetic right shift.
C--          This means sign bits are shifted in at the high end
C
      ISHFTL(IZW,NZB) = LSHIFT(IZW,NZB)
      JBIT (IZW,IZP)      = AND(RSHIFT(IZW,IZP-1),1)
      JBYT (IZW,IZP,NZB) =  ISHFTR(LSHIFT(IZW,33-IZP-NZB),32-NZB)
      MSBIT0 (IZW,IZP)   = AND (IZW, NOT(LSHIFT(1,IZP-1)) )
      MSBIT1 (IZW,IZP)   = OR  (IZW,     LSHIFT(1,IZP-1)  )
      MSBIT (MZ,IZW,IZP) = OR (AND (IZW, NOT(LSHIFT(1,IZP-1)) )
     +                              , LSHIFT(AND(MZ,1),IZP-1))
#endif
*
* l3bits.inc
*
#if (defined(CERNLIB_WINNT))&&(!defined(CERNLIB_F2C))
      JBIT (IZW,IZP)     =  IAND (ISHFT(IZW,1-IZP), 1)
      JBYT (IZW,IZP,NZB) = ISHFT (ISHFT(IZW,33-IZP-NZB), -32+NZB)
      MSBIT0 (IZW,IZP)   = IAND (IZW, NOT(ISHFT(1,IZP-1)) )
      MSBIT1 (IZW,IZP)   = IOR  (IZW,     ISHFT(1,IZP-1)  )
      MSBIT (MZ,IZW,IZP) = IOR (IAND (IZW, NOT(ISHFT(1,IZP-1)) )
     +                              , ISHFT(IAND(MZ,1),IZP-1))
      MSBYT (MZ,IZW,IZP,NZB) = IOR (
     +          IAND (IZW, NOT(ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +        , ISHFT (ISHFT(MZ,32-NZB), -33+IZP+NZB) )
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = IOR (
     +          IAND (IZW, NOT(ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +        , ISHFT (ISHFT(ISHFT(MZ,-IZQ+1),32-NZB), -33+IZP+NZB) )
#endif

#endif
