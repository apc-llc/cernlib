*
* $Id: l3bitx.inc,v 1.1.1.1 1996/02/28 16:24:06 mclareni Exp $
*
* $Log: l3bitx.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:06  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_L3BITX_INC
#define CERNLIB_HEPDB_L3BITX_INC
*
*
* l3bitx.inc
*
#if defined(CERNLIB_APOLLO)
#include "hepdb/l3bits.inc"
      MSBIT (MZ,IZW,IZP) = OR (AND (IZW, NOT(LSHFT(1,IZP-1)) )
     +                             , LSHFT(AND(MZ,1),IZP-1) )
*
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = OR ( AND (IZW, NOT(
     +                   LSHFT (RSHFT(NOT(INT4(0)),32-NZB), IZP-1)))
     +        , RSHFT (LSHFT(RSHFT(MZ,IZQ-1),32-NZB), 33-IZP-NZB) )
*
      JBYTET (MZ,IZW,IZP,NZB) = AND (MZ,
     +                        RSHFT (LSHFT(IZW,33-IZP-NZB), 32-NZB) )
      JBYTOR (MZ,IZW,IZP,NZB) = OR  (MZ,
     +                        RSHFT (LSHFT(IZW,33-IZP-NZB), 32-NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = AND (IZW, NOT(RSHFT (
     +                          LSHFT(NOT(MZ),32-NZB), 33-IZP-NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = OR (IZW, RSHFT (
     +                               LSHFT(MZ,32-NZB), 33-IZP-NZB) )
#endif
*
* l3bitx.inc
*
#if defined(CERNLIB_CRAY)
#include "hepdb/l3bits.inc"
      MSBIT (MZ,IZW,IZP)     = (IZW .AND. SHIFT(MASK(65),IZP))
     +                     .OR. SHIFT(MZ.AND.MASK(127),IZP-1)
*
      MCBYT (MZ,IZQ,IZW,IZP,NZB)=(IZW.AND.SHIFT(MASK(64-NZB),IZP-1))
     +         .OR. SHIFT(SHIFTR(MZ,IZQ-1).AND.MASK(128-NZB),IZP-1)
*
      JBYTET (MZ,IZW,IZP,NZB) = MZ .AND. SHIFTR(IZW,IZP-1)
     +                             .AND. MASK(128-NZB)
      JBYTOR (MZ,IZW,IZP,NZB) = MZ .OR. (SHIFTR(IZW,IZP-1)
     +                             .AND. MASK(128-NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = IZW .AND.
     +                             SHIFT (MZ.OR. MASK(64-NZB), IZP-1)
      MBYTOR (MZ,IZW,IZP,NZB) = IZW .OR.
     +                             SHIFT (MZ.AND.MASK(128-NZB),IZP-1)
#endif
*
* l3bitx.inc
*
#if defined(CERNLIB_IBM)||defined(CERNLIB_IBMRT)||defined(CERNLIB_VAX)
#include "hepdb/l3bits.inc"
      MSBIT (MZ,IZW,IZP) = IOR (IAND (IZW, NOT(ISHFT(1,IZP-1)) )
     +                              , ISHFT(IAND(MZ,1),IZP-1))
*
      MCBYT (MZ,IZQ,IZW,IZP,NZB) = IOR ( IAND (IZW, NOT(
     +                   ISHFT (ISHFT(NOT(0),-32+NZB), IZP-1)))
     +        , ISHFT (ISHFT(ISHFT(MZ,-IZQ+1),32-NZB), -33+IZP+NZB) )
*
      JBYTET (MZ,IZW,IZP,NZB) = IAND (MZ, IBITS(IZW,IZP-1,NZB))
      JBYTOR (MZ,IZW,IZP,NZB) = IOR  (MZ, IBITS(IZW,IZP-1,NZB))
      MBYTET (MZ,IZW,IZP,NZB) = IAND (IZW, NOT(ISHFT (
     +                          ISHFT(NOT(MZ),32-NZB),-33+IZP+NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = IOR (IZW, ISHFT (
     +                               ISHFT(MZ,32-NZB),-33+IZP+NZB) )
#endif
*
* l3bitx.inc
*
#if defined(CERNLIB_F2C)
#include "hepdb/l3bits.inc"
      JBYTET (MZ,IZW,IZP,NZB) = AND (MZ,
     +                        ishftr (LSHIFT(IZW,33-IZP-NZB),32-NZB) )
      JBYTOR (MZ,IZW,IZP,NZB) = OR  (MZ,
     +                        ishftr (LSHIFT(IZW,33-IZP-NZB),32-NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = AND (IZW,
     +           NOT(ishftr (LSHIFT(NOT(MZ),32-NZB),33-IZP-NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = OR (IZW,
     +                    ishftr (LSHIFT(MZ,32-NZB),33-IZP-NZB) )
      BTEST(IZW,IZP) = AND (IZW,LSHIFT(1,IZP)) .NE. 0
      IBSET(IZW,IZP) =  OR (IZW,LSHIFT(1,IZP))
      IBCLR(IZW,IZP) = AND (IZW,NOT(LSHIFT(1,IZP)))
      IBITS (IZW,IZP,NZB) = ISHFTR (LSHIFT(IZW,32-IZP-NZB), 32-NZB)
 
#endif
*
* l3bitx.inc
*
#if (defined(CERNLIB_WINNT))&&(!defined(CERNLIB_F2C))
#include "hepdb/l3bits.inc"
      JBYTET (MZ,IZW,IZP,NZB) = IAND (MZ,
     +                        ISHFT (ISHFT(IZW,33-IZP-NZB),-32+NZB) )
      JBYTOR (MZ,IZW,IZP,NZB) = IOR  (MZ,
     +                        ISHFT (ISHFT(IZW,33-IZP-NZB),-32+NZB) )
      MBYTET (MZ,IZW,IZP,NZB) = IAND (IZW,
     +           NOT(ISHFT (ISHFT(NOT(MZ),32-NZB),-33+IZP+NZB)) )
      MBYTOR (MZ,IZW,IZP,NZB) = IOR (IZW,
     +                    ISHFT (ISHFT(MZ,32-NZB),-33+IZP+NZB) )
#endif

#endif
