*
* $Id: cexam09.F,v 1.1.1.1 1996/02/28 16:23:52 mclareni Exp $
*
* $Log: cexam09.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      PROGRAM CEXAM09
*     ===============
*
************************************************************************
*                                                                      *
*        PROG. CEXAM09                                                 *
*                                                                      *
*   Program to create a fictitious database to test the batch option   *
*   DBENTB and compare its performance against standard CDSTOR         *
*                                                                      *
*   This program uses a different store from /GCBANK/ and hence both   *
*   the user division and the ZEBRA system division refer to the new   *
*   store /CSTOR9/ and muliple save ala CDSTOM                         *
*                                                                      *
************************************************************************
*
      PARAMETER       (NLAST=150000, NDIV1=5000)
      COMMON /CSTOR9/ FENCCS(22), LQ(NLAST)
      DIMENSION       IQ(2), Q(2)
      EQUIVALENCE     (IQ(1),Q(1),LQ(9))
*
      COMMON /QUEST/  IQUEST(100)
*
#include "cuser9.inc"
      PARAMETER       (NKEX=NKEYCU-NOF2CU, NOFF1=10, NOFFP=6)
      CHARACTER       CHTAG(NKEX)*8, CHFOR*(NKEX), CHD*2
      DATA            CHTAG /'US_KEY_1'/, CHFOR /'I'/
*
*     ------------------------------------------------------------------
*
* *** Open the RZ-file with an Open Statement (or JCL, etc)
*
      LUNRZ  = 1
      LPRTCU = 6
      NREC   = 1000
      LREC   = 1024
      NBYTE  = 4*LREC
#if defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'ZO', 'DBTEST.DATA.A4', NBYTE, IOERR)
#endif
#if !defined(CERNLIB_IBM)
      CALL CDROPN (LUNRZ, 'ZO', 'DBTEST.DAT', NBYTE, IOERR)
#endif
      IF (IOERR.NE.0)        GO TO 999
      CALL MZEBRA(-1)
*
* *** Open the 1 and only 1 Store used in the reconstruction
*
      CALL MZSTOR (ISTOCU, '/CSTOR9/', ' ', FENCCS(1), LQ(1), LQ(1)
     +           , LQ(1), LQ(NDIV1), LQ(NLAST))
*
* *** Log-Level
*
      CALL MZLOGL (ISTOCU, 0)
*
* *** Define user division and link area like:
*
      CALL MZDIV  (ISTOCU, IDIVCU, 'DB-USERS', 50000, 100000, 'L')
      CALL MZLINK (ISTOCU, '/CUSER9/', LDUVCU, LUSECU(NOBJCU), LDUVCU)
*
* *** Expand the system-division to its maximum
*
      LARGE  = NLAST/2
      IDIVS  = IDIVCU - 1
      CALL MZNEED (IDIVS, LARGE, 'G')
      ND     = LARGE + IQUEST(11) - 20
      IF (ND.GT.0) THEN
        CALL MZBOOK (IDIVS, L, 0, 2, 'DUMY', 0, 0, ND, 2, -1)
        CALL MZDROP (IDIVS, L, ' ')
      ENDIF
*
* *** Initialize DB-Package
*
      CALL TIMEST (999999.)
      NPAIR = 1
      LUFZ  = 0
      NTOP  = 0
      CALL CDINIT (IDIVCU, LUNRZ, LUFZ, 'DBL3', NPAIR, NREC, NTOP,
     +             'UZ', IRC)
#if !defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  0, ' ', IRC)
#endif
#if defined(CERNLIB__DEBUG)
      CALL CDLOGL ('DBL3',  2, ' ', IRC)
#endif
*
* *** Generate the list of objects to be inserted
*
      IDEND  = 901231
      IDBGN  = 880617
      ITIME  = 0
      CALL CDPKTS (IDEND, ITIME, INFIN, IRC)
      CALL CDPKTS (IDBGN, ITIME, IDTIM, IRC)
      IPVSN  = 1
      XRANG  = 500.
      XCENT  = 1000.
      NDAT   = 48
      DO 10 I = 1, NOBJCU
        KEYU   = MOD (I, NKYNCU)
        IF (KEYU.EQ.0) THEN
          KEYU   = NKYNCU
          IDBGN  = IDBGN + 1
          CALL CDPKTS (IDBGN, ITIME, IDTIM, IRC)
        ENDIF
        KEYSCU(NOFF1+1 ,I) = IDTIM
        KEYSCU(NOFF1+2 ,I) = INFIN
        KEYSCU(NOFFP   ,I) = IPVSN
        KEYSCU(NOF2CU+1,I) = KEYU
        CALL MZBOOK (IDIVCU, LUSECU(I), LUSECU(I), 2, 'UVDS', 0, 0, NDAT
     +             , 3, 0)
        DO 5 J = 1, NDAT
          Q(LUSECU(I)+J) = XRANG*RNDM(Q) - XCENT
    5   CONTINUE
   10 CONTINUE
*
* *** Create the two directories
*
      CHD    = 'C'
      IPREC  = 3
      DELTA  = 0.
      CALL CDMDIR ('//DBL3/DBMU/MCALB/UVD1', NKEX, CHFOR,  CHTAG, 20,
     +             IPREC, DELTA, CHD, IRC)
      CALL CDMDIR ('//DBL3/DBMU/MCALB/UVD2', NKEX, CHFOR,  CHTAG, 20,
     +             IPREC, DELTA, CHD, IRC)
      CALL CDMDIR ('//DBL3/DBMU/MCALB/UVD3', NKEX, CHFOR,  CHTAG, 20,
     +             IPREC, DELTA, CHD, IRC)
      CALL CDMDIR ('//DBL3/DBMU/MCALB/UVD4', NKEX, CHFOR,  CHTAG, 20,
     +             IPREC, DELTA, CHD, IRC)
*
* *** Now use 4 methods for saving the datasets
*
      CALL CDUVD1
#if defined(CERNLIB__DEBUG)
      CALL DZSNAP ('UVD1', IDIVCU, 'LMT')
      CALL DZSNAP ('UVD1', IDIVCU-1, 'LMT')
#endif
      CALL CDUVD2
#if defined(CERNLIB__DEBUG)
      CALL DZSNAP ('UVD2', IDIVCU, 'LMT')
      CALL DZSNAP ('UVD2', IDIVCU-1, 'LMT')
#endif
      CALL CDUVD3
#if defined(CERNLIB__DEBUG)
      CALL DZSNAP ('UVD3', IDIVCU, 'LMT')
      CALL DZSNAP ('UVD3', IDIVCU-1, 'LMT')
#endif
      CALL CDUVD4
#if defined(CERNLIB__DEBUG)
      CALL DZSNAP ('UVD4', IDIVCU, 'LMT')
      CALL DZSNAP ('UVD4', IDIVCU-1, 'LMT')
#endif
*
* *** Test the data objects stored
*
      CALL CDUVDT
#if defined(CERNLIB__DEBUG)
*
* *** Debug print out
*
      DO 20 K = 1, NOBJCU
        IF (MOD(K,NKYNCU).EQ.1)
     +    CALL DZSHOW ('UVDS    ', IDIVCU, LUSECU(K), 'B', 0, 0, 0, 0)
   20 CONTINUE
#endif
*
* *** Close Data Base
*
      CALL CDEND  ('*', 'A', IRC)
*                                                            END CEXAM09
  999 END
