*
* $Id: htdiffb.F,v 1.4 2002/06/25 15:14:03 couet Exp $
*
* $Log: htdiffb.F,v $
* Revision 1.4  2002/06/25 15:14:03  couet
* - Protection added in case the arrays index X goes outside the arrays limits
*
* Revision 1.3  1997/12/03 12:47:17  couet
* - The initialisation of the vector FLLARY was missing
*
* Revision 1.2  1996/06/06 09:52:48  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
*CMZ :  4.17/02 18/12/92  12.28.50  by  Rene Brun
*-- Author :
      PROGRAM HTDIFFB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DEMONSTRATE C AND S OPTIONS OF HDIFFB
C-
C-
C-   Inputs  : none
C-   Outputs : 6 histograms, the 4 used in the 3 example calls, and
C-             the 3 DIFFS arrays packed into histograms.
C-   Controls: none
C-
C-   Created  26-OCT-1992   R J Genik II
C-
C----------------------------------------------------------------------
      INTEGER NWPAWC,X,NBAD,Q,MINBIN,MAXBIN,NBINS,MU
      REAL TOL,H,DIFFS,FLLARY,AMU,ID,RN32,Z
      PARAMETER (NWPAWC = 100000)
      PARAMETER (MU     = 100)
      PARAMETER (TOL    = 0.05)
      PARAMETER (NBINS  = 100)
      PARAMETER (MINBIN = 1)
      PARAMETER (MAXBIN = 100)
      COMMON/PAWC/H(NWPAWC)
      DIMENSION DIFFS(MINBIN:MAXBIN),FLLARY(MINBIN:MAXBIN)
C- Initialise HBOOK
      AMU = REAL(MU)
      CALL HLIMIT(NWPAWC)
C- Create the histograms
      CALL HBOOK1(10,'DATA HISTOGRAM 1',NBINS,REAL(MINBIN),
     +  REAL(MAXBIN+1),0.)
      CALL HBOOK1(20,'DATA HISTOGRAM 2',NBINS,REAL(MINBIN),
     +  REAL(MAXBIN+1),0.)
      CALL HBOOK1(30,'REFERENCE HISTOGRAM',NBINS,REAL(MINBIN),
     +  REAL(MAXBIN+1),0.)
      CALL HBOOK1(40,'LINEARLY INCREASING HISTOGRAM',NBINS,REAL(MINBIN),
     +  REAL(MAXBIN+1),0.)
      CALL HBOOK1(100,'DIFFS VALUES FOR S OPTION ID1=10, ID2=20',NBINS
     +  ,REAL(MINBIN), REAL(MAXBIN+1),0.)
      CALL HBOOK1(200,'DIFFS VALUES FOR C OPTION ID1=30, ID2=20',NBINS
     +  ,REAL(MINBIN), REAL(MAXBIN+1),0.)
      CALL HBOOK1(300,'DIFFS VALUES FOR C OPTION ID1=30, ID2=40',NBINS
     +  ,REAL(MINBIN),REAL(MAXBIN+1),0.)
C-  Initialise the vector FLLARY
      DO 10 I=MINBIN,MAXBIN
         FLLARY(I) = 0.
   10 CONTINUE
C-  Create two pseudo uniform random distributions to compare
      DO 30 Q=1,MU*(MAXBIN-MINBIN+1)
        X = RN32(ID)*(MAXBIN-MINBIN+1)
        X = MAX(X,MINBIN)
        X = MIN(X,MAXBIN)
        FLLARY(X) = FLLARY(X) + 1.
   30 CONTINUE
      CALL HPAK(10,FLLARY)
      DO 35 Q=MINBIN,MAXBIN
        FLLARY(Q) = 0.
   35   CONTINUE
        DO 40 Q=1,MU*(MAXBIN-MINBIN+1)
          X = RN32(ID)*(MAXBIN-MINBIN+1)
          X = MAX(X,MINBIN)
          X = MIN(X,MAXBIN)
          FLLARY(X) = FLLARY(X) + 1.
   40   CONTINUE
        CALL HPAK(20,FLLARY)
C- Create 2 more histograms, one with x=mu, other linearly increasing
        DO 50 Q=MINBIN,MAXBIN
          FLLARY(Q) = AMU
   50   CONTINUE
        CALL HPAK(30,FLLARY)
        Z = 50.
        DO 60 Q = MINBIN,MAXBIN
          FLLARY(Q) = Z
          Z = Z + 1.
   60   CONTINUE
        CALL HPAK(40,FLLARY)
C- HDIFFB calling sequence
        CALL HDIFFB(10,20,TOL,NBINS,'S',NBAD,DIFFS)
        CALL HPAK(100,DIFFS)
        CALL HDIFFB(30,20,TOL,NBINS,'C',NBAD,DIFFS)
        CALL HPAK(200,DIFFS)
        CALL HDIFFB(30,40,TOL,NBINS,'CN',NBAD,DIFFS)
        CALL HPAK(300,DIFFS)
C- Print results, use various options for clarity
        CALL HIDOPT(0,'NPST')
        CALL HIDOPT(0,'NPLO')
        CALL HIDOPT(30,'1EVL')
        CALL HPRINT(10)
        CALL HPRINT(20)
        CALL HPRINT(100)
        CALL HPRINT(30)
        CALL HPRINT(200)
        CALL HPRINT(40)
        CALL HPRINT(300)
        STOP
        END
