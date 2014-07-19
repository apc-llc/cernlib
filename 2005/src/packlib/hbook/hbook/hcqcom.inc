*
* $Id: hcqcom.inc,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: hcqcom.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCQCOM_INC
#define CERNLIB_HBOOK_HCQCOM_INC
*
*
* hcqcom.inc
*
*CMZ :  4.19/00 13/04/93  16.36.40  by  Rene Brun
*-- Author :
* Basic parameters and data for multiquadric fit.
* VPRINT is true if verbose option selected.
      LOGICAL VPRINT
* NTUPLE is .TRUE. if ntuple is being processed, otherwise .FALSE.
      LOGICAL NTUPLE
* IMQFUN function flag: = 0, no function evaluated yet.
*                       = 1, linear sum of multiquadrics.
*                       = 2, exponential of linear sum of multiquadrics.
* (Note: HQF... routines take account of this, HQD... always calculate the
*    simple sum of multiquadrics.)
      INTEGER IMQFUN
* IDMQ stores current histogram no. when required.
      INTEGER IDMQ
* JMQ, KMQ store current multiquadric basis function nos. when required.
      INTEGER JMQ, KMQ
* NSIG is number of significant points.
      INTEGER NSIG
* NFREE is number of free parameters in the fitting (often equal to NSIG).
      INTEGER NFREE
* Limits and recommended values of no. of significant points.
      INTEGER NSMIN, NSMIN1, NSMIN2, NSMIN3, NSREC1, NSREC2, NSREC3
      PARAMETER (NSMIN1 = 5, NSMIN2 = 10, NSMIN3 = 10)
      PARAMETER (NSREC1 = 15, NSREC2 = 50, NSREC3 = 50)
* NSMAX its maximum no. of significant points.
      INTEGER NSMAX
      PARAMETER (NSMAX = 500)
* NDIM is the number of dimensions - NDMAX is its maximum.
      INTEGER NDIM, NDMAX
      PARAMETER (NDMAX = 3)
* NDIMC is dimensionality of data currently under consideration (<= NDIM).
* NDIMWH is which of the NDIM1 dimensions are currently being considered.
      INTEGER NDIMC, NDIMWH (NDMAX)
* NBINS is no. of bins in each coordinate, NBTOT the total
*   no. of bins in current histogram.
      INTEGER NBINS (NDMAX)
      INTEGER NX, NY, NZ, NBTOT
      EQUIVALENCE (NX, NBINS (1)), (NY, NBINS (2)), (NZ, NBINS (3))
* Similarly, NBINSN is no. of bins for which first significant points where
*   found when surveying Ntuple data.
      INTEGER NBINSN (NDMAX)
      INTEGER NXN, NYN, NZN
      EQUIVALENCE (NXN, NBINSN (1)), (NYN, NBINSN (2)),
     +(NZN, NBINSN (3))
* NNBINn is no. of bins used to bin ntuple data in n-dimensions.
      INTEGER NNBIN1, NNBIN2, NNBIN3
      PARAMETER (NNBIN1 = 100, NNBIN2 = 40, NNBIN3 = 40)
* NMQEVS is no. of events (see also WMQEVS).
      INTEGER NMQEVS
* NMQFUL is no. of histogram bins with non-zero content.
      INTEGER NMQFUL
* NINTVS, NQUADS are no. of intervals and no. of gaussian quadrature points
*   per interval for integration of multiquadric functions.
      INTEGER NINTVS, NQUADS
      PARAMETER (NINTVS = 50, NQUADS = 8)
* LAREA is working space for ZEBRA link maintenance.
* LHQUAD is pointer to parent bank in  Q in /PAWC/ in SEQ HCBOOK.
*  (For bank structure see HQINIT header.)
* LnH and LnV are pointers to current n-D histogram contents and variances.
* LHQCOV is pointer to covariances.
* LHQDER is working bank for double derivatives.
* LHQWK1/2.. are more working banks.
* LHQDJN is pointer to temporary bank holding function values.
* LLAST is a dummy pointer - always last!
      INTEGER LAREA (2), LHQUAD, L1H, L1V, L2H, L2V, L3H, L3V,
     +LHQCOV, LHQDER, LHQWK1, LHQWK2, LHQWK3,
     +LHQDJN, LLAST
*
* SIGGRD contains the gradient of the function with respect to the strengths.
* SIGDEN the (approximate) density of the function at the significant point.
      REAL SIGGRD (NSMAX)
      REAL SIGDEN (NSMAX)
* SIGV contains significant points' coordinate vectors.
      REAL SIGV (NSMAX, NDMAX)
      REAL SIGX (NSMAX), SIGY (NSMAX), SIGZ (NSMAX)
      EQUIVALENCE (SIGX, SIGV), (SIGY, SIGV (1, 2)), (SIGZ, SIGV (1, 3))
* SIGDEL contains the scale parameter Delta for each coordinate.
      REAL SIGDEL (NSMAX)
* SIGVMI, SIGVMA, SIGVBI, SIGVT are lower and upper limits, bin size
*   and total range.
      REAL SIGVMI (NDMAX), SIGVMA (NDMAX), SIGVBI (NDMAX), SIGVT (NDMAX)
      REAL XMI, XMA, DX, DXT, YMI, YMA, DY, DYT, ZMI, ZMA, DZ, DZT
      EQUIVALENCE (XMI, SIGVMI (1)), (XMA, SIGVMA (1))
      EQUIVALENCE (DX,  SIGVBI (1)), (DXT, SIGVT (1))
      EQUIVALENCE (YMI, SIGVMI (2)), (YMA, SIGVMA (2))
      EQUIVALENCE (DY,  SIGVBI (2)), (DYT, SIGVT (2))
      EQUIVALENCE (ZMI, SIGVMI (3)), (ZMA, SIGVMA (3))
      EQUIVALENCE (DZ,  SIGVBI (3)), (DZT, SIGVT (3))
* BINV is used to store current coordinates - usually bin centres, but can
*   be edges 0. or 1.
      REAL BINV (NDMAX)
      REAL XBIN, YBIN, ZBIN
      EQUIVALENCE (XBIN, BINV (1)), (YBIN, BINV (2)), (ZBIN, BINV (3))
* VOLTOT is total "volume" and VOLBIN is "volume" of bin in current histogram.
      REAL VOLTOT, VOLBIN
* WMQEVS is weighted no. of events (see also NMQEVS).
      REAL WMQEVS
* VSCALE scales variances (only if incorrectly assigned errors suspected).
      REAL VSCALE
* VMEAN1, VMEAN2 are mean variance per histogram entry and its square.
      REAL VMEAN1, VMEAN2
* VCONST is a constant multiplying exponential of sum of multiquadrics for
*   Poisson likelihood fitting.
      REAL VCONST
* SENSIT is a sensitivity parameter - normally 1 - which controls the
*   selection of significant points.
      REAL SENSIT
* SLCUT is current value of cut for Laplacian significance.
      REAL SLCUT
* SLRMS is rms of Laplacian significance.
      REAL SLRMS
* SPREAD is a smoothness parameter - normally 1 - which multiplies the
*   scale parameter (radius of curvature).
      REAL SPREAD
* SMALLD is a "small" value of the multiquadric parameter DELTA for edges, etc.
*    (usually set to 1.E-6 in HQUAD, HQUADN).
      REAL SMALLD
* HQMIN, HQMAX, HQFMIN, HQFMAX are current minimum and maximum bin heights and
*   function values (at bin centres).
      REAL HQMIN, HQMAX, HQFMIN, HQFMAX
*
      COMMON /HCQCOM/ SIGGRD, SIGDEN, SIGV, SIGDEL,
     + SIGVMI, SIGVMA, SIGVBI, SIGVT,
     + BINV, VOLTOT, VOLBIN, WMQEVS, VSCALE, VMEAN1, VMEAN2,
     + VCONST, SENSIT, SLCUT, SLRMS, SPREAD, SMALLD,
     + HQMIN, HQMAX, HQFMIN, HQFMAX,
     + IMQFUN, IDMQ, JMQ, KMQ,
     + NSIG, NFREE, NSMIN, NDIM, NDIMC, NDIMWH,
     + NBINS, NBTOT, NBINSN, NMQEVS, NMQFUL,
     + LAREA, LHQUAD, L1H, L1V, L2H, L2V, L3H, L3V,
     + LHQCOV, LHQDER, LHQWK1, LHQWK2, LHQWK3,
     + LHQDJN, LLAST,
     + VPRINT, NTUPLE
 

#endif
