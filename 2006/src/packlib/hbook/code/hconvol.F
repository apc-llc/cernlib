*
* $Id: hconvol.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hconvol.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HCONVOL ( ID1, ID2, ID3, IERROR )
************************************************************************
*                                                                      *
*--   Action: Perform 1D convolution of ID2 with ID1 as kernel         *
*--           Result in ID3.                                           *
*                                                                      *
*--   Setup:  All histograms involved must exist before this call.     *
*--           ID2 and ID3 histograms must be 1D.                       *
*--           ID1 can be 1-D or 2-D                                    *
*                                                                      *
*    Author: Per Steinar Iversen  (PerSteinar.Iversen@fi.uib.no)       *
*                                                                      *
* NB: This method scales badly for large histograms. The best general  *
* algorithm would be to unpack the histograms, add a suitable number   *
* of zeros, do the two FFTs, multiply the transforms, do yet           *
* another FFT and stuff the resulting histogram back into HBOOK.       *
* However, for small histograms, the naive method is probably faster,  *
* especially if recoded in terms of lower level calls. It will also    *
* work with 1D and 2D kernel-histograms that do not have matched       *
* coordinate systems - the FFT method implies equal binsize in X and Y *
* for the kernel and the histogram to be folded; This simple method    *
* uses HBOOK to avoid this in one (X) dimension at least, corresponding*
* to folding in a constant resolution term.                            *
*                                                                      *
*   EXAMPLE of use                                                     *
*   ==============                                                     *
*      CALL HBOOK1(1,'Kernel 1 - 1D',100,-5.0,5.0,0.0)                 *
*      CALL HBOOK2(2,'Kernel 2 - 2D',100,-5.0,5.0,100,0.0,100.0,0.0)   *
*      CALL HBPRO(2,0.0)                                               *
*      CALL HBOOK2(3,'Kernel 3 - 2D',100,-5.0,5.0,100,0.0,100.0,0.0)   *
*      CALL HBPRO(3,0.0)                                               *
*      CALL HBOOK1(4,'Function',100,0.0,100.0,0.0)                     *
*      CALL HBOOK1(5,'Result 1',100,-10.0,110.0,0.0)                   *
*      CALL HBOOK1(6,'Result 2',100,-10.0,110.0,0.0)                   *
*      CALL HBOOK1(7,'Result 3',100,-10.0,110.0,0.0)                   *
*                                                                      *
*      DO 10 I=1,100000                                                *
*        CALL RANNOR(A,B)                                              *
*        CALL HFILL(1,A,0.0,1.0)                                       *
*        CALL HFILL(1,B,0.0,1.0)                                       *
*        CALL RANNOR(A,B)                                              *
*        CALL HFILL(2,A,100.0*RNDM(I),1.0)                             *
*        CALL HFILL(2,B,100.0*RNDM(I),1.0)                             *
*        CALL RANNOR(A,B)                                              *
*        CALL HFILL(3,A,100.0*SQRT(RNDM(I+1)),1.0)                     *
*        CALL HFILL(3,B,100.0*SQRT(RNDM(I+1)),1.0)                     *
*        X = 30.0*(RNDM(I)-0.5)+50.0                                   *
*        CALL HFILL(4,X,0.0,1.0)                                       *
*   10 CONTINUE                                                        *
*                                                                      *
*      CALL HCONVOL(1,4,5,IERROR)                                      *
*      CALL HCONVOL(2,4,6,IERROR)                                      *
*      CALL HCONVOL(3,4,7,IERROR)                                      *
************************************************************************
*
*--   Externals
*
      INTEGER ID1, ID2, ID3, IERROR
*
*--   Internals
*
      LOGICAL HEXIST
*
      INTEGER NX1, NY1, LOC1
      INTEGER NX2, NY2, LOC2
      INTEGER NX3, NY3, LOC3
      REAL    XMI1, YMI1, XMA1, YMA1
      REAL    XMI2, YMI2, XMA2, YMA2
      REAL    XMI3, YMI3, XMA3, YMA3
      CHARACTER*4 CHTITL
      INTEGER I, J
      REAL    X1, X2, X3
      REAL    Y1, Y2, Y3
      REAL    HI, HIJ
*
************************************************************************
*                                                                      *
*--   Test input data before anything else                             *
************************************************************************
*
*---  Check if the IDs exist
*
      IERROR = 0
*
      IF ( .NOT. HEXIST ( ID1 ) ) THEN
         CALL HBUG('Kernel histogram does not exist', 'HCONVOL', ID1)
         IERROR = 1
      ENDIF
*
      IF ( .NOT. HEXIST ( ID2 ) ) THEN
         CALL HBUG('Input histogram does not exist', 'HCONVOL', ID2 )
         IERROR = 1
      ENDIF
*
      IF ( .NOT. HEXIST ( ID3 ) ) THEN
         CALL HBUG('Output histogram does not exist', 'HCONVOL', ID3)
         IERROR = 1
      ENDIF
*
      IF ( IERROR .NE. 0 ) RETURN
*
*--   Check if the IDs are 1D (not strictly necessary for ID1)
*
      NWT = 0
      CALL HGIVE (ID1,CHTITL,NX1,XMI1,XMA1,NY1,YMI1,YMA1,NWT,LOC1)
      BWID1=(XMA1-XMI1)/FLOAT(NX1)
*
      CALL HGIVE (ID2,CHTITL,NX2,XMI2,XMA2,NY2,YMI2,YMA2,NWT,LOC2)
      BWID2=(XMA2-XMI2)/FLOAT(NX2)
      IF ( NY2 .NE. 0 ) THEN
         CALL HBUG ( 'Input histogram is not 1D', 'HCONVOL', ID2 )
         IERROR = 1
      ENDIF
*
      CALL HGIVE (ID3,CHTITL,NX3,XMI3,XMA3,NY3,YMI3,YMA3,NWT,LOC3)
      IF ( NY3 .NE. 0 ) THEN
         CALL HBUG ( 'Output histogram is not 1D', 'HCONVOL', ID3 )
         IERROR = 1
      ENDIF
*
      IF ( IERROR .NE. 0 ) RETURN
*
*--   Check if ID1 and ID2 are compatible when kernel is 2D
*
      IF ( NY1 .NE. 0 ) THEN
*
         IF ( NY1 .NE. NX1 ) THEN
            CALL HBUG ( 'Number of bins in kernel and input IDs '
     +      //'unequal', 'HCONVOL', ID1 )
            IERROR = 1
         ENDIF
*
         IF ( YMI1 .NE. XMI2 ) THEN
            CALL HBUG ( 'Lower limit of kernel and input IDs unequal',
     +      'HCONVOL', ID1 )
            IERROR = 1
         ENDIF
*
         IF ( YMA1 .NE. XMA2 ) THEN
            CALL HBUG ( 'Upper limit of kernel and input IDs unequal',
     +      'HCONVOL', ID1 )
            IERROR = 1
         ENDIF
*
      ENDIF
*
      IF ( IERROR .NE. 0 ) RETURN
*
*--   Clean up any previous results
*
      CALL HRESET ( ID3, ' ' )
*
************************************************************************
*                                                                      *
*--   Do the folding                                                   *
*                                                                      *
************************************************************************
*
*--   For each bin in ID2
*
      DO 20 I = 1, NX2
         X2 = XMI2+(FLOAT(I)-0.5)*BWID2
         Y2 = HI ( ID2, I )
*
*--   Sum contribution from kernel ID1 into ID3
*
         DO 10 J = 1, NX1
            IF ( NY1 .EQ. 0 ) THEN
               Y1 = HI ( ID1, J )
            ELSE
               Y1 = HIJ ( ID1, J, I )
            ENDIF
            X1 = XMI1+(FLOAT(J)-0.5)*BWID1
            X3 = X1 + X2
            Y3 = Y1 * Y2
            CALL HFILL ( ID3, X3, 0.0, Y3 )
   10    CONTINUE
   20 CONTINUE
*
      END
