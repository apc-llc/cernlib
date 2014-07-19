*
* $Id: hfcnmin.F,v 1.1.1.1 1996/01/16 17:08:07 mclareni Exp $
*
* $Log: hfcnmin.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:07  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/02 26/04/94  22.25.05  by  Unknown
*-- Author :    Roger Barlow, Christine Beeston   24/09/93
 
       SUBROUTINE HFCNMIN(NPAR,GRAD,FVAL,XVAL,IFLAG,FUTIL)
 
#include "hbook/hcmcpm.inc"
 
       INTEGER NPAR,IFLAG
#if defined(CERNLIB_DOUBLE)
       DOUBLE PRECISION GRAD(*),FVAL,XVAL(*),FUTIL
       DOUBLE PRECISION HMCLNL
#endif
#if !defined(CERNLIB_DOUBLE)
       REAL GRAD(*),FVAL,XVAL(*),FUTIL
       REAL HMCLNL
#endif
       EXTERNAL HMCLNL
 
       IF(IFLAG.EQ.1)THEN
          RETURN
       ENDIF
 
       FVAL=-HMCLNL(IDD,IDM,IDW,NMCSRC,XVAL)
 
C termination code here.
       IF(IFLAG.EQ.3)THEN
          WRITE(6,*)'HFCNMIN: FCN termination for MINUIT'
          WRITE(6,*)'HFCNMIN: END VALUES OF PARAMETERS'
          WRITE(6,*)'HFCNMIN: ',(XVAL(JSRC),JSRC=1,NMCSRC)
       ENDIF
 
       RETURN
       END
