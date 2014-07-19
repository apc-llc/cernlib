*
* $Id: hcmcpm.inc,v 1.1.1.1 1996/01/16 17:07:50 mclareni Exp $
*
* $Log: hcmcpm.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCMCPM_INC
#define CERNLIB_HBOOK_HCMCPM_INC
*
*
* hcmcpm.inc
*
*CMZ :  4.20/13 19/10/93  15.38.13  by  Roger Barlow, Christine Beeston
*-- Author :    Christine Beeston   24/09/93
 
C some maximum values - max number of bins and dimensions
       INTEGER NSRCMX,NSRCMN,NOPTS
       PARAMETER(NSRCMX=20,NSRCMN=2,NOPTS=7)
 
C Numbers of monte carlo and data events, links to data, MC and weight
C histograms, Number of mc sources, options, total number of bins.
C normalisation constants for weight histograms
 
#if !defined(CERNLIB_DOUBLE)
#include "hbook/hcmcpm1.inc"
#endif
#if defined(CERNLIB_DOUBLE)
#include "hbook/hcmcpm2.inc"
#endif
       INTEGER NDATEV,NMCEV(NSRCMX),
     + NMCSRC,IOPT(NOPTS),NTOT
       COMMON/HINPUTS/NDATEV,NMCEV,NMCSRC,IOPT,NTOT,BJ
C Histogram IDs
       INTEGER IDD,IDM(NSRCMX),IDW(NSRCMX)
       COMMON/HMCIDS/IDD,IDM,IDW
 

#endif
