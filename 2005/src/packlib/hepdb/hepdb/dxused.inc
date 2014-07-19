*
* $Id: dxused.inc,v 1.1.1.1 1996/02/28 16:24:01 mclareni Exp $
*
* $Log: dxused.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:01  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_DXUSED_INC
#define CERNLIB_HEPDB_DXUSED_INC
*
*
* dxused.inc
*
      PARAMETER       (NSYSDX=7, MXKYDX=100)
      COMMON /DXUSED/ LDATDX, LKEYDX, KEYSDX(MXKYDX), MASKDX(MXKYDX)
     +              , IOPADX, IOPBDX, IOPCDX, IOPDDX, IOPEDX, IOPFDX
     +              , IOPGDX, IOPHDX, IOPIDX, IOPJDX, IOPKDX, IOPLDX
     +              , IOPMDX, IOPNDX, IOPODX, IOPPDX, IOPQDX, IOPRDX
     +              , IOPSDX, IOPTDX, IOPUDX, IOPVDX, IOPWDX, IOPXDX
     +              , IOPYDX, IOPZDX, IOKYDX(MXKYDX)

#endif
