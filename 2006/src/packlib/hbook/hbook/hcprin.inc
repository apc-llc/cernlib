*
* $Id: hcprin.inc,v 1.1.1.1 1996/01/16 17:07:51 mclareni Exp $
*
* $Log: hcprin.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:51  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCPRIN_INC
#define CERNLIB_HBOOK_HCPRIN_INC
*
*
* hcprin.inc
*
*CMZ :  4.22/04 02/06/94  14.29.18  by  Rene Brun
*-- Author :
      INTEGER       IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,
     +       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,
     +       KSQUEZ,NCOLMA,NCOLPA,NLINPA,       ICBLAC,ICSTAR,ICFUNC,
     +       IDG(42),MAXBIT(30),IDENT(9)
      REAL BIGP
*
      COMMON/HCPRIN/IFW   ,NW    ,NB    ,IH    ,NHT   ,ICN   ,IPONCE,
     +       NH    ,MSTEP ,NOENT ,NOLD  ,IDOLAR,IBLANC,KBINSZ,INO   ,
     +       KSQUEZ,NCOLMA,NCOLPA,NLINPA,BIGP  ,ICBLAC,ICSTAR,ICFUNC,
     +       IDG   ,MAXBIT,IDENT
*

#endif
