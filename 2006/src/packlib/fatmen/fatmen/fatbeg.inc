*
* $Id: fatbeg.inc,v 1.1.1.1 1996/03/07 15:17:59 mclareni Exp $
*
* $Log: fatbeg.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:59  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATBEG_INC
#define CERNLIB_FATMEN_FATBEG_INC
*
*
* fatbeg.inc
*
#if defined(CERNLIB_IBMMVS)
         MACRO
&NAME    BEG   &ID,&MODE=REUS,&WKSIZE=0,&SP=0,&BASE=12,&RMODE=24
         GBLC  &RMD
&RMD     SETC  '&RMODE'
&NAME    CSECT
         AIF   ('&RMODE' EQ '24').NOT311
&NAME    RMODE ANY
&NAME    AMODE ANY
.NOT311  ENTRY &NAME
         USING *,&BASE
         ANF   &ID,MODE=&MODE,WKSIZE=&WKSIZE,SP=&SP,BASE=&BASE
         MEND
#endif

#endif
