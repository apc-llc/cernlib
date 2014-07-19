*
* $Id: jbyt.inc,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: jbyt.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_JBYT_INC
#define CERNLIB_HBOOK_JBYT_INC
*
*
* jbyt.inc
*
*CMZ :  4.21/08 14/02/94  10.53.41  by  Rene Brun
*-- Author :
#if defined(CERNLIB_VAX)||defined(CERNLIB_ALLIANT)
#include "hbook/jbyt1.inc"
#endif
#if (defined(CERNLIB_IBM)||defined(CERNLIB_NORD)||defined(CERNLIB_CRAY)||defined(CERNLIB_CDC)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALLIANT))&&(!defined(CERNLIB_MSDOS))&&(!defined(CERNLIB_WINNT))
#include "hbook/jbyt2.inc"
#endif
#if (defined(CERNLIB_MSDOS))&&(defined(CERNLIB_NDP))
#include "hbook/jbyt3.inc"
#endif
#if (defined(CERNLIB_MSDOS)||defined(CERNLIB_WINNT))&&(defined(CERNLIB_QF2C))
#include "hbook/jbyt4.inc"
#endif
 

#endif
