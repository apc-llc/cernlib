/*
 * $Id: sgtty.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: sgtty.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
#if defined(CERNLIB_VAXVMS)
/* sgtty.h structures for stty(3), gtty(3)    */
 
/*
 * Copyright (C) 1988, 1989 by Frederic Hemmer
 * All rights reserved
 */
 
#ifndef _SGTTY_
#define _SGTTY_
 
struct sgttyb {
 unsigned short  sg_pwidth; /* page width   */
 char sg_type;  /* type    */
 char sg_class;  /* class   */
 char sg_plength;  /* page length   */
 char sg_ispeed;  /* input speed    */
 char sg_ospeed;  /* output speed   */
 char sg_parity;  /* parity   */
 char sg_crcnt;  /* CR fill count  */
 char sg_lfcnt;  /* LF fill count  */
 unsigned long sg_flags;  /* terminal characteristics */
 unsigned long sg_xflags; /* terminal ext. characteristics*/
};
 
#include <ttdef.h>  /* Basic terminal characteristics defs */
#include <tt2def.h>  /* Extended terminal characterist. defs */
 
#endif _SGTTY_
#endif
