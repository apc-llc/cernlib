/*
 * $Id: trace.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: trace.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * @(#)trace.h  1.2 5/5/91 CERN CN-SW/DC Frederic Hemmer
 */
 
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
/* trace.h      tracing routines header                                 */
 
#ifndef _TRACE_H_INCLUDED_
#define _TRACE_H_INCLUDED_
 
extern void     print_trace();
extern void     init_trace();
extern void     end_trace();
 
#define TRACE           print_trace
#define INIT_TRACE      init_trace
#define END_TRACE       end_trace
 
#endif /* _TRACE_H_INCLUDED_  */
