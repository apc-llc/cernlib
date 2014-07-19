/*
 * $Id: patchlevel.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: patchlevel.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * @(#)patchlevel.h	1.2 9/3/91 CERN CN-SW/DC Frederic Hemmer
 */
 
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
/* patchlevel.h                 Patch level                             */
 
#ifndef _PATCHLEVEL_H_INCLUDED_
#define _PATCHLEVEL_H_INCLUDED_
 
/************************************************************************/
/* PATCHLEVEL           Description and changes                         */
/************************************************************************/
/* 1.0  Mar 20, 1991    First working version                           */
/* 1.1  Jul 23, 1991    First exportable version. Support for rfio,     */
/*                      msg, rtcopy, sysreq and library                 */
/* 1.2  Sep  3, 1991    Added tape support, Tape Management System      */
/*                      support. rtcopy -FF thru C IO. Provision for    */
/*                      truncated data records, data buffering.         */
/*                      rfio logs now short. Many many fixes            */
/************************************************************************/
 
 
#define PATCHLEVEL 1.2
 
#endif /* _PATCHLEVEL_H_INCLUDED_ */
