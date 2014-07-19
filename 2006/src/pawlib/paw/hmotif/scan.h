/*
 * $Id: scan.h,v 1.2 1996/09/09 15:21:28 couet Exp $
 *
 * $Log: scan.h,v $
 * Revision 1.2  1996/09/09 15:21:28  couet
 * - OLD command scan removed
 *
 * Revision 1.1.1.1  1996/03/01 11:39:14  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 14/03/94  01.42.04  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   12/10/92*/
/***********************************************************************
 *                                                                     *
 * scan.h: declarations and definitions for the ntuple scanner.        *
 *                                                                     *
 ***********************************************************************/
#ifndef _scan_h
#define _scan_h

#include "ntuplescan.h"
#ifdef CERNLIB_VAXVMS
#include "xbaevms/matrix.h"
#else
#include "xbae/matrix.h"
#endif


#define MAX_SCANNERS         5
#define MAX_ROWS             100
#define MAX_COLS             512
#define MAXLAB               32       /* max length of labels */


typedef struct {
   Widget  widget;                    /* scanner widget itself */
   Widget  matrix;                    /* matrix widget */
   int     nrows;                     /* number of rows in matrix */
   int     ncols;                     /* number of columns in matrix */
   int     last_event;                /* last event in matrix */
   int     ntuple_id;                 /* id of Ntuple being scanned */
   char    ntuple_dir[MAX_STRING];    /* directory of scanned Ntuple */
   int     ntuple_nrows;              /* number of rows in scanned Ntuple */
   int     no_vars;                   /* number of variables to scan */
   char    var_list[MAX_CMD_LENGTH];  /* list of variables to scan */
} NtupleScanStruct;


#endif  /* _scan_h */
