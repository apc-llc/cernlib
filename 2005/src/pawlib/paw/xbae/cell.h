/*
 * $Id: cell.h,v 1.1.1.1 1996/03/01 11:39:35 mclareni Exp $
 *
 * $Log: cell.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:35  mclareni
 * Paw
 *
 */
/*
 * Copyright(C) Q. Frank Xia (qx@math.columbia.edu), 1994.
 *
 *                       All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Q. Frank Xia not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * This software is provided as-is and without any warranty of any kind.
 */

/*
 * Cell.h - Public definitions for Cell widget
 *
 * 7-20-1994:
 *      This file is created.
 */

#ifndef _XQ_CELL_H_
#define _XQ_CELL_H_

#include <Xm/Xm.h>
#include <Xm/TextF.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XqCellClassRec  *XqCellWidgetClass;
typedef struct _XqCellRec       *XqCellWidget;

externalref WidgetClass         xqCellWidgetClass;

#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XQ_CELL_H_ */
