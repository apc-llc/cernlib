/*
 * $Id: matrixp.h,v 1.1.1.1 1996/03/01 11:39:36 mclareni Exp $
 *
 * $Log: matrixp.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:36  mclareni
 * Paw
 *
 */
/*
 * Copyright(c) 1992 Bell Communications Research, Inc. (Bellcore)
 *                        All rights reserved
 * Permission to use, copy, modify and distribute this material for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Bellcore not be used in advertising
 * or publicity pertaining to this material without the specific,
 * prior written permission of an authorized representative of
 * Bellcore.
 *
 * BELLCORE MAKES NO REPRESENTATIONS AND EXTENDS NO WARRANTIES, EX-
 * PRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR ANY PARTICULAR PURPOSE, AND THE WARRANTY AGAINST IN-
 * FRINGEMENT OF PATENTS OR OTHER INTELLECTUAL PROPERTY RIGHTS.  THE
 * SOFTWARE IS PROVIDED "AS IS", AND IN NO EVENT SHALL BELLCORE OR
 * ANY OF ITS AFFILIATES BE LIABLE FOR ANY DAMAGES, INCLUDING ANY
 * LOST PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES RELAT-
 * ING TO THE SOFTWARE.
 *
 * MatrixWidget Author: Andrew Wason, Bellcore, aw@bae.bellcore.com
 *
 * 7/20/1994:
 *      Changes are made by Q. Frank Xia, qx@math.columbia.edu,
 *      for cosmetic improvement.
 */

/*
 * MatrixP.h - Private definitions for Matrix widget
 */

#ifndef _Xbae_MatrixP_h
#define _Xbae_MatrixP_h

/*
 *  "@(#)MatrixP.h      3.12 11/25/92"
 */
#if XmVersion == 1001
#include <Xm/XmP.h>
#else
#include <Xm/ManagerP.h>
#endif
/* #include <Xbae/Matrix.h> */
#include "matrix.h"
#include <limits.h>                     /* ULONG_MAX */

/*
 * New types for the class methods
 */

#if defined (__cplusplus) || defined(c_plusplus)
#define CONST const
extern "C" {
#else
#define CONST
#endif

typedef void (*XbaeMatrixSetCellProc)(
#if NeedFunctionPrototypes
                                      XbaeMatrixWidget  /* w */,
                                      int               /* row */,
                                      int               /* column */,
                                      CONST String      /* value */
#endif
                                      );

typedef String (*XbaeMatrixGetCellProc)(
#if NeedFunctionPrototypes
                                        XbaeMatrixWidget/* w */,
                                        int             /* row */,
                                        int             /* column */
#endif
                                        );

typedef void (*XbaeMatrixEditCellProc)(
#if NeedFunctionPrototypes
                                       XbaeMatrixWidget /* w */,
                                       int              /* row */,
                                       int              /* column */
#endif
                                       );

typedef void (*XbaeMatrixSelectCellProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */,
                                         int                    /* row */,
                                         int                    /* column */
#endif
                                         );

typedef void (*XbaeMatrixSelectRowProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */,
                                         int                    /* row */
#endif
                                         );

typedef void (*XbaeMatrixSelectColumnProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */,
                                         int                    /* column */
#endif
                                         );

typedef void (*XbaeMatrixDeselectAllProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */
#endif
                                         );

typedef void (*XbaeMatrixDeselectCellProc)(
#if NeedFunctionPrototypes
                                           XbaeMatrixWidget     /* w */,
                                           int                  /* row */,
                                           int                  /* column */
#endif
                                         );

typedef void (*XbaeMatrixDeselectRowProc)(
#if NeedFunctionPrototypes
                                           XbaeMatrixWidget     /* w */,
                                           int                  /* row */
#endif
                                         );

typedef void (*XbaeMatrixDeselectColumnProc)(
#if NeedFunctionPrototypes
                                           XbaeMatrixWidget     /* w */,
                                           int                  /* column */
#endif
                                         );

typedef Boolean (*XbaeMatrixCommitEditProc)(
#if NeedFunctionPrototypes
                                            XbaeMatrixWidget    /* w */,
                                            Boolean             /* unmap */
#endif
                                            );

typedef void (*XbaeMatrixCancelEditProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */,
                                         Boolean                /* unmap */
#endif
                                         );

typedef void (*XbaeMatrixAddRowsProc)(
#if NeedFunctionPrototypes
                                      XbaeMatrixWidget  /* w */,
                                      int               /* position */,
                                      String *          /* rows */,
                                      String *          /* labels */,
                                      Pixel *           /* colors */,
                                      int               /* num_rows */
#endif
                                      );

typedef void (*XbaeMatrixDeleteRowsProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */,
                                         int                    /* position */,
                                         int                    /* num_rows */
#endif
                                         );

typedef void (*XbaeMatrixAddColumnsProc)(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget/* w */,
                                         int            /* position */,
                                         String *       /* columns */,
                                         String *       /* labels */,
                                         short *        /* widths */,
                                         int *          /* max_lengths */,
                                         unsigned char* /* alignments */,
                                         unsigned char* /* label_alignments */,
                                         Pixel *        /* colors */,
                                         int            /* num_columns */
#endif
                                         );

typedef void (*XbaeMatrixDeleteColumnsProc)(
#if NeedFunctionPrototypes
                                            XbaeMatrixWidget/* w */,
                                            int         /* position */,
                                            int         /* num_columns */
#endif
                                            );

typedef void (*XbaeMatrixSetRowColorsProc)(
#if NeedFunctionPrototypes
                                           XbaeMatrixWidget     /* w */,
                                           int                  /* position */,
                                           Pixel *              /* colors */,
                                           int                  /* num_colors */
#endif
                                           );

typedef void (*XbaeMatrixSetColumnColorsProc)(
#if NeedFunctionPrototypes
                                              XbaeMatrixWidget  /* w */,
                                              int               /* position */,
                                              Pixel *           /* colors */,
                                              int               /* num_colors */
#endif
                                              );

typedef void (*XbaeMatrixSetCellColorProc)(
#if NeedFunctionPrototypes
                                              XbaeMatrixWidget  /* w */,
                                              int               /* row */,
                                              int               /* column */,
                                              Pixel             /* color */
#endif
                                              );

#if defined (__cplusplus) || defined(c_plusplus)
}
#endif
#undef CONST

/*
 * Different than the traversal directions in Xm.h
 */
#define NOT_TRAVERSING  -1

/*
 * New fields for the Matrix widget class record
 */
typedef struct {
    XbaeMatrixSetCellProc               set_cell;
    XbaeMatrixGetCellProc               get_cell;
    XbaeMatrixEditCellProc              edit_cell;
    XbaeMatrixSelectCellProc            select_cell;
    XbaeMatrixSelectRowProc             select_row;
    XbaeMatrixSelectColumnProc          select_column;
    XbaeMatrixDeselectAllProc           deselect_all;
    XbaeMatrixDeselectCellProc          deselect_cell;
    XbaeMatrixDeselectRowProc           deselect_row;
    XbaeMatrixDeselectColumnProc        deselect_column;
    XbaeMatrixCommitEditProc            commit_edit;
    XbaeMatrixCancelEditProc            cancel_edit;
    XbaeMatrixAddRowsProc               add_rows;
    XbaeMatrixDeleteRowsProc            delete_rows;
    XbaeMatrixAddColumnsProc            add_columns;
    XbaeMatrixDeleteColumnsProc         delete_columns;
    XbaeMatrixSetRowColorsProc          set_row_colors;
    XbaeMatrixSetColumnColorsProc       set_column_colors;
    XbaeMatrixSetCellColorProc          set_cell_color;
    XtPointer                           extension;
} XbaeMatrixClassPart;

/*
 * Full class record declaration
 */
typedef struct _XbaeMatrixClassRec {
    CoreClassPart               core_class;
    CompositeClassPart          composite_class;
    ConstraintClassPart         constraint_class;
    XmManagerClassPart          manager_class;
    XbaeMatrixClassPart         matrix_class;
} XbaeMatrixClassRec;

extern XbaeMatrixClassRec xbaeMatrixClassRec;


/*
 * Inheritance constants for set/get/edit methods
 */
#define XbaeInheritGetCell ((XbaeMatrixGetCellProc) _XtInherit)
#define XbaeInheritSetCell ((XbaeMatrixSetCellProc) _XtInherit)
#define XbaeInheritEditCell ((XbaeMatrixEditCellProc) _XtInherit)
#define XbaeInheritSelectCell ((XbaeMatrixSelectCellProc) _XtInherit)
#define XbaeInheritSelectRow ((XbaeMatrixSelectRowProc) _XtInherit)
#define XbaeInheritSelectColumn ((XbaeMatrixSelectColumnProc) _XtInherit)
#define XbaeInheritDeselectAll ((XbaeMatrixDeselectAllProc) _XtInherit)
#define XbaeInheritDeselectCell ((XbaeMatrixDeselectCellProc) _XtInherit)
#define XbaeInheritDeselectRow ((XbaeMatrixDeselectRowProc) _XtInherit)
#define XbaeInheritDeselectColumn ((XbaeMatrixDeselectColumnProc) _XtInherit)
#define XbaeInheritCommitEdit ((XbaeMatrixCommitEditProc) _XtInherit)
#define XbaeInheritCancelEdit ((XbaeMatrixCancelEditProc) _XtInherit)
#define XbaeInheritAddRows ((XbaeMatrixAddRowsProc) _XtInherit)
#define XbaeInheritDeleteRows ((XbaeMatrixDeleteRowsProc) _XtInherit)
#define XbaeInheritAddColumns ((XbaeMatrixAddColumnsProc) _XtInherit)
#define XbaeInheritDeleteColumns ((XbaeMatrixDeleteColumnsProc)_XtInherit)
#define XbaeInheritSetRowColors ((XbaeMatrixSetRowColorsProc)_XtInherit)
#define XbaeInheritSetColumnColors ((XbaeMatrixSetColumnColorsProc)_XtInherit)
#define XbaeInheritSetCellColor ((XbaeMatrixSetCellColorProc)_XtInherit)


/*
 * New data structures for the ScrollMgr code
 */
typedef struct _SmScrollNode {
    int x;
    int y;
    struct _SmScrollNode *next;
    struct _SmScrollNode *prev;
} SmScrollNodeRec, *SmScrollNode;

typedef struct _SmScrollMgr {
    int offset_x;
    int offset_y;
    int scroll_count;
    SmScrollNode scroll_queue;
    Boolean scrolling;
} SmScrollMgrRec, *SmScrollMgr;


/*
 * Data structure for column labels
 */
typedef struct _ColumnLabelLines {
    int lines;
    int *lengths;
} ColumnLabelLinesRec, *ColumnLabelLines;


/*
 * New fields for the Matrix widget record
 */

#define XbaeMatrixDefaultBackground     ULONG_MAX

typedef struct {
    /* resources */
    int         rows, columns;      /* number of cells per row/column */
    Dimension   fixed_rows,         /* number of fixed rows OR columns */
                fixed_columns;
    Pixel       **cell_background;  /* 2D array of Pixels */
    Dimension   visible_columns;    /* number of columns to make visible */
    Dimension   visible_rows;       /* number of rows to make visible */
    short       *column_widths;     /* width of each column in chars */
    int         *column_max_lengths;/* max length of each column in chars */
    short       row_label_width;    /* max width of row labels in chars */

    String      **cells;            /* 2D array of strings */
    String      *row_labels;        /* array of labels next to each row */
    String      *column_labels;     /* array of labels above each column */
    unsigned char
                *column_alignments; /* alignment of each column */
    unsigned char
                *column_label_alignments; /* alignment of each column label */
    unsigned char
                row_label_alignment;/* alignment of row labels */

    Pixel       even_row_background;/* even row background color */
    Pixel       odd_row_background; /* odd row background color */

    XmFontList  font_list;          /* fontList of widget and textField */

    Dimension   space;              /* spacing for scrollbars */
    Boolean     bold_labels;        /* draw bold row/column labels */

    Boolean     **selected_cells;   /* 2D array indicating selected cells */
    Pixel       selected_foreground;/* foreground for selected cells */
    Pixel       selected_background;/* background for selected cells */

    Dimension   cell_margin_width,  /* margin resources for textField */
                cell_margin_height;
    Dimension   cell_shadow_thickness; /* shadow thickness for each cell */
    Dimension   cell_highlight_thickness;/* highlight thickness of textField */
    unsigned char  grid_type;       /* shadowed in/shadowed out/plan */
    Pixel       grid_line_color;    /* color of grid, for XmGrid_LINE */
    XtTranslations
                text_translations;  /* translations for textField widget */

    XtCallbackList
                modify_verify_callback; /* verify change to textField */
    XtCallbackList
                enter_cell_callback;/* called when a cell is entered */
    XtCallbackList
                leave_cell_callback;/* called when a cell is left */
    XtCallbackList
                traverse_cell_callback; /* determines the next cell to
                                           traverse to */
    XtCallbackList
                select_cell_callback;/* called when cells are selected */

    int         top_row;            /* vertical origin (in row space) */


    /* private state */
    int         horiz_origin;       /* horizontal origin (in pixel space) */

    int         current_row,        /* row/column we are editing */
                current_column;

    int         text_baseline;      /* baseline of text in each cell */

    GC          draw_gc;            /* GC for drawing cells/labels */
    GC          draw_clip_gc;       /* GC for clipped cells/labels */
    GC          cell_top_shadow_clip_gc; /* GC for clipped top shadow */
    GC          cell_bottom_shadow_clip_gc; /* GC for clipped bottom shadow */
    GC          grid_line_gc;

    XFontStruct *font;              /* fontStruct from fontList */

    int         *column_positions;  /* pixel position of each column */

    unsigned int cell_total_width;  /* width of cell area in pixels */

    int         cell_visible_height; /* height of visible cell area in pixels
                                        (not truncated to nearest row) */

    Dimension   desired_width,      /* width/height widget wants to be */
                desired_height;

    SmScrollMgr matrix_scroll_mgr,  /* ScrollMgrs for Matrix and Clip */
                clip_scroll_mgr;

    int         traversing;         /* set to direction we are traversing,
                                     * or NOT_TRAVERSING */

    ColumnLabelLines
                column_label_lines; /* structs to handle multi line column
                                     * labels */
    int         column_label_maxlines;
                                    /* max # lines in column labels */

} XbaeMatrixPart;

/*
 * Full instance record declaration
 */
typedef struct _XbaeMatrixRec {
    CorePart            core;
    CompositePart       composite;
    ConstraintPart      constraint;
    XmManagerPart       manager;
    XbaeMatrixPart      matrix;
} XbaeMatrixRec;

#endif /* _Xbae_MatrixP_h */
