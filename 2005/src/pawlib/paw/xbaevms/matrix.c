/*
 * $Id: matrix.c,v 1.2 1996/10/18 13:59:36 couet Exp $
 *
 * $Log: matrix.c,v $
 * Revision 1.2  1996/10/18 13:59:36  couet
 * - include X11/IntrinsicP.h
 *
 * Revision 1.1.1.1  1996/03/01 11:39:37  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 14/03/94  01.37.20  by  Fons Rademakers*/
/*-- Author :*/
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
 */

/*
 * Matrix.c - row/column editable matrix widget
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <X11/Xos.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <Xm/XmP.h>
#if XmREVISION == 2
#include <Xm/ManagerP.h>
#endif
#include <Xm/AtomMgr.h>
#include <Xm/ScrollBar.h>
#include <Xm/TextF.h>
/* #include <Xbae/Clip.h> */
#include "clip.h"
/* #include <Xbae/MatrixP.h> */
#include "matrixp.h"

#ifndef SABER
static char sccsid[] = "@(#)Matrix.c    3.20 7/8/92";
#endif

#ifndef XlibSpecificationRelease
#define XrmPermStringToQuark XrmStringToQuark
#endif

#ifndef Max
#define Max(x, y)       (((x) > (y)) ? (x) : (y))
#endif
#ifndef Min
#define Min(x, y)       (((x) < (y)) ? (x) : (y))
#endif

/* this BSDism is used to often to replace it in the code */
#define bcopy(src,dst,n) memmove(dst,src,n)

/*
 * Translations for Matrix (these will also be used by the Clip child).
 */
static char defaultTranslations[] =
    ":<Btn1Down>:               EditCell(Pointer)";

/*
 * Default translations for XmNtextTranslations resource
 */
#define TEXT_TRANSLATIONS \
    "#override\n\
     Shift ~Ctrl ~Meta ~Alt <Key>Tab:   EditCell(Left)\n\
     ~Ctrl ~Meta ~Alt <Key>Tab:         EditCell(Right)\n\
     <Key>osfUp:                        EditCell(Up)\n\
     <Key>osfDown:                      EditCell(Down)\n\
     <Key>osfActivate:                  CommitEdit(False)\n\
     ~Shift ~Meta ~Alt <Key>Return:     CommitEdit(False)\n\
     <Key>osfCancel:                    CancelEdit(False)\n\
     Shift Ctrl ~Meta ~Alt <Key>Tab:    TraversePrev()\n\
     Ctrl ~Meta ~Alt <Key>Tab:          TraverseNext()"

#define offset(field)   XtOffsetOf(XbaeMatrixRec, field)

static XtResource resources[] = {
    { XmNboldLabels, XmCBoldLabels, XmRBoolean, sizeof(Boolean),
          offset(matrix.bold_labels), XmRImmediate, (XtPointer) False },
    { XmNcellHighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
          sizeof(Dimension), offset(matrix.cell_highlight_thickness),
          XmRImmediate, (XtPointer) 2 },
    { XmNcellMarginHeight, XmCMarginHeight, XmRVerticalDimension,
          sizeof(Dimension), offset(matrix.cell_margin_height),
          XmRImmediate, (XtPointer) 5 },
    { XmNcellMarginWidth, XmCMarginWidth, XmRHorizontalDimension,
          sizeof(Dimension), offset(matrix.cell_margin_width),
          XmRImmediate, (XtPointer) 5 },
    { XmNcells, XmCCells, XmRStringTable, sizeof(String **),
          offset(matrix.cells), XmRImmediate, NULL },
    { XmNcellShadowThickness, XmCShadowThickness, XmRHorizontalDimension,
          sizeof(Dimension), offset(matrix.cell_shadow_thickness),
          XmRImmediate, (XtPointer) 2 },
    { XmNcolors, XmCColors, XmRPixelTable, sizeof(Pixel **),
          offset(matrix.colors), XmRImmediate, (XtPointer) NULL },
    { XmNcolumnAlignments, XmCAlignments, XmRAlignmentArray,
          sizeof(unsigned char *), offset(matrix.column_alignments),
          XmRImmediate, (XtPointer) NULL },
    { XmNcolumnLabelAlignments, XmCAlignments, XmRAlignmentArray,
          sizeof(unsigned char *), offset(matrix.column_label_alignments),
          XmRImmediate, (XtPointer) NULL },
    { XmNcolumnLabels, XmCLabels, XmRStringArray, sizeof(String *),
          offset(matrix.column_labels), XmRImmediate, NULL },
    { XmNcolumnMaxLengths, XmCColumnMaxLengths, XmRMaxLengthArray,
          sizeof(int *), offset(matrix.column_max_lengths),
          XmRImmediate, NULL },
    { XmNcolumnWidths, XmCColumnWidths, XmRWidthArray, sizeof(short *),
          offset(matrix.column_widths), XmRImmediate, NULL },
    { XmNcolumns, XmCColumns, XmRInt, sizeof(int),
          offset(matrix.columns), XmRImmediate, (XtPointer) 0 },
    { XmNenterCellCallback, XmCCallback, XmRCallback,sizeof(XtCallbackList),
          offset(matrix.enter_cell_callback), XmRCallback, NULL },
    { XmNfixedColumns, XmCFixedColumns, XmRDimension, sizeof(Dimension),
          offset(matrix.fixed_columns), XmRImmediate, (XtPointer) 0 },
    { XmNfixedRows, XmCFixedRows, XmRDimension, sizeof(Dimension),
          offset(matrix.fixed_rows), XmRImmediate, (XtPointer) 0 },
    { XmNfontList, XmCFontList, XmRFontList, sizeof(XmFontList),
          offset(matrix.font_list), XmRString, (XtPointer) "fixed" },
    { XmNleaveCellCallback, XmCCallback, XmRCallback,sizeof(XtCallbackList),
          offset(matrix.leave_cell_callback), XmRCallback, NULL },
    { XmNmodifyVerifyCallback, XmCCallback, XmRCallback,sizeof(XtCallbackList),
          offset(matrix.modify_verify_callback), XmRCallback, NULL },
    { XmNrowLabelAlignment, XmCAlignment, XmRAlignment, sizeof(unsigned char),
          offset(matrix.row_label_alignment),
          XmRImmediate, (XtPointer) XmALIGNMENT_END },
    { XmNrowLabelWidth, XmCRowLabelWidth, XmRShort, sizeof(short),
          offset(matrix.row_label_width), XmRImmediate, (XtPointer) 0 },
    { XmNrowLabels, XmCLabels, XmRStringArray, sizeof(String *),
          offset(matrix.row_labels), XmRImmediate, NULL },
    { XmNrows, XmCRows, XmRInt, sizeof(int),
          offset(matrix.rows), XmRImmediate, (XtPointer) 0 },
    { XmNselectCellCallback, XmCCallback, XmRCallback,
          sizeof(XtCallbackList), offset(matrix.select_cell_callback),
          XmRCallback, NULL },
    { XmNselectedCells, XmCSelectedCells, XmRBooleanTable, sizeof(Boolean **),
          offset(matrix.selected_cells), XmRImmediate, (XtPointer) NULL },
    { XmNspace, XmCSpace, XmRHorizontalDimension, sizeof(Dimension),
          offset(matrix.space), XmRImmediate, (XtPointer) 6 },
    { XmNtextTranslations, XmCTranslations, XmRTranslationTable,
          sizeof(XtTranslations), offset(matrix.text_translations),
          XmRString, (XtPointer) TEXT_TRANSLATIONS },
    { XmNtopRow, XmCTopRow, XmRInt, sizeof(int),
          offset(matrix.top_row), XmRImmediate, (XtPointer) 0 },
    { XmNtraverseCellCallback, XmCCallback, XmRCallback,sizeof(XtCallbackList),
          offset(matrix.traverse_cell_callback), XmRCallback, NULL },
    { XmNvisibleColumns, XmCVisibleColumns, XmRDimension, sizeof(Dimension),
          offset(matrix.visible_columns), XmRImmediate, (XtPointer) 0 },
    { XmNvisibleRows, XmCVisibleRows, XmRDimension, sizeof(Dimension),
          offset(matrix.visible_rows), XmRImmediate, (XtPointer) 0 },

    /* Override Manager default */
    { XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
          sizeof(Dimension),
          XtOffsetOf(XmManagerRec, manager.shadow_thickness),
          XmRImmediate, (XtPointer) 2 },
};

static XmSyntheticResource syn_resources[] = {
    { XmNcellHighlightThickness, sizeof(Dimension),
          offset(matrix.cell_highlight_thickness),
          _XmFromHorizontalPixels, _XmToHorizontalPixels },
    { XmNcellMarginHeight, sizeof(Dimension),
          offset(matrix.cell_margin_height),
          _XmFromVerticalPixels, _XmToVerticalPixels },
    { XmNcellMarginWidth, sizeof(Dimension),
          offset(matrix.cell_margin_width),
          _XmFromHorizontalPixels, _XmToHorizontalPixels },
    { XmNcellShadowThickness, sizeof(Dimension),
          offset(matrix.cell_shadow_thickness),
          _XmFromHorizontalPixels, _XmToHorizontalPixels },
    { XmNspace, sizeof(Dimension),
          offset(matrix.space),
          _XmFromHorizontalPixels, _XmToHorizontalPixels },
};

/*
 * End of array flags for the array type converters
 */
#define BAD_WIDTH       0
#define BAD_MAXLENGTH   0
#define BAD_ALIGNMENT   3       /* see Xm.h */

/*
 * Macros to retrieve our children.  Children must be created in this order.
 */
#define HorizScrollChild(mw)    (mw->composite.children[0])
#define VertScrollChild(mw)     (mw->composite.children[1])
#define ClipChild(mw)           (mw->composite.children[2])
#define TextChild(mw)           (mw->composite.children[3])
/*
 * Macros
 */
#define FONT_WIDTH(mw)          (mw->matrix.font->max_bounds.width)
#define TEXT_WIDTH_OFFSET(mw)   (mw->matrix.cell_margin_width +\
                                 mw->matrix.cell_shadow_thickness +\
                                 mw->matrix.cell_highlight_thickness)
#define TEXT_HEIGHT_OFFSET(mw)  (mw->matrix.cell_margin_height +\
                                 mw->matrix.cell_shadow_thickness +\
                                 mw->matrix.cell_highlight_thickness)
#define COLUMN_WIDTH(mw, col)   ((mw->matrix.column_widths[col] * \
                                  FONT_WIDTH(mw)) + \
                                 (TEXT_WIDTH_OFFSET(mw) * 2))
#define TEXT_HEIGHT(mw)         (mw->matrix.font->descent + \
                                 mw->matrix.font->ascent)
#define ROW_HEIGHT(mw)          (int)((TEXT_HEIGHT_OFFSET(mw) * 2) + \
                                        TEXT_HEIGHT(mw))

#define TEXT_X_OFFSET(mw)       (int)(TEXT_WIDTH_OFFSET(mw))
#define TEXT_Y_OFFSET(mw)       (int)(mw->matrix.text_baseline)

#define ROW_LABEL_WIDTH(mw)     (mw->matrix.row_labels \
                                 ? (mw->matrix.row_label_width * \
                                    FONT_WIDTH(mw)) + \
                                        TEXT_WIDTH_OFFSET(mw) * 2 \
                                 : 0)
#define COLUMN_LABEL_HEIGHT(mw) (mw->matrix.column_labels \
                                 ? TEXT_HEIGHT_OFFSET(mw) * 2 + \
                                   mw->matrix.column_label_maxlines * \
                                   TEXT_HEIGHT(mw) \
                                 : 0)

#define FIXED_COLUMN_WIDTH(mw)  mw->matrix.column_positions \
                                        [mw->matrix.fixed_columns]
#define COLUMN_LABEL_OFFSET(mw) (ROW_LABEL_WIDTH(mw) + \
                                 mw->manager.shadow_thickness)
#define FIXED_COLUMN_LABEL_OFFSET(mw) (COLUMN_LABEL_OFFSET(mw) + \
                                       FIXED_COLUMN_WIDTH(mw))

#define FIXED_ROW_HEIGHT(mw)    (mw->matrix.fixed_rows * ROW_HEIGHT(mw))
#define ROW_LABEL_OFFSET(mw)    (COLUMN_LABEL_HEIGHT(mw) + \
                                 mw->manager.shadow_thickness)
#define FIXED_ROW_LABEL_OFFSET(mw) (ROW_LABEL_OFFSET(mw) + \
                                    FIXED_ROW_HEIGHT(mw))

#define CELL_TOTAL_WIDTH(mw)    mw->matrix.cell_total_width
#define CELL_TOTAL_HEIGHT(mw)   ((mw->matrix.rows - \
                                  (int) mw->matrix.fixed_rows) \
                                 * ROW_HEIGHT(mw))

#define VISIBLE_WIDTH(mw)       (ClipChild(mw)->core.width)
#define VISIBLE_HEIGHT(mw)      (ClipChild(mw)->core.height)

#define VERT_ORIGIN(mw)         (mw->matrix.top_row)
#define HORIZ_ORIGIN(mw)        (mw->matrix.horiz_origin)

#define VSCROLL_WIDTH(mw)       (VertScrollChild(mw)->core.width + \
                                 2 * VertScrollChild(mw)->core.border_width +\
                                 mw->matrix.space)
#define VSCROLL_HEIGHT(mw)      (VertScrollChild(mw)->core.height + \
                                 2 * VertScrollChild(mw)->core.border_width)
#define HSCROLL_WIDTH(mw)       (HorizScrollChild(mw)->core.width + \
                                 2 * HorizScrollChild(mw)->core.border_width)
#define HSCROLL_HEIGHT(mw)      (HorizScrollChild(mw)->core.height + \
                                 2 * HorizScrollChild(mw)->core.border_width+\
                                 mw->matrix.space)

#define IS_FIXED(mw, row, column) (row < mw->matrix.fixed_rows || \
                                   column < mw->matrix.fixed_columns)
#define CELL_WINDOW(mw, row, column) \
    (IS_FIXED(mw, row, column) \
     ? XtWindow(mw) \
     : XtWindow(ClipChild(mw)))

/* Inline functions */
#define FreeColumnWidths(mw)            XtFree((XtPointer) \
                                               mw->matrix.column_widths)
#define FreeColumnMaxLengths(mw)        XtFree((XtPointer) \
                                               mw->matrix.column_max_lengths)
#define FreeColumnPositions(mw)         XtFree((XtPointer) \
                                               mw->matrix.column_positions)
#define FreeColumnAlignments(mw)        XtFree((XtPointer) \
                                               mw->matrix.column_alignments)
#define FreeColumnLabelAlignments(mw)   XtFree((XtPointer) \
                                               mw->matrix.\
                                               column_label_alignments)
#define CreateColumnPositions(mw)       (int *) XtMalloc(mw->matrix.columns * \
                                                         sizeof(int))
#define YtoRow(mw, y)                   ((y) / (ROW_HEIGHT(mw)))


/*
 * Macros used for Rectangle calculations.  A Rectangle is defined by it's
 * upper left and lower right corners.
 */

/*
 * Set a Rectangle. (x1,y1) is upper left corner, (x2,y2) is lower right corner
 */
#define SETRECT(r, X1, Y1, X2, Y2) { (r).x1 = X1; (r).y1 = Y1; \
                                         (r).x2 = X2; (r).y2 = Y2; }

/*
 * Evaluates to 1 if two Rectangles overlap, 0 if no overlap
 */
#define OVERLAP(r1, r2) \
    ((r1).x2 > (r2).x1 && \
     (r1).x1 < (r2).x2 && \
     (r1).y2 > (r2).y1 && \
     (r1).y1 < (r2).y2)

/*
 * Intersect rectangles r1 and r2, place the result in res.
 * Result will be in r1's coord system.
 * Max and Min are defined in Xm/XmP.h.
 */
#define X_INTERSECT(r1, r2, res) { (res).x1 = Max((r1).x1, (r2).x1) - (r1).x1;\
                                   (res).x2 = Min((r1).x2, (r2).x2) - (r1).x1;}
#define Y_INTERSECT(r1, r2, res) { (res).y1 = Max((r1).y1, (r2).y1) - (r1).y1;\
                                   (res).y2 = Min((r1).y2, (r2).y2) - (r1).y1;}
#define INTERSECT(r1, r2, res)  { X_INTERSECT(r1, r2, res); \
                                      Y_INTERSECT(r1, r2, res); }

/*
 * Evaluates to 1 if the point is in the Rectangle, 0 if not
 */
#define INBOX(r, x, y) \
      ( ( ((r).x2 >= x)) && \
        ( ((r).x1 <= x)) && \
        ( ((r).y2 >= y)) && \
        ( ((r).y1 <= y)) )

/*
 * Rectangle struct used for internal calculations.  (x1,y1) are the upper
 * left corner, (x2,y2) are the lower right.
 */
typedef struct _Rectangle {
    int x1, y1;
    int x2, y2;
} Rectangle;

/*
 * Enumeration for type of a cell
 */
typedef enum {FixedCell, NonFixedCell} CellType;

/*
 * Declaration of methods
 */
static void ClassInitialize();
static void ClassPartInitialize();
static void Initialize();
static void Realize();
static void InsertChild();
static void Redisplay();
static Boolean SetValues();
static void SetValuesAlmost();
static void Destroy();
static void Resize();
static XtGeometryResult GeometryManager();
static XtGeometryResult QueryGeometry();

/*
 * Redraw function for clip widget
 */
static void ClipRedisplay();

/*
 * New Matrix methods
 */
static void SetCell();
static void EditCell();
static void SelectCell();
static void SelectRow();
static void SelectColumn();
static void DeselectAll();
static void DeselectCell();
static void DeselectRow();
static void DeselectColumn();
static String GetCell();
static Boolean CommitEdit(
#if NeedFunctionPrototypes
                                            XbaeMatrixWidget    /* w */,
                                            Boolean     /* unmap */
#endif
);
static void CancelEdit(
#if NeedFunctionPrototypes
                                         XbaeMatrixWidget       /* w */,
                                         Boolean/* unmap */
#endif
);
static void AddRows();
static void DeleteRows();
static void AddColumns();
static void DeleteColumns();
static void SetRowColors();
static void SetColumnColors();
static void SetCellColor();

/*
 * Private functions unique to Matrix
 */
static void CreateDrawGC(), GetInverseGC(),
    CreateDrawClipGC(), CreateInverseClipGC(),
    CreateTopShadowClipGC(), CreateBottomShadowClipGC(),
    SetClipMask(), NewFont(),
    GetCellTotalWidth(), GetColumnPositions(),
    DrawString(), DrawColumnLabel(), DrawRowLabel(), DrawCell(),
    RedrawCells(), RedrawLabelsAndFixed(), ComputeSize(),
    FreeCells(), FreeRowLabels(), FreeColumnLabels(), FreeColors(),
    FreeSelectedCells(), CreateColors(), CopySelectedCells(),
    ResizeCells(), ResizeSelectedCells(), ResizeColors(), RowColToXY(),
    AddRowsToTable(), DeleteRowsFromTable(), AddColumnsToTable(),
    DeleteColumnsFromTable(), ClearCell(), GetVisibleRows(),
    GetVisibleColumns(), GetVisibleCells(),
    MakeRowVisible(), MakeColumnVisible(),
    MakeCellVisible(), AdjustTopRow();
static Boolean IsRowVisible(), IsColumnVisible(), IsCellVisible(),
    XYToRowCol(), DoCommitEdit(), EventToXY();
static short MaxRowLabel();
static int XtoCol();
static void CopyRowLabels(), CopyColumnLabels(), CopyCells(),
    CopyColumnWidths(), CopyColumnMaxLengths(),
    CopyColumnAlignments(), CopyColumnLabelAlignments(), CopyColors();
static void ParseColumnLabel();

/*
 * Scrollbar callbacks
 */
static void ScrollVertCB(), ScrollHorizCB();

/*
 * TextField modifyVerifyCallback
 */
static void ModifyVerifyCB();

/*
 * Clip widget focusCallback
 */
static void TraverseInCB();

/*
 * ScrollMgr implementation
 */
static SmScrollMgr SmCreateScrollMgr();
static void SmDestroyScrollMgr(), SmAddScroll(),
    SmRemoveScroll(), SmScrollEvent();

/*
 * Public convenience functions
 */
void XbaeMatrixSetCell();
void XbaeMatrixEditCell();
void XbaeMatrixSelectCell();
void XbaeMatrixSelectRow();
void XbaeMatrixSelectColumn();
void XbaeMatrixDeselectAll();
void XbaeMatrixDeselectCell();
void XbaeMatrixDeselectRow();
void XbaeMatrixDeselectColumn();
String XbaeMatrixGetCell();
Boolean XbaeMatrixCommitEdit(
#if NeedFunctionPrototypes
                                    Widget      /* w */,
                                    Boolean     /* unmap */
#endif
);
void XbaeMatrixCancelEdit(
#if NeedFunctionPrototypes
                                 Widget         /* w */,
                                 Boolean        /* unmap */
#endif
);
void XbaeMatrixAddRows();
void XbaeMatrixDeleteRows();
void XbaeMatrixAddColumns();
void XbaeMatrixDeleteColumns();
void XbaeMatrixSetRowColors();
void XbaeMatrixSetColumnColors();
void XbaeMatrixSetCellColor();

/*
 * Type converters
 */
Boolean CvtStringToStringArray();
void StringArrayDestructor();
Boolean CvtStringToWidthArray();
void WidthArrayDestructor();
Boolean CvtStringToMaxLengthArray();
void MaxLengthArrayDestructor();
static Boolean StringsAreEqual();
Boolean CvtStringToAlignmentArray();
void AlignmentArrayDestructor();

/*
 * Actions
 */
static void EditCellACT(), CancelEditACT(), CommitEditACT(),
    SelectCellACT(), TraverseNextACT(), TraversePrevACT();

/*
 * Matrix actions
 */
static XtActionsRec actions[] = {
    {"EditCell",        EditCellACT},
    {"CancelEdit",      CancelEditACT},
    {"CommitEdit",      CommitEditACT},
    {"SelectCell",      SelectCellACT},
    {"TraverseNext",    TraverseNextACT},
    {"TraversePrev",    TraversePrevACT},
};


XbaeMatrixClassRec xbaeMatrixClassRec = {
    {
    /* core_class fields */
        /* superclass                   */ (WidgetClass) &xmManagerClassRec,
        /* class_name                   */ "XbaeMatrix",
        /* widget_size                  */ sizeof(XbaeMatrixRec),
        /* class_initialize             */ ClassInitialize,
        /* class_part_initialize        */ ClassPartInitialize,
        /* class_inited                 */ False,
        /* initialize                   */ Initialize,
        /* initialize_hook              */ NULL,
        /* realize                      */ Realize,
        /* actions                      */ actions,
        /* num_actions                  */ XtNumber(actions),
        /* resources                    */ resources,
        /* num_resources                */ XtNumber(resources),
        /* xrm_class                    */ NULLQUARK,
        /* compress_motion              */ True,
        /* compress_exposure            */ XtExposeCompressSeries |
                                                XtExposeGraphicsExpose |
                                                    XtExposeNoExpose,
        /* compress_enterleave          */ True,
        /* visible_interest             */ False,
        /* destroy                      */ Destroy,
        /* resize                       */ Resize,
        /* expose                       */ Redisplay,
        /* set_values                   */ SetValues,
        /* set_values_hook              */ NULL,
        /* set_values_almost            */ SetValuesAlmost,
        /* get_values_hook              */ NULL,
        /* accept_focus                 */ NULL,
        /* version                      */ XtVersion,
        /* callback_private             */ NULL,
        /* tm_table                     */ defaultTranslations,
        /* query_geometry               */ QueryGeometry,
        /* display_accelerator          */ NULL,
        /* extension                    */ NULL
    },

/*
 * XXX I should be able to use XtInheritGeometryManager &
 * XtInheritChangeManaged but Composite defines these as NULL.
 * (Xt seems to allow change_managed to be NULL)
 */
    {
    /* composite_class fields */
        /* geometry_manager             */ GeometryManager,
        /* change_managed               */ NULL,
        /* insert_child                 */ InsertChild,
        /* delete_child                 */ XtInheritDeleteChild,
        /* extension                    */ NULL,
    },
    {
    /* constraint_class fields */
        /* resources                    */ NULL,
        /* num_resources                */ 0,
        /* constraint_size              */ 0,
        /* initialize                   */ NULL,
        /* destroy                      */ NULL,
        /* set_values                   */ NULL,
        /* extension                    */ NULL
    },
    {
    /* manager_class fields */
        /* translations                 */  XtInheritTranslations,
        /* syn_resources                */  syn_resources,
        /* num_syn_resources            */  XtNumber(syn_resources),
        /* syn_constraint_resources     */  NULL,
        /* num_syn_constraint_resources */  0,
        /* parent_process               */  XmInheritParentProcess,
        /* extension                    */  NULL
    },
    {
    /* matrix_class fields */
        /* set_cell                     */ SetCell,
        /* get_cell                     */ GetCell,
        /* edit_cell                    */ EditCell,
        /* select_cell                  */ SelectCell,
        /* select_row                   */ SelectRow,
        /* select_column                */ SelectColumn,
        /* deselect_all                 */ DeselectAll,
        /* deselect_cell                */ DeselectCell,
        /* deselect_row                 */ DeselectRow,
        /* deselect_column              */ DeselectColumn,
        /* commit_edit                  */ CommitEdit,
        /* cancel_edit                  */ CancelEdit,
        /* add_rows                     */ AddRows,
        /* delete_rows                  */ DeleteRows,
        /* add_columns                  */ AddColumns,
        /* delete_columns               */ DeleteColumns,
        /* set_row_colors               */ SetRowColors,
        /* set_column_colors            */ SetColumnColors,
        /* set_cell_color               */ SetCellColor,
        /* extension                    */ NULL,
    }
};

WidgetClass xbaeMatrixWidgetClass = (WidgetClass)&xbaeMatrixClassRec;


static void
ClassInitialize()
{
    /*
     * String to StringArray is used for XmNrowLabels and XmNcolumnLabels
     * We make a private copy of this table, should we cache? XXX
     */
    XtSetTypeConverter(XmRString, XmRStringArray,
                       CvtStringToStringArray, NULL, 0,
                       XtCacheAll | XtCacheRefCount,
                       StringArrayDestructor);

    /*
     * String to ShortArray is used for XmNcolumnWidths resource.
     * We make a private copy of this table, should we cache? XXX
     */
    XtSetTypeConverter(XmRString, XmRWidthArray,
                       CvtStringToWidthArray, NULL, 0,
                       XtCacheAll | XtCacheRefCount,
                       WidthArrayDestructor);

    /*
     * String to IntArray is used for XmNcolumnMaxLengths resource.
     * We make a private copy of this table, should we cache? XXX
     */
    XtSetTypeConverter(XmRString, XmRMaxLengthArray,
                       CvtStringToMaxLengthArray, NULL, 0,
                       XtCacheAll | XtCacheRefCount,
                       MaxLengthArrayDestructor);

    /*
     * String to AlignmentArray is used for XmNcolumnAlignments
     * and XmNcolumnLabelAlignments resources.
     */
    XtSetTypeConverter(XmRString, XmRAlignmentArray,
                       CvtStringToAlignmentArray, NULL, 0,
                       XtCacheAll | XtCacheRefCount,
                       AlignmentArrayDestructor);
}

static void
ClassPartInitialize(mwc)
XbaeMatrixWidgetClass mwc;
{
    register XbaeMatrixWidgetClass super =
        (XbaeMatrixWidgetClass) mwc->core_class.superclass;

    /*
     * Allow subclasses to inherit new Matrix methods
     */
    if (mwc->matrix_class.set_cell == XbaeInheritSetCell)
        mwc->matrix_class.set_cell = super->matrix_class.set_cell;
    if (mwc->matrix_class.get_cell == XbaeInheritGetCell)
        mwc->matrix_class.get_cell = super->matrix_class.get_cell;
    if (mwc->matrix_class.edit_cell == XbaeInheritEditCell)
        mwc->matrix_class.edit_cell = super->matrix_class.edit_cell;
    if (mwc->matrix_class.select_cell == XbaeInheritSelectCell)
        mwc->matrix_class.select_cell = super->matrix_class.select_cell;
    if (mwc->matrix_class.select_row == XbaeInheritSelectRow)
        mwc->matrix_class.select_row = super->matrix_class.select_row;
    if (mwc->matrix_class.select_column == XbaeInheritSelectColumn)
        mwc->matrix_class.select_column = super->matrix_class.select_column;
    if (mwc->matrix_class.deselect_all == XbaeInheritDeselectAll)
        mwc->matrix_class.deselect_all = super->matrix_class.deselect_all;
    if (mwc->matrix_class.deselect_cell == XbaeInheritDeselectCell)
        mwc->matrix_class.deselect_cell = super->matrix_class.deselect_cell;
    if (mwc->matrix_class.deselect_row == XbaeInheritDeselectRow)
        mwc->matrix_class.deselect_row = super->matrix_class.deselect_row;
    if (mwc->matrix_class.deselect_column == XbaeInheritDeselectColumn)
        mwc->matrix_class.deselect_column =
            super->matrix_class.deselect_column;
    if (mwc->matrix_class.commit_edit == XbaeInheritCommitEdit)
        mwc->matrix_class.commit_edit = super->matrix_class.commit_edit;
    if (mwc->matrix_class.cancel_edit == XbaeInheritCancelEdit)
        mwc->matrix_class.cancel_edit = super->matrix_class.cancel_edit;
    if (mwc->matrix_class.add_rows == XbaeInheritAddRows)
        mwc->matrix_class.add_rows = super->matrix_class.add_rows;
    if (mwc->matrix_class.delete_rows == XbaeInheritDeleteRows)
        mwc->matrix_class.delete_rows = super->matrix_class.delete_rows;
    if (mwc->matrix_class.add_columns == XbaeInheritAddColumns)
        mwc->matrix_class.add_columns = super->matrix_class.add_columns;
    if (mwc->matrix_class.delete_columns == XbaeInheritDeleteColumns)
        mwc->matrix_class.delete_columns = super->matrix_class.delete_columns;
    if (mwc->matrix_class.set_row_colors == XbaeInheritSetRowColors)
        mwc->matrix_class.set_row_colors = super->matrix_class.set_row_colors;
    if (mwc->matrix_class.set_column_colors == XbaeInheritSetColumnColors)
        mwc->matrix_class.set_column_colors =
            super->matrix_class.set_column_colors;
    if (mwc->matrix_class.set_cell_color == XbaeInheritSetCellColor)
        mwc->matrix_class.set_cell_color = super->matrix_class.set_cell_color;
}

static void
CreateDrawGC(mw)
XbaeMatrixWidget mw;
{
    XGCValues values;
    unsigned long mask = GCForeground | GCFont;

    /*
     * GC for drawing cells/labels. We create it instead of using a cached one,
     * since the foreground may change frequently.
     */
    values.foreground = mw->manager.foreground;
    values.font = mw->matrix.font->fid;
    mw->matrix.draw_gc = XCreateGC(XtDisplay(mw),
                                   RootWindowOfScreen(XtScreen(mw)),
                                   mask, &values);
}

static void
GetInverseGC(mw)
XbaeMatrixWidget mw;
{
    XGCValues values;
    XtGCMask mask = GCForeground | GCFont;

    /*
     * GC for drawing selected cells.
     */
    values.foreground = mw->core.background_pixel;
    values.font = mw->matrix.font->fid;
    mw->matrix.inverse_gc = XtGetGC((Widget)mw, mask, &values);
}

static void
CreateDrawClipGC(mw)
XbaeMatrixWidget mw;
{
    XGCValues values;
    unsigned long mask = GCForeground | GCFont;

    /*
     * GC for drawing cells/labels with clipping.
     */
    values.foreground = mw->manager.foreground;
    values.font = mw->matrix.font->fid;
    mw->matrix.draw_clip_gc = XCreateGC(XtDisplay(mw),
                                        RootWindowOfScreen(XtScreen(mw)),
                                        mask, &values);
}

static void
CreateInverseClipGC(mw)
XbaeMatrixWidget mw;
{
    XGCValues values;
    XtGCMask mask = GCForeground | GCFont;

    /*
     * GC for drawing selected cells with clipping.
     */
    values.foreground = mw->core.background_pixel;
    values.font = mw->matrix.font->fid;

    mw->matrix.inverse_clip_gc = XCreateGC(XtDisplay(mw),
                                           RootWindowOfScreen(XtScreen(mw)),
                                           mask, &values);
}

static void
CreateTopShadowClipGC(mw)
XbaeMatrixWidget mw;
{
    XGCValues values;
    XtGCMask mask = GCForeground | GCBackground;

    /*
     * GC for drawing top shadow inside cells with clipping.
     */
    values.foreground = mw->manager.top_shadow_color;
    values.background = mw->manager.foreground;

    if (mw->manager.top_shadow_pixmap != XmUNSPECIFIED_PIXMAP) {
        mask |= GCFillStyle | GCTile;
        values.fill_style = FillTiled;
        values.tile = mw->manager.top_shadow_pixmap;
    }
    mw->matrix.cell_top_shadow_clip_gc =
        XCreateGC(XtDisplay(mw),
                  RootWindowOfScreen(XtScreen(mw)),
                  mask, &values);
}

static void
CreateBottomShadowClipGC(mw)
XbaeMatrixWidget mw;
{
    XGCValues values;
    XtGCMask mask = GCForeground | GCBackground;

    /*
     * GC for drawing bottom shadow inside cells with clipping.
     */
    values.foreground = mw->manager.bottom_shadow_color;
    values.background = mw->manager.foreground;

    if (mw->manager.bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP) {
        mask |= GCFillStyle | GCTile;
        values.fill_style = FillTiled;
        values.tile = mw->manager.bottom_shadow_pixmap;
    }
    mw->matrix.cell_bottom_shadow_clip_gc =
        XCreateGC(XtDisplay(mw),
                  RootWindowOfScreen(XtScreen(mw)),
                  mask, &values);
}

/*
 * Set the clip_mask in our draw and shadow GCs.  This is necessary for
 * drawing non-fixed column labels and fixed rows.
 */
static void
SetClipMask(mw)
XbaeMatrixWidget mw;
{
    XRectangle r;

    /*
     * XRectangle enclosing column labels and fixed rows
     */
    r.x = FIXED_COLUMN_LABEL_OFFSET(mw);
    r.y = 0;
    r.width = ClipChild(mw)->core.width;
    r.height = FIXED_ROW_LABEL_OFFSET(mw);

    /*
     * Reset the clip_mask in our clipping GCs
     */
    XSetClipRectangles(XtDisplay(mw), mw->matrix.draw_clip_gc,
                       0, 0, &r, 1, Unsorted);
    XSetClipRectangles(XtDisplay(mw), mw->matrix.inverse_clip_gc,
                       0, 0, &r, 1, Unsorted);
    XSetClipRectangles(XtDisplay(mw), mw->matrix.cell_top_shadow_clip_gc,
                       0, 0, &r, 1, Unsorted);
    XSetClipRectangles(XtDisplay(mw), mw->matrix.cell_bottom_shadow_clip_gc,
                       0, 0, &r, 1, Unsorted);
}

static void
NewFont(mw)
XbaeMatrixWidget mw;
{
    XmFontContext context;
    XmStringCharSet charset;
    XFontStruct *font;

    /*
     * Make a private copy of the FontList
     */
    mw->matrix.font_list = XmFontListCopy(mw->matrix.font_list);

    /*
     * Get XFontStruct from FontList
     */

    if (!XmFontListInitFontContext(&context, mw->matrix.font_list))
        XtAppErrorMsg(XtWidgetToApplicationContext((Widget)mw),
                      "newFont", "badFont", "XbaeMatrix",
                 "XbaeMatrix: XmFontListInitFontContext failed, bad fontList",
                      NULL, 0);

    if (!XmFontListGetNextFont(context, &charset, &font))
        XtAppErrorMsg(XtWidgetToApplicationContext((Widget)mw),
                      "newFont", "badFont", "XbaeMatrix",
     "XbaeMatrix: XmFontListGetNextFont failed, cannot get font from fontList",
                      NULL, 0);

    XtFree(charset);
    XmFontListFreeFontContext(context);

    mw->matrix.font = font;
}

/*
 * Return the length of the longest row label
 */
static short
MaxRowLabel(mw)
XbaeMatrixWidget mw;
{
    int i;
    short max = 0, len;

    /*
     * Determine the length of the longest row label
     */
    for (i = 0; i < mw->matrix.rows; i++) {
        len = strlen(mw->matrix.row_labels[i]);
        if (len > max)
            max = len;
    }
    return max;
}

/*
 * Get the total pixel width of the non-fixed cell area
 */
static void
GetCellTotalWidth(mw)
XbaeMatrixWidget mw;
{
    int i;

    /*
     * Calculate width of non-fixed cell area.
     */
    for (i = mw->matrix.fixed_columns, mw->matrix.cell_total_width = 0;
         i < mw->matrix.columns;
         i++)
        mw->matrix.cell_total_width += COLUMN_WIDTH(mw, i);
}

/*
 * Cache the pixel position of each column
 */
static void
GetColumnPositions(mw)
XbaeMatrixWidget mw;
{
    int i, x;

    for (i = 0, x = 0;
         i < mw->matrix.columns;
         x += COLUMN_WIDTH(mw, i), i++)
        mw->matrix.column_positions[i] = x;
}


/*
 * Callbacks for our scrollbars.
 */
static XtCallbackRec VSCallback[] =
{
    {(XtCallbackProc) ScrollVertCB, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec HSCallback[] =
{
    {(XtCallbackProc) ScrollHorizCB, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

/* ARGSUSED */
static void
Initialize(request, new, args, num_args)
XbaeMatrixWidget request, new;
ArgList args;
Cardinal *num_args;
{
    /*
     * Check rows/cols set by resources for consistency/validity
     */
    if (new->matrix.rows == 0 || new->matrix.columns == 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "initialize", "badSize", "XbaeMatrix",
                        "XbaeMatrix: Number of rows or columns is zero",
                        (String *) NULL, (Cardinal *) NULL);
        if (!new->matrix.rows)
            new->matrix.rows = 1;
        if (!new->matrix.columns)
            new->matrix.columns = 1;
    }

    /*
     * Make sure column_widths were specified
     */
    if (new->matrix.column_widths == NULL)
        XtAppErrorMsg(XtWidgetToApplicationContext((Widget)new),
                      "initialize", "noColumnWidths", "XbaeMatrix",
                      "XbaeMatrix: No columnWidths specified",
                      (String *) NULL, (Cardinal *) NULL);

    /*
     * Only one of fixed_rows/fixed_columns may be specified
     */
    if (new->matrix.fixed_rows && new->matrix.fixed_columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                      "initialize", "bothFixed", "XbaeMatrix",
                  "XbaeMatrix: Cannot specify both fixedRows and fixedColumns",
                      (String *) NULL, (Cardinal *) NULL);
        new->matrix.fixed_rows = 0;
        new->matrix.fixed_columns = 0;
    }


    /*
     * We must have at least one non-fixed row/column
     */
    if ((new->matrix.fixed_rows &&
         new->matrix.fixed_rows >= new->matrix.rows) ||
        (new->matrix.fixed_columns &&
         new->matrix.fixed_columns >= new->matrix.columns)) {
        String params[1];
        Cardinal num_params = 1;
        params[0] = new->matrix.fixed_rows ? "row" : "column";
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "initialize", "tooManyFixed", "XbaeMatrix",
                        "XbaeMatrix: At least one %s must not be fixed",
                        (String *) params, (Cardinal *) &num_params);
        if (new->matrix.fixed_rows)
            new->matrix.fixed_rows = 0;
        else if (new->matrix.fixed_columns)
            new->matrix.fixed_columns = 0;
    }

    /*
     * We can't have too many visible columns
     */
    if (new->matrix.visible_columns >
        new->matrix.columns - new->matrix.fixed_columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "initialize", "tooManyVisibleColumns", "XbaeMatrix",
                        "XbaeMatrix: Too many visible columns",
                        (String *) NULL, (Cardinal *) NULL);
        new->matrix.visible_columns = 0;
    }

    /*
     * Copy the pointed to resources.
     * If cells is NULL, we create an array of "" strings.
     */

    CopyCells(new);

    if (new->matrix.row_labels)
        CopyRowLabels(new);
    if (new->matrix.column_labels)
        CopyColumnLabels(new);
    else {
        new->matrix.column_label_lines = NULL;
        new->matrix.column_label_maxlines = 0;
    }

    CopyColumnWidths(new);

    if (new->matrix.column_max_lengths)
        CopyColumnMaxLengths(new);

    if (new->matrix.column_alignments)
        CopyColumnAlignments(new);
    if (new->matrix.column_label_alignments)
        CopyColumnLabelAlignments(new);

    if (new->matrix.colors)
        CopyColors(new);

    CopySelectedCells(new);

    /*
     * If user didn't specify a rowLabelWidth, then calculate one based on
     * the widest label
     */
    if (new->matrix.row_label_width == 0 && new->matrix.row_labels)
        new->matrix.row_label_width = MaxRowLabel(new);

    /*
     * Copy the fontList. Get fontStruct from fontList.
     */
    NewFont(new);

    /*
     * Create our 4 children (SBs and textField are unmanaged for now)
     * they must be created in this order so our macros work
     * (horiz scroll, vert scroll and then clip and textField).
     * We scroll horizontally by pixels, vertically by rows.
     */

    (void) XtVaCreateWidget("horizScroll",
                            xmScrollBarWidgetClass, (Widget)new,
                            XmNorientation,     XmHORIZONTAL,
                            XmNdragCallback,    HSCallback,
                            XmNvalueChangedCallback, HSCallback,
                            XmNincrement,       FONT_WIDTH(new),
                            XmNsliderSize,      1,
                            XmNbackground,      new->core.background_pixel,
                            XmNforeground,      new->manager.foreground,
                            XmNbottomShadowColor,
                                new->manager.bottom_shadow_color,
                            XmNbottomShadowPixmap,
                                new->manager.bottom_shadow_pixmap,
                            XmNhighlightColor,  new->manager.highlight_color,
                            XmNhighlightPixmap, new->manager.highlight_pixmap,
                            XmNshadowThickness, new->manager.shadow_thickness,
                            XmNtopShadowColor,  new->manager.top_shadow_color,
                            XmNtopShadowPixmap, new->manager.top_shadow_pixmap,
                            NULL);
    HORIZ_ORIGIN(new) = 0;

    (void) XtVaCreateWidget("vertScroll",
                            xmScrollBarWidgetClass,     (Widget)new,
                            XmNorientation,             XmVERTICAL,
                            XmNdragCallback,            VSCallback,
                            XmNvalueChangedCallback,    VSCallback,
                            XmNincrement,       1,
                            XmNminimum,         0,
                            XmNmaximum,         (new->matrix.rows -
                                                 (int) new->matrix.fixed_rows),
                            XmNsliderSize,      1,
                            XmNbackground,      new->core.background_pixel,
                            XmNforeground,      new->manager.foreground,
                            XmNbottomShadowColor,
                                new->manager.bottom_shadow_color,
                            XmNbottomShadowPixmap,
                                new->manager.bottom_shadow_pixmap,
                            XmNhighlightColor,  new->manager.highlight_color,
                            XmNhighlightPixmap, new->manager.highlight_pixmap,
                            XmNshadowThickness, new->manager.shadow_thickness,
                            XmNtopShadowColor,  new->manager.top_shadow_color,
                            XmNtopShadowPixmap, new->manager.top_shadow_pixmap,
                            NULL);

    /*
     * Create the Clip widget managed so we can use it for traversal
     */
    (void) XtVaCreateManagedWidget("clip",
                                   xbaeClipWidgetClass, (Widget)new,
                                   XmNexposeProc,       ClipRedisplay,
                                   XmNtraversalOn,
                                        new->manager.traversal_on,
                                   XmNbackground,
                                        new->core.background_pixel,
                                   NULL);

    /*
     * Add a callback to the Clip widget so we know when it gets the focus
     * and can use it in traversal.
     */
    XtAddCallback(ClipChild(new), XmNfocusCallback, TraverseInCB,
                  (XtPointer)new);

    /*
     * Create text field (unmanaged for now) - its window will be reparented
     * in Realize to be a subwindow of Clip
     */
    (void) XtVaCreateWidget("textField",
                            xmTextFieldWidgetClass,  (Widget)new,
                            XmNmarginWidth,     new->matrix.cell_margin_width,
                            XmNmarginHeight,    new->matrix.cell_margin_height,
                            XmNtranslations,    new->matrix.text_translations,
                            XmNfontList,        new->matrix.font_list,
                            XmNshadowThickness,
                                        new->matrix.cell_shadow_thickness,
                            XmNhighlightThickness,
                                        new->matrix.cell_highlight_thickness,
                            XmNbackground,      new->core.background_pixel,
                            XmNforeground,      new->manager.foreground,
                            XmNbottomShadowColor,
                                        new->manager.bottom_shadow_color,
                            XmNbottomShadowPixmap,
                                        new->manager.bottom_shadow_pixmap,
                            XmNhighlightColor,  new->manager.highlight_color,
                            XmNhighlightPixmap, new->manager.highlight_pixmap,
                            XmNtopShadowColor,  new->manager.top_shadow_color,
                            XmNtopShadowPixmap, new->manager.top_shadow_pixmap,
                            NULL);

    /*
     * We need a hook onto textField for the modifyVerifyCallback
     * XXX we always have a modifyVerifyCallback even if the user
     * doesn't want one - how else can we tell when we need to
     * reinstall callbacks on the textField?
     */
    XtAddCallback(TextChild(new), XmNmodifyVerifyCallback, ModifyVerifyCB,
                  (XtPointer)new);

    /*
     * Compute cell text baseline based on TextField widget
     */
    new->matrix.text_baseline = XmTextFieldGetBaseline(TextChild(new));

    /*
     * Calculate total pixel width of cell area
     */
    GetCellTotalWidth(new);

    /*
     * Cache the pixel position of each column
     */
    new->matrix.column_positions = CreateColumnPositions(new);
    GetColumnPositions(new);

    /*
     * Now we can set the VSB maximum (relies on data from
     * GetCellTotalWidth above)
     */
    XtVaSetValues(HorizScrollChild(new),
                  XmNmaximum,   CELL_TOTAL_WIDTH(new),
                  NULL);

    /*
     * Current position starts at the top left editable cell.
     */
    new->matrix.current_row = new->matrix.fixed_rows;
    new->matrix.current_column = new->matrix.fixed_columns;

    /*
     * We aren't trying to traverse out
     */
    new->matrix.traversing = NOT_TRAVERSING;

    /*
     * Get/create our GCs
     */
    CreateDrawGC(new);
    GetInverseGC(new);
    CreateDrawClipGC(new);
    CreateInverseClipGC(new);
    CreateTopShadowClipGC(new);
    CreateBottomShadowClipGC(new);

    /*
     * Create ScrollMgrs to manage scrolling events
     */
    new->matrix.matrix_scroll_mgr = SmCreateScrollMgr();
    new->matrix.clip_scroll_mgr = SmCreateScrollMgr();

    /*
     * Compute our size.  If either dimension was explicitly set to 0,
     * then that dimension is computed.
     * Use request because superclasses modify width/height.
     */
    if (request->core.width == 0 || request->core.height == 0)
        ComputeSize(new, request->core.width == 0, request->core.height == 0);

    /*
     * Make sure top_row is sensible before we call Resize
     */
    if (VERT_ORIGIN(new) < 0)
        VERT_ORIGIN(new) = 0;
    else if (VERT_ORIGIN(new) > new->matrix.rows)
        VERT_ORIGIN(new) = new->matrix.rows;

    /*
     * Tweak top_row to make sure it is valid before calling Resize
     */
    if (VERT_ORIGIN(new))
        AdjustTopRow(new);

    /*
     * Layout the scrollbars and clip widget based on our size
     */
    Resize(new);
}

static void
Realize(mw, valueMask, attributes)
XbaeMatrixWidget mw;
XtValueMask *valueMask;
XSetWindowAttributes *attributes;
{
    *valueMask |= CWDontPropagate;
    attributes->do_not_propagate_mask =
        ButtonPressMask | ButtonReleaseMask |
        KeyPressMask | KeyReleaseMask | PointerMotionMask;

    /*
     * Don't call our superclasses realize method, because Manager sets
     * bit_gravity
     */
    XtCreateWindow((Widget)mw, InputOutput, CopyFromParent,
                   *valueMask, attributes);

    /*
     * Reparent the textFields window to be a subwindow of Clip widget
     * (we need to realize them first)
     */
    XtRealizeWidget(TextChild(mw));
    XtRealizeWidget(ClipChild(mw));
    XReparentWindow(XtDisplay(mw), XtWindow(TextChild(mw)),
                    XtWindow(ClipChild(mw)),
                    TextChild(mw)->core.x, TextChild(mw)->core.y);
}

static void
InsertChild(w)
Widget w;
{
    if (((CompositeWidget)XtParent(w))->composite.num_children > 3) {
        String params[1];
        Cardinal num_params = 1;
        params[0] = XtClass(XtParent(w))->core_class.class_name;
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "insertChild", "badChild", "XbaeMatrix",
                  "XbaeMatrix: Applications cannot add children to %s widgets",
                        params, &num_params);
        return;
    }

    (*((XmManagerWidgetClass)
       (xbaeMatrixWidgetClass->core_class.superclass))->composite_class.
     insert_child) (w);
}

/*
 * Convert a pixel position to the column it is contained in.
 */
static int
XtoCol(mw, x)
XbaeMatrixWidget mw;
int x;
{
    int i;

    for (i = 1; i < mw->matrix.columns; i++)
        if (mw->matrix.column_positions[i] > x)
            return i-1;

    return i-1;
}

/*
 * Convert a row/column cell position to the x/y of its upper left corner
 * wrt the window it will be drawn in (either the matrix window for
 * fixed cells, or the clip window for non-fixed).
 */
static void
RowColToXY(mw, row, column, x, y)
XbaeMatrixWidget mw;
int row, column;
int *x, *y;
{
    /*
     * If we are in a fixed cell, calculate x/y relative to Matrixs
     * window (take into account labels etc)
     */
    if (column < mw->matrix.fixed_columns || row < mw->matrix.fixed_rows) {

        /*
         * Ignore horiz_origin if we are in a fixed column
         */
        if (column < mw->matrix.fixed_columns)
            *x = COLUMN_LABEL_OFFSET(mw) + mw->matrix.column_positions[column];
        else
            *x = COLUMN_LABEL_OFFSET(mw) +
                mw->matrix.column_positions[column] - HORIZ_ORIGIN(mw);

        /*
         * Ignore vert_origin if we are in a fixed row
         */
        if (row < mw->matrix.fixed_rows)
            *y = ROW_LABEL_OFFSET(mw) + ROW_HEIGHT(mw) * row;
        else
            *y = ROW_LABEL_OFFSET(mw) + ROW_HEIGHT(mw) *
                (row - VERT_ORIGIN(mw));
    }

    /*
     * If we are not fixed we must account for fixed rows/columns
     * and scrolling origins.
     */
    else {
        *x = (mw->matrix.column_positions[column] -
              mw->matrix.column_positions[mw->matrix.fixed_columns]) -
                  HORIZ_ORIGIN(mw);
        *y = ROW_HEIGHT(mw) * ((row - (int)mw->matrix.fixed_rows) -
                               VERT_ORIGIN(mw));
    }
}

/*
 * Convert an x/y pixel position to the row/column cell position it picks.
 * 'cell' specifies whether the x/y coord is relative to the fixed cells
 * window or the non-fixed cells window.
 * The coords x,y are adjusted so they are relative to the origin of the
 * picked cell.
 */
static Boolean
XYToRowCol(mw, x, y, row, column, cell)
XbaeMatrixWidget mw;
int *x, *y;
int *row, *column;
CellType cell;
{
    Rectangle rect;

    switch (cell) {

    case FixedCell:

        /*
         * Get the Rectangle enclosing the cells in fixed rows or columns.
         * If we don't have fixed rows or columns, then we didn't hit a cell.
         */
        if (mw->matrix.fixed_columns) {
            SETRECT(rect,
                    COLUMN_LABEL_OFFSET(mw), ROW_LABEL_OFFSET(mw),
                    FIXED_COLUMN_LABEL_OFFSET(mw) - 1,
                    ROW_LABEL_OFFSET(mw) + VISIBLE_HEIGHT(mw) - 1);
        }
        else if (mw->matrix.fixed_rows) {
            SETRECT(rect,
                    COLUMN_LABEL_OFFSET(mw), ROW_LABEL_OFFSET(mw),
                    COLUMN_LABEL_OFFSET(mw) + VISIBLE_WIDTH(mw) - 1,
                    FIXED_ROW_LABEL_OFFSET(mw) - 1);
        }
        else
            return False;

        /*
         * If the point is in this rectangle, calculate the row/column
         * it hits. Otherwise we didn't hit a cell.
         */
        if (INBOX(rect, *x, *y)) {

            /*
             * Translate the point to rect's coord system
             */
            *x -= rect.x1;
            *y -= rect.y1;

            /*
             * Convert this point to a row/column. We only take into
             * account the scrolling origins if we are not fixed in that
             * dimension.
             */
            *row = YtoRow(mw, *y) + (mw->matrix.fixed_rows
                                     ? 0
                                     : VERT_ORIGIN(mw));
            *column = XtoCol(mw, *x + (mw->matrix.fixed_columns
                                       ? 0
                                       : HORIZ_ORIGIN(mw)));

            /*
             * Sanity check the result
             */
            if ((mw->matrix.fixed_rows && *row >= mw->matrix.fixed_rows) ||
                (mw->matrix.fixed_columns && *column >=
                 mw->matrix.fixed_columns) ||
                *row < 0 || *column < 0)
                return False;

            /*
             * Adjust x,y so they are relative to this cells origin.
             */
            *x -= mw->matrix.column_positions[*column] -
                (mw->matrix.fixed_columns
                 ? 0
                 : HORIZ_ORIGIN(mw));
            *y %= ROW_HEIGHT(mw);

            return True;
        }
        else
            return False;

        /* NOTREACHED */
        break;

    case NonFixedCell:

        /*
         * Translate the point to take into account fixed rows or columns.
         */
        *x += FIXED_COLUMN_WIDTH(mw);
        *y += FIXED_ROW_HEIGHT(mw);

        /*
         * Convert the new point to a row/column position
         */
        *row = YtoRow(mw, *y) + VERT_ORIGIN(mw);
        *column = XtoCol(mw, *x + HORIZ_ORIGIN(mw));

        /*
         * Sanity check the result
         */
        if (*row >= mw->matrix.rows || *column >= mw->matrix.columns ||
            *row < 0 || *column < 0)
            return False;

        /*
         * Adjust x,y so they are relative to this cells origin.
         */
        *x -= mw->matrix.column_positions[*column] - HORIZ_ORIGIN(mw);
        *y %= ROW_HEIGHT(mw);

        return True;

        /* NOTREACHED */
        break;

    default:
        return False;
    }
}

/*
 * Width in pixels of a character in a given font
 */
#define charWidth(fs,c) ((fs)->per_char\
                         ? (fs)->per_char[(unsigned char)(c) -\
                                          (fs)->min_char_or_byte2].width\
                         : (fs)->min_bounds.width)


/*
 * Draw a string with specified attributes. We want to avoid having to
 * use a GC clip_mask, so we clip by characters. This complicates the code.
 */
static void
DrawString(mw, win, string, length, x, y, maxlen, alignment, highlight,
           bold, clip, color)
XbaeMatrixWidget mw;
Window win;
String string;
int length;
int x, y;
int maxlen;
unsigned char alignment;
Boolean highlight;
Boolean bold;
Boolean clip;
Pixel color;
{
    int start, width, maxwidth;
    GC gc;

    /*
     * Initialize starting character in string
     */
    start = 0;

    /*
     * Calculate max width in pixels and actual pixel width of string.
     * XXX XTextWidth only sums the width of each char.  A char could
     * have an rbearing greater than the width.  So we might not clip
     * a char which really should be.
     */
    maxwidth = maxlen * FONT_WIDTH(mw);
    width = XTextWidth(mw->matrix.font, string, length);

    /*
     * If the width of the string is greater than the width of this cell,
     * we need to clip. We don't want to use the server to clip because
     * it is slow, so we truncate characters if we exceed a cells pixel width.
     */
    if (width > maxwidth) {

        switch (alignment) {

        case XmALIGNMENT_CENTER: {
            int startx = x;
            int endx = x + maxwidth - 1;
            int newendx;

            /*
             * Figure out our x for the centered string.  Then loop and chop
             * characters off the front until we are within the cell.
             * Adjust x, the starting character and the length of the string
             * for each char.
             */
            x += maxwidth / 2 - width / 2;
            while (x < startx) {
                int cw = charWidth(mw->matrix.font, string[start]);
                x += cw;
                width -= cw;
                length--;
                start++;
            }

            /*
             * Now figure out the end x of the string.  Then loop and chop
             * characters off the end until we are within the cell.
             */
            newendx = x + width - 1;
            while (newendx > endx) {
                newendx -= charWidth(mw->matrix.font,
                                     string[start + length - 1]);
                length--;
            }

            break;
        }

        case XmALIGNMENT_END: {

            /*
             * Figure out our x for the right justified string.
             * Then loop and chop characters off the front until we fit.
             * Adjust x for each char lopped off. Also adjust the starting
             * character and length of the string for each char.
             */
            x += maxwidth - width;
            while (width > maxwidth) {
                int cw = charWidth(mw->matrix.font, string[start]);
                width -= cw;
                x += cw;
                length--;
                start++;
            }
            break;
        }

        case XmALIGNMENT_BEGINNING:
        default:
            /*
             * Leave x alone, but chop characters off the end until we fit
             */
            while (width > maxwidth) {
                width -= charWidth(mw->matrix.font, string[length - 1]);
                length--;
            }
            break;
        }
    }

    /*
     * We fit inside our cell, so just compute the x of the start of our string
     */
    else {
        switch (alignment) {

        case XmALIGNMENT_CENTER:
            x += maxwidth / 2 - width / 2;
            break;

        case XmALIGNMENT_END:
            x += maxwidth - width;
            break;

        case XmALIGNMENT_BEGINNING:
        default:
            /*
             * Leave x alone
             */
            break;
        }
    }

    /*
     * Figure out which GC to use
     */
    if (highlight) {
        if (clip)
            gc = mw->matrix.inverse_clip_gc;
        else
            gc = mw->matrix.inverse_gc;
    }
    else {
        if (clip)
            gc = mw->matrix.draw_clip_gc;
        else
            gc = mw->matrix.draw_gc;
    }

    /*
     * Don't worry, XSetForeground is smart about avoiding unnecessary
     * protocol requests.
     */
    if (!highlight)
        XSetForeground(XtDisplay(mw), gc, color);

    /*
     * Now draw the string at x starting at char 'start' and of length 'length'
     */
    XDrawString(XtDisplay(mw), win, gc, x, y, &string[start], length);

    /*
     * If bold is on, draw the string again offset by 1 pixel (overstrike)
     */
    if (bold)
        XDrawString(XtDisplay(mw), win, gc, x - 1, y, &string[start], length);
}

/*
 * Draw the column label for the specified column.  Handles labels in
 * fixed and non-fixed columns.
 */
static void
DrawColumnLabel(mw, column)
XbaeMatrixWidget mw;
int column;
{
    String label;
    int x, y, i;

    if (mw->matrix.column_labels[column][0] == '\0')
        return;

    /*
     * If the column label is in a fixed column, we don't need to account
     * for the horiz_origin
     */
    if (column < mw->matrix.fixed_columns)
        x = COLUMN_LABEL_OFFSET(mw) + mw->matrix.column_positions[column] +
            TEXT_X_OFFSET(mw);
    else
        x = COLUMN_LABEL_OFFSET(mw) + (mw->matrix.column_positions[column] -
                                       HORIZ_ORIGIN(mw)) + TEXT_X_OFFSET(mw);

    /*
     * Set our y to the baseline of the first line in this column
     */
    y = TEXT_Y_OFFSET(mw) + (mw->matrix.column_label_maxlines -
                             mw->matrix.column_label_lines[column].lines) *
                                 TEXT_HEIGHT(mw);

    label = mw->matrix.column_labels[column];
    for (i = 0; i < mw->matrix.column_label_lines[column].lines; i++) {
        DrawString(mw, XtWindow(mw),
                   label, mw->matrix.column_label_lines[column].lengths[i],
                   x, y,
                   mw->matrix.column_widths[column],
                   mw->matrix.column_label_alignments
                   ? mw->matrix.column_label_alignments[column]
                   : XmALIGNMENT_BEGINNING,
                   False,
                   mw->matrix.bold_labels,
                   column >= mw->matrix.fixed_columns,
                   mw->manager.foreground);
        y += TEXT_HEIGHT(mw);
        label += mw->matrix.column_label_lines[column].lengths[i] + 1;
    }
}

/*
 * Draw the row label for the specified row. Handles labels in fixed and
 * non-fixed rows.
 */
static void
DrawRowLabel(mw, row)
XbaeMatrixWidget mw;
int row;
{
    int y;

    if (mw->matrix.row_labels[row][0] == '\0')
        return;

    /*
     * If the row label is in a fixed row we don't need to account
     * for the vert_origin
     */
    if (row < mw->matrix.fixed_rows)
        y = ROW_LABEL_OFFSET(mw) + ROW_HEIGHT(mw) * row + TEXT_Y_OFFSET(mw);
    else
        y = ROW_LABEL_OFFSET(mw) + ROW_HEIGHT(mw) * (row - VERT_ORIGIN(mw)) +
            TEXT_Y_OFFSET(mw);

    DrawString(mw, XtWindow(mw),
               mw->matrix.row_labels[row],
               strlen(mw->matrix.row_labels[row]),
               TEXT_X_OFFSET(mw), y,
               mw->matrix.row_label_width,
               mw->matrix.row_label_alignment,
               False,
               mw->matrix.bold_labels,
               False,
               mw->manager.foreground);
}

/*
 * Draw a fixed or non-fixed cell. The coordinates are calculated relative
 * to the correct window and the cell is drawn in that window.
 */
static void
DrawCell(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    int x, y;
    Pixel color;
    Window win = CELL_WINDOW(mw, row, column);
    Boolean clipped = row < mw->matrix.fixed_rows;

    if (!win)
        return;

    /*
     * Convert the row/column to the coordinates relative to the correct
     * window
     */
    RowColToXY(mw, row, column, &x, &y);

    /*
     * Figure out what color this cell is
     */
    if (mw->matrix.colors)
        color = mw->matrix.colors[row][column];
    else
        color = mw->manager.foreground;

    /*
     * If this cell is selected, fill the cell with the foreground color
     */
    if (mw->matrix.selected_cells[row][column]) {
        GC gc = (clipped
                 ? mw->matrix.draw_clip_gc
                 : mw->matrix.draw_gc);

        /*
         * Don't worry, XSetForeground is smart about avoiding
         * unnecessary protocol requests.
         */
        XSetForeground(XtDisplay(mw), gc, color);

        XFillRectangle(XtDisplay(mw), win,
                       gc,
                       x + (mw->matrix.cell_highlight_thickness +
                            mw->matrix.cell_shadow_thickness),
                       y + (mw->matrix.cell_highlight_thickness +
                            mw->matrix.cell_shadow_thickness),
                       COLUMN_WIDTH(mw, column) -
                                (mw->matrix.cell_highlight_thickness +
                                 mw->matrix.cell_shadow_thickness) * 2,
                       ROW_HEIGHT(mw) -
                                (mw->matrix.cell_highlight_thickness +
                                 mw->matrix.cell_shadow_thickness) * 2);
    }

    /*
     * Draw the string in the cell.
     */
    if (*mw->matrix.cells[row][column] != '\0') {
        DrawString(mw, win,
                   mw->matrix.cells[row][column],
                   strlen(mw->matrix.cells[row][column]),
                   x + TEXT_X_OFFSET(mw), y + TEXT_Y_OFFSET(mw),
                   mw->matrix.column_widths[column],
                   mw->matrix.column_alignments
                   ? mw->matrix.column_alignments[column]
                   : XmALIGNMENT_BEGINNING,
                   mw->matrix.selected_cells[row][column],
                   False, clipped, color);
    }

    /*
     * Surround the cell with a shadow.
     */
    if (mw->matrix.cell_shadow_thickness) {
        if (clipped)
            _XmDrawShadow(XtDisplay(mw), win,
                          mw->matrix.cell_bottom_shadow_clip_gc,
                          mw->matrix.cell_top_shadow_clip_gc,
                          mw->matrix.cell_shadow_thickness,
                          x + (int)mw->matrix.cell_highlight_thickness,
                          y + (int)mw->matrix.cell_highlight_thickness,
                          COLUMN_WIDTH(mw, column) -
                                2 * mw->matrix.cell_highlight_thickness,
                          ROW_HEIGHT(mw) -
                                2 * mw->matrix.cell_highlight_thickness);
        else
            _XmDrawShadow(XtDisplay(mw), win,
                          mw->manager.bottom_shadow_GC,
                          mw->manager.top_shadow_GC,
                          mw->matrix.cell_shadow_thickness,
                          x + (int)mw->matrix.cell_highlight_thickness,
                          y + (int)mw->matrix.cell_highlight_thickness,
                          COLUMN_WIDTH(mw, column) -
                                2 * mw->matrix.cell_highlight_thickness,
                          ROW_HEIGHT(mw) -
                                2 * mw->matrix.cell_highlight_thickness);
    }
}

/*
 * Redraw the row and column labels and the cells in fixed rows/columns
 * that are overlapped by the Rectangle argument.
 */
static void
RedrawLabelsAndFixed(mw, expose)
XbaeMatrixWidget mw;
Rectangle *expose;
{
    /*
     * Handle the row labels that are in fixed rows
     */
    if (mw->matrix.fixed_rows && mw->matrix.row_labels) {
        Rectangle rect;

        /*
         * Get the Rectangle enclosing the fixed row labels
         */
        SETRECT(rect,
                0, ROW_LABEL_OFFSET(mw),
                ROW_LABEL_WIDTH(mw) - 1, FIXED_ROW_LABEL_OFFSET(mw) - 1);

        /*
         * If the expose Rectangle overlaps, then some labels must be redrawn
         */
        if (OVERLAP(*expose, rect)) {
            Rectangle intersect;
            int endRow, i;

            /*
             * Intersect the fixed-row-labels Rectangle with the expose
             * Rectangle along the Y axis.  The resulting Rectangle will
             * be in 'rect's coordinate system.
             */
            Y_INTERSECT(rect, *expose, intersect);

            /*
             * Redraw each label that was intersected
             */
            endRow = YtoRow(mw, intersect.y2);
            for (i = YtoRow(mw, intersect.y1); i <= endRow; i++)
                DrawRowLabel(mw, i);
        }
    }

    /*
     * Handle row labels that aren't in fixed rows
     */
    if (mw->matrix.row_labels) {
        Rectangle rect;

        /*
         * Get the Rectangle enclosing the non-fixed row labels
         */
        SETRECT(rect,
                0, FIXED_ROW_LABEL_OFFSET(mw),
                ROW_LABEL_WIDTH(mw) - 1,
                FIXED_ROW_LABEL_OFFSET(mw) + VISIBLE_HEIGHT(mw) - 1);

        /*
         * If the expose Rectangle overlaps, then some labels must be redrawn
         */
        if (OVERLAP(*expose, rect)) {
            Rectangle intersect;
            int endRow, i;

            /*
             * Intersect the fixed-row-labels Rectangle with the expose
             * Rectangle along the Y axis.  The resulting Rectangle will
             * be in 'rect's coordinate system.
             */
            Y_INTERSECT(rect, *expose, intersect);

            /*
             * Translate 'intersect' to take into account any fixed rows.
             * This gets it back into the coord system expected by YtoRow().
             */
            intersect.y1 += FIXED_ROW_HEIGHT(mw);
            intersect.y2 += FIXED_ROW_HEIGHT(mw);

            /*
             * Redraw each label that was intersected
             */
            endRow = YtoRow(mw, intersect.y2) + VERT_ORIGIN(mw);
            for (i = YtoRow(mw, intersect.y1) + VERT_ORIGIN(mw);
                 i <= endRow;
                 i++)
                DrawRowLabel(mw, i);
        }
    }

    /*
     * Handle the column labels that are in fixed columns
     */
    if (mw->matrix.fixed_columns && mw->matrix.column_labels) {
        Rectangle rect;

        /*
         * Get the Rectangle enclosing the portion of the column labels
         * that are in fixed columns
         */
        SETRECT(rect,
                COLUMN_LABEL_OFFSET(mw), 0,
                FIXED_COLUMN_LABEL_OFFSET(mw) - 1,
                COLUMN_LABEL_HEIGHT(mw) - 1);

        /*
         * If the expose Rectangle overlaps, then some labels must be redrawn
         */
        if (OVERLAP(*expose, rect)) {
            Rectangle intersect;
            int endCol, i;

            /*
             * Intersect the fixed-column-labels Rectangle with the expose
             * Rectangle along the X axis.  The resulting Rectangle will
             * be in 'rect's coordinate system.
             */
            X_INTERSECT(rect, *expose, intersect);

            /*
             * Redraw each label that was intersected
             */
            endCol = XtoCol(mw, intersect.x2);
            for (i = XtoCol(mw, intersect.x1); i <= endCol; i++)
                DrawColumnLabel(mw, i);
        }
    }

    /*
     * Handle column labels that aren't in fixed columns
     */
    if (mw->matrix.column_labels) {
        Rectangle rect;

        /*
         * Get the Rectangle enclosing the non-fixed column labels
         */
        SETRECT(rect,
                FIXED_COLUMN_LABEL_OFFSET(mw), 0,
                FIXED_COLUMN_LABEL_OFFSET(mw) + VISIBLE_WIDTH(mw) - 1,
                COLUMN_LABEL_HEIGHT(mw) - 1);

        /*
         * If the expose Rectangle overlaps, then some labels must be redrawn
         */
        if (OVERLAP(*expose, rect)) {
            Rectangle intersect;
            int endCol, i;

            /*
             * Intersect the non-fixed-column-labels Rectangle with the expose
             * Rectangle along the X axis.  The resulting Rectangle will
             * be in 'rect's coordinate system.
             */
            X_INTERSECT(rect, *expose, intersect);

            /*
             * Translate 'intersect' to take into account any fixed columns.
             * This gets it back into the coord system expected by XtoCol().
             */
            intersect.x1 += FIXED_COLUMN_WIDTH(mw);
            intersect.x2 += FIXED_COLUMN_WIDTH(mw);

            /*
             * Redraw each label that was intersected
             */
            endCol = XtoCol(mw, intersect.x2 + HORIZ_ORIGIN(mw));
            for (i = XtoCol(mw, intersect.x1 + HORIZ_ORIGIN(mw));
                 i <= endCol;
                 i++)
                DrawColumnLabel(mw, i);
        }
    }

    /*
     * Handle cells in fixed rows or fixed columns
     */
    if (mw->matrix.fixed_rows || mw->matrix.fixed_columns) {
        Rectangle rect;

        /*
         * Get the Rectangle enclosing the cells in fixed rows or columns
         */
        if (mw->matrix.fixed_columns) {
            SETRECT(rect,
                    COLUMN_LABEL_OFFSET(mw), ROW_LABEL_OFFSET(mw),
                    FIXED_COLUMN_LABEL_OFFSET(mw) - 1,
                    ROW_LABEL_OFFSET(mw) + VISIBLE_HEIGHT(mw) - 1);
        }
        else if (mw->matrix.fixed_rows) {
            SETRECT(rect,
                    COLUMN_LABEL_OFFSET(mw), ROW_LABEL_OFFSET(mw),
                    COLUMN_LABEL_OFFSET(mw) + VISIBLE_WIDTH(mw) - 1,
                    FIXED_ROW_LABEL_OFFSET(mw) - 1);
        }

        /*
         * If the expose Rectangle overlaps, then some cells must be redrawn
         */
        if (OVERLAP(*expose, rect)) {
            Rectangle intersect;
            int startCol, endCol, startRow, endRow, i, j;

            /*
             * Intersect the fixed-cells Rectangle with the expose
             * Rectangle along the X and Y axis.  The resulting Rectangle will
             * be in 'rect's coordinate system.
             */
            INTERSECT(rect, *expose, intersect);

            /*
             * Get starting and ending rows/columns.  We only take into
             * account the scrolling origins if we are not fixed in that
             * dimension.
             */
            startCol = XtoCol(mw, intersect.x1 +
                              (mw->matrix.fixed_columns
                               ? 0
                               : HORIZ_ORIGIN(mw)));
            endCol = XtoCol(mw, intersect.x2 +
                            (mw->matrix.fixed_columns
                             ? 0
                             : HORIZ_ORIGIN(mw)));
            startRow = YtoRow(mw, intersect.y1) +
                (mw->matrix.fixed_rows
                 ? 0
                 : VERT_ORIGIN(mw));
            endRow = YtoRow(mw, intersect.y2) +
                (mw->matrix.fixed_rows
                 ? 0
                 : VERT_ORIGIN(mw));

            /*
             * Redraw each cell that was intersected
             */
            for (i = startRow; i <= endRow; i++)
                for (j = startCol; j <= endCol; j++)
                    DrawCell(mw, i, j);
        }
    }

    /*
     * Draw a shadow just inside row/column labels and around outer edge
     * of clip widget.  We can't use height of clip widget because it is
     * truncated to nearest row.  We use cell_visible_height instead.
     */
    if (mw->manager.shadow_thickness)
        _XmDrawShadow(XtDisplay(mw), XtWindow(mw),
                      mw->manager.bottom_shadow_GC,
                      mw->manager.top_shadow_GC,
                      mw->manager.shadow_thickness,
                      ROW_LABEL_WIDTH(mw),
                      COLUMN_LABEL_HEIGHT(mw),
                      ClipChild(mw)->core.width + FIXED_COLUMN_WIDTH(mw) +
                          2 * mw->manager.shadow_thickness,
                      mw->matrix.cell_visible_height + FIXED_ROW_HEIGHT(mw) +
                          2 * mw->manager.shadow_thickness);
}

/*
 * This is the expose method for the Matrix widget.
 * It redraws the row and column labels, the cells in fixed rows and columns
 * and the clip window shadow.
 */
/* ARGSUSED */
static void
Redisplay(mw, event, region)
XbaeMatrixWidget mw;
XEvent *event;
Region region;
{
    Rectangle expose;

    /*
     * Send our events to the mw ScrollMgr to be adjusted.
     */
    switch (event->type) {

    case Expose:
        /*
         * The Expose event will be translated into our scrolled
         * coordinate system.  Then it is put in a Rectangle.
         */
        SmScrollEvent(mw->matrix.matrix_scroll_mgr, event);
        SETRECT(expose,
                event->xexpose.x, event->xexpose.y,
                event->xexpose.x + event->xexpose.width,
                event->xexpose.y + event->xexpose.height);
        break;

    case GraphicsExpose:
        /*
         * The GraphicsExpose event will cause a scroll to be removed
         * from the managers queue, then the event will be translated
         * into our scrolled coordinate system.  Then it is put in a Rectangle.
         */
        SmScrollEvent(mw->matrix.matrix_scroll_mgr, event);
        SETRECT(expose,
                event->xgraphicsexpose.x, event->xgraphicsexpose.y,
                event->xgraphicsexpose.x + event->xgraphicsexpose.width,
                event->xgraphicsexpose.y + event->xgraphicsexpose.height);
        break;

    case NoExpose:
        /*
         * The NoExpose event means we won't be getting any GraphicsExpose
         * events, so the scroll well be removed from the queue and
         * we are done.
         */
        SmScrollEvent(mw->matrix.matrix_scroll_mgr, event);
        return;

    default:
        return;
    }

    /*
     * Redraw the row/column labels and fixed rows/columns which are
     * overlapped by the expose Rectangle.
     */
    RedrawLabelsAndFixed(mw, &expose);
}

/*
 * Redraw all cells in the clip widget damaged by the passed Rectangle.
 * The Rectangle must be within the bounds of the cells. These are the
 * non-fixed cells.
 */
static void
RedrawCells(mw, expose)
XbaeMatrixWidget mw;
Rectangle *expose;
{
    int startCol, endCol, startRow, endRow, i, j;
    Rectangle rect;

    /*
     * Translate the 'expose' Rectangle to take into account the
     * fixed rows or columns.
     */
    SETRECT(rect,
            expose->x1 + FIXED_COLUMN_WIDTH(mw),
            expose->y1 + FIXED_ROW_HEIGHT(mw),
            expose->x2 + FIXED_COLUMN_WIDTH(mw),
            expose->y2 + FIXED_ROW_HEIGHT(mw));

    /*
     * Calculate the starting and ending rows/columns of the cells
     * which must be redrawn.
     */
    startCol = XtoCol(mw, rect.x1 + HORIZ_ORIGIN(mw));
    endCol = XtoCol(mw, rect.x2 + HORIZ_ORIGIN(mw));
    startRow = YtoRow(mw, rect.y1) + VERT_ORIGIN(mw);
    endRow = YtoRow(mw, rect.y2) + VERT_ORIGIN(mw);

    /*
     * Redraw all cells which were exposed.
     */
    for (i = startRow; i <= endRow; i++)
        for (j = startCol; j <= endCol; j++)
            DrawCell(mw, i, j);
}

/*
 * This is the exposeProc function for the Clip widget.
 * It handles expose events for the Clip widget by redrawing those
 * non-fixed cells which were damaged.
 * It recieves Expose, GraphicsExpose and NoExpose events.
 */
/* ARGSUSED */
static void
ClipRedisplay(w, event, region)
Widget w;
XEvent *event;
Region region;
{
    XbaeMatrixWidget mw = (XbaeMatrixWidget) XtParent(w);
    Rectangle expose, clip, intersect;

    /*
     * Send our events to the clip ScrollMgr to be adjusted.
     */
    switch (event->type) {

    case Expose:
        /*
         * The Expose event will be translated into our scrolled
         * coordinate system.  Then it is put in a Rectangle.
         */
        SmScrollEvent(mw->matrix.clip_scroll_mgr, event);
        SETRECT(expose,
                event->xexpose.x, event->xexpose.y,
                event->xexpose.x + event->xexpose.width - 1,
                event->xexpose.y + event->xexpose.height - 1);
        break;

    case GraphicsExpose:
        /*
         * The GraphicsExpose event will cause a scroll to be removed
         * from the managers queue, then the event will be translated
         * into our scrolled coordinate system.  Then it is put in a Rectangle.
         */
        SmScrollEvent(mw->matrix.clip_scroll_mgr, event);
        SETRECT(expose,
                event->xgraphicsexpose.x, event->xgraphicsexpose.y,
                event->xgraphicsexpose.x + event->xgraphicsexpose.width - 1,
                event->xgraphicsexpose.y + event->xgraphicsexpose.height - 1);
        break;

    case NoExpose:
        /*
         * The NoExpose event means we won't be getting any GraphicsExpose
         * events, so the scroll well be removed from the queue and
         * we are done.
         */
        SmScrollEvent(mw->matrix.clip_scroll_mgr, event);
        return;

    default:
        return;
    }

    /*
     * We may get an expose event larger than the size of the Clip widget.
     * This is because in set_values we may clear the Clip widget
     * before it gets resized smaller (maybe in set_values_almost).
     * So here we intersect the expose event with the clip widget
     * to ensure the expose Rectangle is not larger than the Clip widget.
     */
    SETRECT(clip,
            0, 0,
            w->core.width - 1, w->core.height - 1);
    INTERSECT(clip, expose, intersect);

    /*
     * Redraw those cells which overlap the intersect Rectangle.
     */
    RedrawCells(mw, &intersect);
}


/* ARGSUSED */
static Boolean
SetValues(current, request, new, args, num_args)
XbaeMatrixWidget current, request, new;
ArgList args;
Cardinal *num_args;
{
    Boolean redisplay = False;          /* need to redraw */
    Boolean relayout = False;           /* need to layout, but same size */
    Boolean new_column_widths = False;  /* column widths changed */
    Boolean new_cells = False;          /* cells changed */
    Boolean do_top_row = False;         /* reset top_row */
    int n;
    Arg wargs[9];

#define NE(field)       (current->field != new->field)
#define EQ(field)       (current->field == new->field)



    /*
     * If rows changed, then:
     *  row_labels must change or be NULL
     */
    if (NE(matrix.rows) &&
         (new->matrix.row_labels && EQ(matrix.row_labels))) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "setValues", "rows",
                        "XbaeMatrix",
          "XbaeMatrix: Number of rows changed but dependent resources did not",
                        NULL, 0);
        new->matrix.rows = current->matrix.rows;
        new->matrix.row_labels = current->matrix.row_labels;
    }

    /*
     * If columns changed, then:
     *  column_widths must change
     *  column_max_lengths must change or be NULL
     *  column_labels must change or be NULL
     *  column_alignments must change or be NULL
     *  column_label_alignments must change or be NULL
     */
    if (NE(matrix.columns) &&
         ((new->matrix.column_labels && EQ(matrix.column_labels)) ||
          (new->matrix.column_max_lengths && EQ(matrix.column_max_lengths)) ||
          (new->matrix.column_alignments && EQ(matrix.column_alignments)) ||
          (new->matrix.column_label_alignments &&
           EQ(matrix.column_label_alignments)) ||
          EQ(matrix.column_widths))) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "setValues", "columns", "XbaeMatrix",
      "XbaeMatrix: Number of columns changed but dependent resources did not",
                        NULL, 0);
        new->matrix.columns = current->matrix.columns;
        new->matrix.column_widths = current->matrix.column_widths;
        new->matrix.column_max_lengths = current->matrix.column_max_lengths;
        new->matrix.column_labels = current->matrix.column_labels;
        new->matrix.column_alignments = current->matrix.column_alignments;
        new->matrix.column_label_alignments =
            current->matrix.column_label_alignments;
    }

    /*
     * Make sure we have at least one row/column.
     */
    if (new->matrix.columns <= 0 || new->matrix.rows <= 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "setValues", "size", "XbaeMatrix",
                        "XbaeMatrix: Must have at least one row and column",
                        NULL, 0);
        if (new->matrix.columns <= 0)
            new->matrix.columns = current->matrix.columns;
        if (new->matrix.rows <= 0)
            new->matrix.rows = current->matrix.rows;
    }

    /*
     * We cannot have both fixed rows and fixed columns
     */
    if (new->matrix.fixed_rows && new->matrix.fixed_columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "setValues", "bothFixed", "XbaeMatrix",
                        "XbaeMatrix: Cannot have both fixed rows and columns",
                        NULL, 0);
        if (NE(matrix.fixed_rows))
            new->matrix.fixed_rows = current->matrix.fixed_rows;
        if (NE(matrix.fixed_columns))
            new->matrix.fixed_columns = current->matrix.fixed_columns;
    }

    /*
     * We must have at least one non-fixed row/column.
     * This could be caused by fixed_rows/columns or rows/columns changing.
     */
    if ((new->matrix.fixed_rows &&
         new->matrix.fixed_rows >= new->matrix.rows) ||
        (new->matrix.fixed_columns &&
         new->matrix.fixed_columns >= new->matrix.columns)) {
        String params[1];
        Cardinal num_params = 1;
        params[0] = new->matrix.fixed_rows ? "row" : "column";
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "setValues", "tooManyFixed", "XbaeMatrix",
                        "XbaeMatrix: At least one %s must not be fixed",
                        (String *) params, (Cardinal *) &num_params);
        if (new->matrix.fixed_rows) {
            if (NE(matrix.fixed_rows))
                new->matrix.fixed_rows = current->matrix.fixed_rows;
            if (NE(matrix.rows))
                new->matrix.rows = current->matrix.rows;
        }
        else if (new->matrix.fixed_columns) {
            if (NE(matrix.fixed_columns))
                new->matrix.fixed_columns = current->matrix.fixed_columns;
            if (NE(matrix.columns))
                new->matrix.columns = current->matrix.columns;
        }
    }

    /*
     * We can't have too many visible columns.
     * This could be caused by visible_columns or columns or fixed_columns
     * changing.
     */
    if (new->matrix.visible_columns >
        new->matrix.columns - new->matrix.fixed_columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                      "setValues", "tooManyVisibleColumns", "XbaeMatrix",
"XbaeMatrix: visibleColumns must not be greater than (columns - fixedColumns)",
                      (String *) NULL, (Cardinal *) NULL);
        if (NE(matrix.visible_columns))
            new->matrix.visible_columns = current->matrix.visible_columns;
        if (NE(matrix.columns))
            new->matrix.columns = current->matrix.columns;
        if (NE(matrix.fixed_columns))
            new->matrix.fixed_columns = current->matrix.fixed_columns;
    }

    /*
     * Make sure we have column_widths
     */
    if (new->matrix.column_widths == NULL) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)new),
                        "setValues", "columnWidths", "XbaeMatrix",
                        "XbaeMatrix: Must specify columnWidths",
                        NULL, 0);
        new->matrix.column_widths = current->matrix.column_widths;
    }


    /*
     * If rows or columns or fixed rows/columns changed,
     * then we need to relayout.
     */
    if (NE(matrix.rows) || NE(matrix.fixed_rows)) {
        /*
         * Reset VSB maximum. sliderSize will be reset later in Resize.
         */
        XtVaSetValues(VertScrollChild(new),
                      XmNmaximum,       (new->matrix.rows -
                                         (int) new->matrix.fixed_rows),
                      XmNvalue,         0,
                      XmNsliderSize,    1,
                      NULL);
        do_top_row = True;
        relayout = True;
    }
    if (NE(matrix.columns) || NE(matrix.fixed_columns))
        relayout = True;

    /*
     * Copy any pointed to resources if they changed
     */

    if (NE(matrix.cells)) {
        FreeCells(current);
        CopyCells(new);
        redisplay = True;
        new_cells = True;
    }
    else if (NE(matrix.rows) || NE(matrix.columns))
        ResizeCells(current, new);
    if (NE(matrix.row_labels)) {
        /*
         * If we added or deleted row_labels, we need to layout.
         */
        if (!current->matrix.row_labels || !new->matrix.row_labels)
            relayout = True;
        else
            redisplay = True;

        FreeRowLabels(current);
        if (new->matrix.row_labels)
            CopyRowLabels(new);
    }
    if (NE(matrix.column_labels)) {
        FreeColumnLabels(current);
        if (new->matrix.column_labels)
            CopyColumnLabels(new);
        else {
            new->matrix.column_label_lines = NULL;
            new->matrix.column_label_maxlines = 0;
        }

        /*
         * If the number of lines in column labels changed, we need to relayout
         */
        if (current->matrix.column_label_maxlines !=
            new->matrix.column_label_maxlines)
            relayout = True;
        else
            redisplay = True;
    }
    if (NE(matrix.column_max_lengths)) {
        FreeColumnMaxLengths(current);
        if (new->matrix.column_max_lengths)
            CopyColumnMaxLengths(new);
        redisplay = True;
    }
    if (NE(matrix.column_alignments)) {
        FreeColumnAlignments(current);
        if (new->matrix.column_alignments)
            CopyColumnAlignments(new);
        redisplay = True;
    }
    if (NE(matrix.column_label_alignments)) {
        FreeColumnLabelAlignments(current);
        if (new->matrix.column_label_alignments)
            CopyColumnLabelAlignments(new);
        redisplay = True;
    }
    if (NE(matrix.colors)) {
        FreeColors(current);
        if (new->matrix.colors)
            CopyColors(new);
        redisplay = True;
    }
    else if(new->matrix.colors && (NE(matrix.rows) || NE(matrix.columns)))
        ResizeColors(current, new);
    if (NE(matrix.column_widths)) {
        FreeColumnWidths(current);
        CopyColumnWidths(new);
        relayout = True;
        new_column_widths = True;
    }
    if (NE(matrix.selected_cells)) {
        FreeSelectedCells(current);
        CopySelectedCells(new);
        redisplay = True;
    }
    else if (NE(matrix.rows) || NE(matrix.columns))
        ResizeSelectedCells(current, new);

    /*
     * If traversal changes, pass through to Clip and textField children.
     */
    if (NE(manager.traversal_on)) {
        XtVaSetValues(ClipChild(new),
                      XmNtraversalOn,   new->manager.traversal_on,
                      NULL);
    }

    /*
     * Pass through primitive/manager resources to our children
     */
    n = 0;
    if (NE(core.background_pixel)) {
        XtVaSetValues(ClipChild(new),
                      XmNbackground,            new->core.background_pixel,
                      NULL);
        XtSetArg(wargs[n], XmNbackground, new->core.background_pixel); n++;
    }
    if (NE(manager.foreground)) {
        XtSetArg(wargs[n], XmNforeground, new->manager.foreground); n++;
    }
    if (NE(manager.bottom_shadow_color)) {
        XtSetArg(wargs[n], XmNbottomShadowColor,
                 new->manager.bottom_shadow_color); n++;
    }
    if (NE(manager.bottom_shadow_pixmap)) {
        XtSetArg(wargs[n], XmNbottomShadowPixmap,
                 new->manager.bottom_shadow_pixmap); n++;
    }
    if (NE(manager.highlight_color)) {
        XtSetArg(wargs[n], XmNhighlightColor,
                 new->manager.highlight_color); n++;
    }
    if (NE(manager.highlight_pixmap)) {
        XtSetArg(wargs[n], XmNhighlightPixmap,
                 new->manager.highlight_pixmap); n++;
    }
    if (NE(manager.top_shadow_color)) {
        XtSetArg(wargs[n], XmNtopShadowColor,
                 new->manager.top_shadow_color); n++;
    }
    if (NE(manager.top_shadow_pixmap)) {
        XtSetArg(wargs[n], XmNtopShadowPixmap,
                 new->manager.top_shadow_pixmap); n++;
    }
    if (n) {
        XtSetValues(VertScrollChild(new), wargs, n);
        XtSetValues(HorizScrollChild(new), wargs, n);
        XtSetValues(TextChild(new), wargs, n);
    }


    /*
     * Get a new XFontStruct and copy the fontList if it changed
     * and pass it to the textField.
     * Reset the HSB increment.
     * redisplay and relayout will be set below.
     */
    if (NE(matrix.font_list)) {
        XmFontListFree(current->matrix.font_list);
        NewFont(new);
        XtVaSetValues(TextChild(new),
                      XmNfontList,      new->matrix.font_list,
                      NULL);
        XtVaSetValues(HorizScrollChild(new),
                      XmNincrement,     FONT_WIDTH(new),
                      NULL);
    }

    /*
     * Pass the cell resources on to the textField.
     * Both redisplay and relayout will be set below.
     * XXX XmTextField has a bug where it will not recompute it's size when
     * XmNshadowThickness changes.
     */
    if (NE(matrix.cell_margin_width) ||
        NE(matrix.cell_margin_height) ||
        NE(matrix.cell_shadow_thickness) ||
        NE(matrix.cell_highlight_thickness)) {
        XtVaSetValues(TextChild(new),
                      XmNmarginWidth,   new->matrix.cell_margin_width,
                      XmNmarginHeight,  new->matrix.cell_margin_height,
                      XmNshadowThickness, new->matrix.cell_shadow_thickness,
                      XmNhighlightThickness,
                                new->matrix.cell_highlight_thickness,
                      NULL);
    }

    /*
     * If anything changed which affects text offsets, recalc them
     */
    if (NE(matrix.font->fid) || NE(matrix.cell_margin_width) ||
        NE(matrix.cell_margin_height) ||
        NE(matrix.cell_shadow_thickness) ||
        NE(matrix.cell_highlight_thickness)) {

        new->matrix.text_baseline =
            XmTextFieldGetBaseline(TextChild(new));
        relayout = True;
    }

    /*
     * If anything changed to affect cell total width or column positions,
     * recalc them
     */
    if (new_cells || NE(matrix.font->fid) ||
        NE(matrix.cell_margin_width) ||
        NE(matrix.cell_margin_height) ||
        NE(matrix.cell_shadow_thickness) ||
        NE(matrix.cell_highlight_thickness) ||
        NE(matrix.fixed_columns) ||
        new_column_widths) {

        GetCellTotalWidth(new);

        /*
         * Reset the HSB maximum.  sliderSize will be reset later in Resize.
         */
        XtVaSetValues(HorizScrollChild(new),
                      XmNmaximum,       CELL_TOTAL_WIDTH(new),
                      XmNvalue,         0,
                      XmNsliderSize,    1,
                      NULL);
        HORIZ_ORIGIN(new) = 0;

        /*
         * If the number of columns changed, we need to allocate a new array.
         */
        if (NE(matrix.columns)) {
            FreeColumnPositions(current);
            new->matrix.column_positions = CreateColumnPositions(new);
        }

        /*
         * If anything but fixed_columns changed, we need to recalc
         * column positions.
         */
        if (new_cells || NE(matrix.font->fid) ||
            NE(matrix.cell_margin_width) ||
            NE(matrix.cell_margin_height) ||
            NE(matrix.cell_shadow_thickness) ||
            NE(matrix.cell_highlight_thickness) ||
            new_column_widths)
            GetColumnPositions(new);

        relayout = True;
    }

    /*
     * Install text_translations on textField
     */
    if (NE(matrix.text_translations))
        XtVaSetValues(TextChild(new),
                      XmNtranslations,  new->matrix.text_translations,
                      NULL);

    /*
     * If row_label_width was set to 0, calculate it.
     * Otherwise if it was changed, set flags.
     */
    if (new->matrix.row_label_width == 0 && new->matrix.row_labels) {
        new->matrix.row_label_width = MaxRowLabel(new);
        relayout = True;
    }
    else if (NE(matrix.row_label_width))
        relayout = True;


    /*
     * Get new cached GC if needed
     */
    if (NE(core.background_pixel) || NE(matrix.font->fid)) {
        XtReleaseGC((Widget)new, new->matrix.inverse_gc);
        GetInverseGC(new);
        redisplay = True;
    }

    /*
     * Change created GCs if needed
     */

    if (NE(manager.foreground)) {
        /*
         * We don't need to put the new foreground in draw_gc or
         * draw_clip_gc because they get a new foreground when they are used.
         */

        XSetForeground(XtDisplay(new), new->matrix.cell_top_shadow_clip_gc,
                       new->manager.foreground);
        XSetBackground(XtDisplay(new), new->matrix.cell_bottom_shadow_clip_gc,
                       new->manager.foreground);
        redisplay = True;
    }
    if (NE(core.background_pixel)) {
        XSetForeground(XtDisplay(new), new->matrix.inverse_clip_gc,
                       new->core.background_pixel);
        redisplay = True;
    }
    if (NE(manager.top_shadow_color)) {
        XSetForeground(XtDisplay(new), new->matrix.cell_top_shadow_clip_gc,
                       new->manager.top_shadow_color);
        redisplay = True;
    }
    if (NE(manager.top_shadow_pixmap)) {
        XSetTile(XtDisplay(new), new->matrix.cell_top_shadow_clip_gc,
                 new->manager.top_shadow_pixmap);
        redisplay = True;
    }
    if (NE(manager.bottom_shadow_color)) {
        XSetForeground(XtDisplay(new), new->matrix.cell_bottom_shadow_clip_gc,
                       new->manager.bottom_shadow_color);
        redisplay = True;
    }
    if (NE(manager.bottom_shadow_pixmap)) {
        XSetTile(XtDisplay(new), new->matrix.cell_bottom_shadow_clip_gc,
                 new->manager.bottom_shadow_pixmap);
        redisplay = True;
    }
    if (NE(matrix.font->fid)) {
        XSetFont(XtDisplay(new), new->matrix.draw_gc, new->matrix.font->fid);
        XSetFont(XtDisplay(new), new->matrix.draw_clip_gc,
                 new->matrix.font->fid);
        XSetFont(XtDisplay(new), new->matrix.inverse_clip_gc,
                 new->matrix.font->fid);
        redisplay = True;
    }

    /*
     * See if any other resources changed which will require a relayout
     */
    if (NE(matrix.space) || NE(matrix.cell_shadow_thickness) ||
        NE(manager.shadow_thickness))
        relayout = True;

    /*
     * If bold_labels changed, and we have labels, we must redisplay
     */
    if (NE(matrix.bold_labels) &&
        (new->matrix.row_labels || new->matrix.column_labels))
        redisplay = True;

    /*
     * Compute a new size if:
     *   visible_rows or visible_columns changed.
     *   user set our width or height to zero.
     */
    if (NE(matrix.visible_rows) || NE(matrix.visible_columns) ||
        request->core.height == 0 || request->core.width == 0)
        ComputeSize(new, request->core.width == 0, request->core.height == 0);

    /*
     * If our size didn't change, but we need to layout, call Resize.
     * If our size did change, then Xt will call our Resize method for us.
     * If our size did change, but the new size is later refused,
     *   then SetValuesAlmost will call Resize to layout.
     */
    if (EQ(core.width) && EQ(core.height) && relayout)
        Resize(new);

    /*
     * The user forced a new top_row or something changed to force
     * us to recheck the current top_row.
     */
    if (NE(matrix.top_row) || do_top_row) {
        AdjustTopRow(new);
        XtVaSetValues(VertScrollChild(new),
                      XmNvalue, VERT_ORIGIN(new),
                      NULL);
        redisplay = True;
    }

    /*
     * Cancel any current edit.
     * Force the Clip widget to redisplay.  Note: this may generate an
     * expose event for the current size of the Clip widget, and the Clip
     * widget may be sized smaller in set_values_almost.  The ClipRedisplay
     * function can handle this case.
     * XXX If the Clip widget has been/will be resized, then we don't need
     * to force a redraw.  But how can set_values_almost for XtGeometryAlmost
     * determine if Resize will really resize the Clip widget?
     */
    if (redisplay || relayout) {
        (*((XbaeMatrixWidgetClass) XtClass(new))->matrix_class.cancel_edit)
            (new, True);

        XbaeClipRedraw(ClipChild(new));
    }

    /*
     * We want to return True when we need to redisplay or relayout.
     */
    return redisplay || relayout;

#undef NE
#undef EQ
}

/* ARGSUSED */
static void
SetValuesAlmost(old, new, request, reply)
XbaeMatrixWidget old;
XbaeMatrixWidget new;
XtWidgetGeometry *request;
XtWidgetGeometry *reply;
{
    /*
     * If XtGeometryAlmost, accept compromize - Resize will take care of it
     */
    if (reply->request_mode)
        *request = *reply;

    /*
     * If XtGeometryNo, call Resize to relayout if it was a size change
     * that was denied.
     * Accept the original geometry.
     * (we need to call Resize even though the size
     * didn't change to force a relayout - set_values relies on this)
     */
    else {
        if ((request->request_mode & CWWidth ||
             request->request_mode & CWHeight))
            Resize(new);

        request->request_mode = 0;
    }
}

static void
ComputeSize(mw, compute_width, compute_height)
XbaeMatrixWidget mw;
Boolean compute_width, compute_height;
{
    unsigned long full_width = CELL_TOTAL_WIDTH(mw) + FIXED_COLUMN_WIDTH(mw) +
        ROW_LABEL_WIDTH(mw) + 2 * mw->manager.shadow_thickness;
    unsigned long full_height = CELL_TOTAL_HEIGHT(mw) + FIXED_ROW_HEIGHT(mw) +
        COLUMN_LABEL_HEIGHT(mw) + 2 * mw->manager.shadow_thickness;
    unsigned long width, height;

    /*
     * Calculate our width.
     * If visible_columns is set, then base it on that.
     * Otherwise, if the compute_width flag is set, then we are full width.
     * Otherwise we keep whatever width we are.
     */
    if (mw->matrix.visible_columns)
        width = ROW_LABEL_WIDTH(mw) + 2 * mw->manager.shadow_thickness +
            COLUMN_WIDTH(mw, mw->matrix.visible_columns - 1) +
                mw->matrix.column_positions[mw->matrix.fixed_columns +
                                            mw->matrix.visible_columns - 1];
    else if (compute_width)
        width = full_width;
    else
        width = mw->core.width;

    /*
     * Calculate our height.
     * If visible_rows is set, then base it on that.
     * Otherwise, if the compute_height flag is set, then we are full height.
     * Otherwise we keep whatever height we are.
     */
    if (mw->matrix.visible_rows)
        height = mw->matrix.visible_rows * ROW_HEIGHT(mw) +
            FIXED_ROW_HEIGHT(mw) + COLUMN_LABEL_HEIGHT(mw) +
                2 * mw->manager.shadow_thickness;
    else if (compute_height)
        height = full_height;
    else
        height = mw->core.height;

    /*
     * Store our calculated size.
     */
    mw->core.width = width;
    mw->core.height = height;

    /*
     * If we are less than full width, then we need an HSB, so increment
     * our height by the size of the HSB (if we are allowed to modify our
     * height).
     */
    if (width < full_width)
        if (compute_height || mw->matrix.visible_rows)
            mw->core.height += HSCROLL_HEIGHT(mw);

    /*
     * If we are less than full height, then we need a VSB, so increment
     * our width by the size of the VSB (if we are allowed to modify our
     * width).
     */
    if (height < full_height)
        if (compute_width || mw->matrix.visible_columns)
            mw->core.width += VSCROLL_WIDTH(mw);

    /*
     * Save our calculated size for use in our query_geometry method.
     * This is the size we really want to be (not necessarily the size
     * we will end up being).
     */
    mw->matrix.desired_width = mw->core.width;
    mw->matrix.desired_height = mw->core.height;
}


static void
Destroy(mw)
XbaeMatrixWidget mw;
{
    XtReleaseGC((Widget)mw, mw->matrix.inverse_gc);

    XFreeGC(XtDisplay(mw), mw->matrix.draw_gc);
    XFreeGC(XtDisplay(mw), mw->matrix.draw_clip_gc);
    XFreeGC(XtDisplay(mw), mw->matrix.inverse_clip_gc);
    XFreeGC(XtDisplay(mw), mw->matrix.cell_top_shadow_clip_gc);
    XFreeGC(XtDisplay(mw), mw->matrix.cell_bottom_shadow_clip_gc);

    FreeCells(mw);
    FreeRowLabels(mw);
    FreeColumnLabels(mw);
    FreeColumnWidths(mw);
    FreeColumnMaxLengths(mw);
    FreeColumnPositions(mw);
    FreeColumnAlignments(mw);
    FreeColumnLabelAlignments(mw);
    FreeColors(mw);
    FreeSelectedCells(mw);

    XmFontListFree(mw->matrix.font_list);

    SmDestroyScrollMgr(mw->matrix.matrix_scroll_mgr);
    SmDestroyScrollMgr(mw->matrix.clip_scroll_mgr);
}


/*
 * Position and size the scrollbars and clip widget for our new size.
 */
static void
Resize(mw)
XbaeMatrixWidget mw;
{
    int cell_width, cell_height, rows_visible;
    Boolean has_horiz, has_vert;
    int width = mw->core.width;
    int height = mw->core.height;

    /*
     * Full size of widget (no SBs needed) - may be very large
     */
    long int full_width = CELL_TOTAL_WIDTH(mw) + FIXED_COLUMN_WIDTH(mw) +
        ROW_LABEL_WIDTH(mw) + 2 * mw->manager.shadow_thickness;
    long int full_height = CELL_TOTAL_HEIGHT(mw) + FIXED_ROW_HEIGHT(mw) +
        COLUMN_LABEL_HEIGHT(mw) + 2 * mw->manager.shadow_thickness;

    /*
     * Portion of cells which are visible in clip widget
     */
    int horiz_visible = CELL_TOTAL_WIDTH(mw) - HORIZ_ORIGIN(mw);
    int vert_visible = CELL_TOTAL_HEIGHT(mw) -
        VERT_ORIGIN(mw) * ROW_HEIGHT(mw);


    /*
     * If matrix is bigger in both dimensions, then we don't need either
     * SB. If both dimensions are smaller than matrix, then we need both
     * SBs. Otherwise, only one or the other dimension must be too small:
     *  - Get the dimensions assuming both SBs are mapped (subtract size
     *    of SBs)
     *  - Flag both SBs as mapped
     *  - If the full_width is small enough that we won't need a HSB even
     *    if the VSB is mapped, then unflag the HSB and reset the height
     *  - Next compare the full_height to the height just computed and decide
     *    if we need a VSB (if we didn't need a HSB above, then we had reset
     *    the height to not count the HSB)
     *
     * The whole point is, if we map one SB then we may need to map the
     * other one because of the space taken by the first one
     */
    if (mw->core.width >= full_width && mw->core.height >= full_height)
        has_horiz = has_vert = False;
    else if (mw->core.width < full_width && mw->core.height < full_height)
        has_horiz = has_vert = True;
    else {
        height -= HSCROLL_HEIGHT(mw);
        width -= VSCROLL_WIDTH(mw);

        has_horiz = has_vert = True;
        if (full_width <= width) {
            height = mw->core.height;
            has_horiz = False;
        }
        if (full_height <= height)
            has_vert = False;
    }


    /*
     * If widget is smaller than full size, move/resize the scrollbar and
     * set sliderSize, also if cell_width/cell_height is greater than
     * the amount of cell area visible, then we need to drag the cells
     * back into the visible part of the clip widget and set the
     * scrollbar value.
     *
     * Otherwise, the widget is larger than full size, so set
     * cell_width/cell_height to size of cells and set origin to 0
     * to force full cell area to be displayed
     *
     * We also need to move the textField correspondingly
     */

    /*
     * We were resized smaller than our max width.
     */
    if (width < full_width) {

        /*
         * Calculate the width of the non-fixed visible cells.
         */
        cell_width = mw->core.width -
            (FIXED_COLUMN_WIDTH(mw) + ROW_LABEL_WIDTH(mw) +
             2 * mw->manager.shadow_thickness);

        /*
         * Subtract the VSB if we have one.
         */
        if (has_vert)
            cell_width -= VSCROLL_WIDTH(mw);

        if (cell_width <= 0)
            cell_width = 1;

        /*
         * If the window is not full height, then place the HSB at the edge
         * of the window.  Is the window is larger than full height, then
         * place the HSB immediately below the cell region.
         */
        XtConfigureWidget(HorizScrollChild(mw),
                          FIXED_COLUMN_LABEL_OFFSET(mw),
                          height < full_height
                          ? (Position)(mw->core.height -
                                       (HorizScrollChild(mw)->core.height +
                                        2 * HorizScrollChild(mw)->
                                                core.border_width))
                          : (Position) (full_height + mw->matrix.space),
                          cell_width,
                          HorizScrollChild(mw)->core.height,
                          HorizScrollChild(mw)->core.border_width);

        /*
         * If the cells are scrolled off to the left, then drag them
         * back onto the screen.
         */
        if (cell_width > horiz_visible) {
            HORIZ_ORIGIN(mw) -= (cell_width - horiz_visible);

            if (XtIsManaged(TextChild(mw)))
                XtMoveWidget(TextChild(mw),
                             TextChild(mw)->core.x +
                                (cell_width - horiz_visible),
                             TextChild(mw)->core.y);
        }

        /*
         * Setup the HSB to reflect our new size.
         */
        XtVaSetValues(HorizScrollChild(mw),
                      XmNpageIncrement, cell_width,
                      XmNsliderSize,    cell_width,
                      XmNvalue,         HORIZ_ORIGIN(mw),
                      NULL);
    }

    /*
     * We were resized larger than the our max width.  Drag the cells back
     * onto the screen if they were scrolled off to the left.
     */
    else {
        if (XtIsManaged(TextChild(mw)))
            XtMoveWidget(TextChild(mw),
                         TextChild(mw)->core.x + HORIZ_ORIGIN(mw),
                         TextChild(mw)->core.y);

        cell_width = CELL_TOTAL_WIDTH(mw);
        HORIZ_ORIGIN(mw) = 0;
    }

    /*
     * We were resized smaller than our max height.
     */
    if (height < full_height) {

        /*
         * Calculate the height of the non-fixed visible cells.
         */
        cell_height = mw->core.height -
            (FIXED_ROW_HEIGHT(mw) + COLUMN_LABEL_HEIGHT(mw) +
             2 * mw->manager.shadow_thickness);

        /*
         * Subtract the HSB if we have one.
         */
        if (has_horiz)
            cell_height -= HSCROLL_HEIGHT(mw);

        if (cell_height <= 0)
            cell_height = 1;

        /*
         * If the window is not full width, then place the VSB at the edge
         * of the window.  Is the window is larger than full width, then
         * place the VSB immediately to the right of the cell region.
         */
        XtConfigureWidget(VertScrollChild(mw),
                          width < full_width
                          ? (Position)(mw->core.width -
                                       (VertScrollChild(mw)->core.width +
                                        2 * VertScrollChild(mw)->
                                                core.border_width))
                          : (Position) full_width + mw->matrix.space,
                          FIXED_ROW_LABEL_OFFSET(mw),
                          VertScrollChild(mw)->core.width,
                          cell_height,
                          VertScrollChild(mw)->core.border_width);

        /*
         * If the cells are scrolled off the top, then drag them
         * back onto the screen.
         */
        if (cell_height > vert_visible) {
            int rows = (cell_height - vert_visible) / ROW_HEIGHT(mw);

            VERT_ORIGIN(mw) -= rows;

            if (XtIsManaged(TextChild(mw)))
                XtMoveWidget(TextChild(mw),
                             TextChild(mw)->core.x,
                             TextChild(mw)->core.y + rows * ROW_HEIGHT(mw));
        }

        /*
         * Setup the VSB to reflect our new size.
         */
        rows_visible = cell_height / ROW_HEIGHT(mw);
        XtVaSetValues(VertScrollChild(mw),
                      XmNpageIncrement, rows_visible <= 0 ? 1 : rows_visible,
                      XmNsliderSize,    rows_visible <= 0 ? 1 : rows_visible,
                      XmNvalue,         VERT_ORIGIN(mw),
                      NULL);
    }

    /*
     * We were resized larger than the our max height.  Drag the cells back
     * onto the screen if they were scrolled off the top.
     */
    else {
        if (XtIsManaged(TextChild(mw)))
            XtMoveWidget(TextChild(mw),
                         TextChild(mw)->core.x,
                         TextChild(mw)->core.y +
                             VERT_ORIGIN(mw) * ROW_HEIGHT(mw));

        cell_height = CELL_TOTAL_HEIGHT(mw);
        rows_visible = mw->matrix.rows - mw->matrix.fixed_rows;
        VERT_ORIGIN(mw) = 0;
    }

    /*
     * Map/unmap scrollbars based on flags set above
     */
    if (has_horiz)
        XtManageChild(HorizScrollChild(mw));
    else
        XtUnmanageChild(HorizScrollChild(mw));

    if (has_vert)
        XtManageChild(VertScrollChild(mw));
    else
        XtUnmanageChild(VertScrollChild(mw));

    /*
     * Now that we have cell_width & cell_height,
     * make the clip widget this size.  Height is truncated to the
     * nearest row.
     */
    XtConfigureWidget(ClipChild(mw),
                      FIXED_COLUMN_LABEL_OFFSET(mw),
                      FIXED_ROW_LABEL_OFFSET(mw),
                      cell_width,
                      rows_visible <= 0
                        ? cell_height
                        : rows_visible * ROW_HEIGHT(mw),
                      0);

    /*
     * Save the non-truncated height.  We need this so we can draw
     * the shadow correctly.
     */
    mw->matrix.cell_visible_height = cell_height;

    /*
     * Set the clip_mask in our clipping GCs.  This function relies on
     * the Clip widget being the correct size (above).
     */
    SetClipMask(mw);
}

/*
 * Since we totally control our childrens geometry, allow anything.
 */
/* ARGSUSED */
static XtGeometryResult
GeometryManager(w, desired, allowed)
Widget w;
XtWidgetGeometry *desired, *allowed;
{
#define Wants(flag) (desired->request_mode & flag)

    if (Wants(XtCWQueryOnly))
        return(XtGeometryYes);

    if (Wants(CWWidth))
        w->core.width = desired->width;
    if (Wants(CWHeight))
        w->core.height = desired->height;
    if (Wants(CWX))
        w->core.x = desired->x;
    if (Wants(CWY))
        w->core.y = desired->y;
    if (Wants(CWBorderWidth))
        w->core.border_width = desired->border_width;

    return(XtGeometryYes);

#undef Wants
}

/*
 * We would prefer to be the size calculated in ComputeSize and saved in
 * desired_width/height
 */
static XtGeometryResult
QueryGeometry(mw, proposed, desired)
XbaeMatrixWidget mw;
XtWidgetGeometry *proposed, *desired;
{
#define Set(bit) (proposed->request_mode & bit)

    desired->width = mw->matrix.desired_width;
    desired->height = mw->matrix.desired_height;
    desired->request_mode = CWWidth | CWHeight;

    if (Set(CWWidth) && proposed->width == desired->width &&
        Set(CWHeight) && proposed->height == desired->height)
        return(XtGeometryYes);

    if (desired->width == mw->core.width && desired->height == mw->core.height)
        return(XtGeometryNo);

    return(XtGeometryAlmost);

#undef Set
}

/*
 * Callback for vertical scrollbar
 */
/* ARGSUSED */
static void
ScrollVertCB(w, client_data, call_data)
Widget w;
XtPointer client_data;
XmScrollBarCallbackStruct *call_data;
{
    XbaeMatrixWidget mw = (XbaeMatrixWidget)XtParent(w);
    int src_y, dest_y, height;

    /*
     * Didn't scroll
     */
    if (call_data->value == VERT_ORIGIN(mw))
        return;

    /*
     * Scrolled forward. We want to copy a chunk starting at src_y up
     * to the top (dest_y=0)
     */
    else if (call_data->value > VERT_ORIGIN(mw)) {
        dest_y = 0;
        src_y = (call_data->value - VERT_ORIGIN(mw)) * ROW_HEIGHT(mw);
        height = ClipChild(mw)->core.height - src_y;
    }

    /*
     * Scrolled backward. We want to copy a chunk starting at the top
     * (src_y=0) down to dest_y.
     */
    else {
        dest_y = (VERT_ORIGIN(mw) - call_data->value) * ROW_HEIGHT(mw);
        src_y = 0;
        height = ClipChild(mw)->core.height - dest_y;
    }

    /*
     * The textField needs to scroll along with the cells.
     */
    if (XtIsManaged(TextChild(mw)))
        XtMoveWidget(TextChild(mw),
                     TextChild(mw)->core.x,
                     TextChild(mw)->core.y +
                     (VERT_ORIGIN(mw) - call_data->value) * ROW_HEIGHT(mw));

    /*
     * Now we can adjust our vertical origin
     */
    VERT_ORIGIN(mw) = call_data->value;

    if (!XtIsRealized(mw))
        return;

    /*
     * If we scrolled more than a screenful, just clear and
     * redraw the whole thing
     */
    if (height <= 0) {
        Rectangle rect;

        /*
         * Clear the whole clip window.
         */
        XClearArea(XtDisplay(mw), XtWindow(ClipChild(mw)),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   False);

        /*
         * Redraw all the non-fixed cells in the clip window
         */
        SETRECT(rect,
                0, 0,
                ClipChild(mw)->core.width - 1,
                ClipChild(mw)->core.height - 1);
        RedrawCells(mw, &rect);

        /*
         * Clear the non-fixed row labels and the cells in fixed columns
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   0, FIXED_ROW_LABEL_OFFSET(mw),
                   FIXED_COLUMN_LABEL_OFFSET(mw), 0 /*Full Height*/,
                   False);

        /*
         * Redraw non-fixed row labels and cells in fixed columns
         */
        SETRECT(rect,
                0, FIXED_ROW_LABEL_OFFSET(mw),
                FIXED_COLUMN_LABEL_OFFSET(mw), mw->core.height);
        RedrawLabelsAndFixed(mw, &rect);
    }

    /*
     * If we scrolled less than a screenful, we want to copy as many
     * pixels as we can and then clear and redraw the newly scrolled data.
     */
    else {
        Rectangle rect;
        int y_clear = src_y > dest_y ? height : 0;

        /*
         * Queue this scroll with the ScrollMgr
         */
        SmAddScroll(mw->matrix.clip_scroll_mgr, 0, dest_y - src_y);

        /*
         * Copy the non-fixed cells in the clip widget
         */
        XCopyArea(XtDisplay(mw),
                  XtWindow(ClipChild(mw)), XtWindow(ClipChild(mw)),
                  mw->matrix.draw_gc,
                  0, src_y,
                  ClipChild(mw)->core.width, height,
                  0, dest_y);

        /*
         * Clear the newly scrolled chunk of the clip widget
         */
        XClearArea(XtDisplay(mw), XtWindow(ClipChild(mw)),
                   0, y_clear,
                   0 /*Full Width*/, ClipChild(mw)->core.height - height,
                   False);

        /*
         * Redraw the non-fixed cells into the new chunk
         */
        SETRECT(rect,
                0, y_clear,
                ClipChild(mw)->core.width - 1,
                (y_clear + (ClipChild(mw)->core.height - height)) - 1);
        RedrawCells(mw, &rect);

        /*
         * Translate coordinates for fixed columns and row labels.
         */
        src_y += FIXED_ROW_LABEL_OFFSET(mw);
        dest_y += FIXED_ROW_LABEL_OFFSET(mw);
        y_clear += FIXED_ROW_LABEL_OFFSET(mw);

        /*
         * Queue this scroll with the ScrollMgr
         */
        SmAddScroll(mw->matrix.matrix_scroll_mgr, 0, dest_y - src_y);

        /*
         * Copy the fixed columns and row labels
         */
        XCopyArea(XtDisplay(mw),
                  XtWindow(mw), XtWindow(mw),
                  mw->matrix.draw_gc,
                  0, src_y,
                  FIXED_COLUMN_LABEL_OFFSET(mw), height,
                  0, dest_y);

        /*
         * Clear the newly scrolled chunk of fixed columns and row labels
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   0, y_clear,
                   FIXED_COLUMN_LABEL_OFFSET(mw),
                   ClipChild(mw)->core.height - height,
                   False);

        /*
         * Redraw the new chunk of fixed columns and row labels
         */
        SETRECT(rect,
                0, y_clear,
                FIXED_COLUMN_LABEL_OFFSET(mw) - 1,
                (y_clear + (ClipChild(mw)->core.height - height)) - 1);
        RedrawLabelsAndFixed(mw, &rect);
    }
}

/*
 * Callback for horizontal scrollbar
 */
/* ARGSUSED */
static void
ScrollHorizCB(w, client_data, call_data)
Widget w;
XtPointer client_data;
XmScrollBarCallbackStruct *call_data;
{
    XbaeMatrixWidget mw = (XbaeMatrixWidget)XtParent(w);
    int src_x, dest_x, width;

    /*
     * Didn't scroll
     */
    if (call_data->value == HORIZ_ORIGIN(mw))
        return;

    /*
     * Scrolled right. We want to copy a chunk starting at src_x over to
     * the left (dest_x=0)
     */
    else if (call_data->value > HORIZ_ORIGIN(mw)) {
        dest_x = 0;
        src_x = call_data->value - HORIZ_ORIGIN(mw);
        width = ClipChild(mw)->core.width - src_x;
    }

    /*
     * Scrolled left. We want to copy a chunk starting at the left (src_x=0)
     * over to the right to dest_x
     */
    else {
        dest_x = HORIZ_ORIGIN(mw) - call_data->value;
        src_x = 0;
        width = ClipChild(mw)->core.width - dest_x;
    }

    /*
     * The textField needs to scroll along with the cells.
     */
    if (XtIsManaged(TextChild(mw)))
        XtMoveWidget(TextChild(mw),
                     TextChild(mw)->core.x + (HORIZ_ORIGIN(mw) -
                                              call_data->value),
                     TextChild(mw)->core.y);

    /*
     * Now we can adjust our horizontal origin
     */
    HORIZ_ORIGIN(mw) = call_data->value;

    if (!XtIsRealized(mw))
        return;

    /*
     * If we scrolled more than a screenful, just clear and
     * redraw the whole thing
     */
    if (width <= 0) {
        Rectangle rect;

        /*
         * Clear the whole clip window
         */
        XClearArea(XtDisplay(mw), XtWindow(ClipChild(mw)),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   False);

        /*
         * Redraw all the non-fixed cells in the clip window
         */
        SETRECT(rect,
                0, 0,
                ClipChild(mw)->core.width - 1,
                ClipChild(mw)->core.height - 1);
        RedrawCells(mw, &rect);

        /*
         * Clear the non-fixed column labels and the cells in fixed rows
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   FIXED_COLUMN_LABEL_OFFSET(mw), 0,
                   0 /*Full Width*/, FIXED_ROW_LABEL_OFFSET(mw), False);

        /*
         * Redraw non-fixed column labels and cells in fixed rows
         */
        SETRECT(rect,
                FIXED_COLUMN_LABEL_OFFSET(mw), 0,
                mw->core.width, FIXED_ROW_LABEL_OFFSET(mw));
        RedrawLabelsAndFixed(mw, &rect);
    }

    /*
     * If we scrolled less than a screenful, we want to copy as many
     * pixels as we can and then clear and redraw the newly scrolled data.
     */
    else {
        Rectangle rect;
        int x_clear = src_x > dest_x ? width : 0;

        /*
         * Queue this scroll with the ScrollMgr
         */
        SmAddScroll(mw->matrix.clip_scroll_mgr, dest_x - src_x, 0);

        /*
         * Copy the non-fixed cells in the clip widget
         */
        XCopyArea(XtDisplay(mw),
                  XtWindow(ClipChild(mw)), XtWindow(ClipChild(mw)),
                  mw->matrix.draw_gc,
                  src_x, 0,
                  width, ClipChild(mw)->core.height,
                  dest_x, 0);

        /*
         * Clear the newly scrolled chunk of the clip widget
         */
        XClearArea(XtDisplay(mw), XtWindow(ClipChild(mw)),
                   x_clear, 0,
                   ClipChild(mw)->core.width - width, 0 /*Full Height*/,
                   False);

        /*
         * Redraw the non-fixed cells into the new chunk
         */
        SETRECT(rect,
                x_clear, 0,
                (x_clear + (ClipChild(mw)->core.width - width)) - 1,
                ClipChild(mw)->core.height - 1);
        RedrawCells(mw, &rect);

        /*
         * Translate coordinates for fixed rows and column labels.
         */
        src_x += FIXED_COLUMN_LABEL_OFFSET(mw);
        dest_x += FIXED_COLUMN_LABEL_OFFSET(mw);
        x_clear += FIXED_COLUMN_LABEL_OFFSET(mw);

        /*
         * Queue this scroll with the ScrollMgr
         */
        SmAddScroll(mw->matrix.matrix_scroll_mgr, dest_x - src_x, 0);

        /*
         * Copy the fixed rows and column labels
         */
        XCopyArea(XtDisplay(mw),
                  XtWindow(mw), XtWindow(mw),
                  mw->matrix.draw_gc,
                  src_x, 0,
                  width, FIXED_ROW_LABEL_OFFSET(mw),
                  dest_x, 0);

        /*
         * Clear the newly scrolled chunk of fixed rows and column labels
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   x_clear, 0,
                   ClipChild(mw)->core.width - width,
                   FIXED_ROW_LABEL_OFFSET(mw),
                   False);

        /*
         * Redraw the new chunk of fixed rows and column labels
         */
        SETRECT(rect,
                x_clear, 0,
                (x_clear + (ClipChild(mw)->core.width - width)) - 1,
                FIXED_ROW_LABEL_OFFSET(mw) - 1);
        RedrawLabelsAndFixed(mw, &rect);
    }
}

/*
 * This is the modifyVerifyCallback we added to textField. We need to
 * call Matrixs modifyVerifyCallback list with the textField info
 * and the row/col that is changing.
 */
/* ARGSUSED */
static void
ModifyVerifyCB(w, mw, verify)
Widget w;
XbaeMatrixWidget mw;
XmTextVerifyCallbackStruct *verify;
{
    XbaeMatrixModifyVerifyCallbackStruct call_data;

    if (mw->matrix.modify_verify_callback == NULL)
        return;

    call_data.reason = XbaeModifyVerifyReason;
    call_data.row = mw->matrix.current_row;
    call_data.column = mw->matrix.current_column;
    call_data.verify = verify;

    XtCallCallbackList((Widget)mw, mw->matrix.modify_verify_callback,
                       (XtPointer) &call_data);
}

/*
 * This is the Clip widgets focusCallback. We want to give the focus to
 * the textField if a cell is being edited.  If no cells are being edited,
 * force an edit on the top left most visible cell.
 */
/* ARGSUSED */
static void
TraverseInCB(w, mw, call_data)
Widget w;
XbaeMatrixWidget mw;
XtPointer call_data;
{
    /*
     * If the traversing flag is set, then Clip got the focus because
     * textField was trying to traverse out of mw.  We'll help it along.
     * Sickening.
     */
    if (mw->matrix.traversing != NOT_TRAVERSING) {
        XmProcessTraversal(w, mw->matrix.traversing);
        return;
    }

    /*
     * If the textField is managed and not visible, scroll it onto the screen
     * and traverse to it.
     */
    if (XtIsManaged(TextChild(mw))) {
        if (!IsCellVisible(mw,
                           mw->matrix.current_row, mw->matrix.current_column))
            MakeCellVisible(mw,
                            mw->matrix.current_row, mw->matrix.current_column);
        XmProcessTraversal(TextChild(mw), XmTRAVERSE_CURRENT);
    }

    /*
     * Otherwise, no cell is being edited.  Force an edit on the top-left
     * most visible cell.
     */
    else {
        int column = XtoCol(mw, FIXED_COLUMN_WIDTH(mw) +  HORIZ_ORIGIN(mw));
        int row = VERT_ORIGIN(mw) + mw->matrix.fixed_rows;

        /*
         * Call the traverseCellCallback to allow the application to
         * perform custom traversal.
         */
        if (mw->matrix.traverse_cell_callback) {
            XbaeMatrixTraverseCellCallbackStruct call_data;

            call_data.reason = XbaeTraverseCellReason;
            call_data.row = 0;
            call_data.column = 0;
            call_data.next_row = row;
            call_data.next_column = column;
            call_data.fixed_rows = mw->matrix.fixed_rows;
            call_data.fixed_columns = mw->matrix.fixed_columns;
            call_data.num_rows = mw->matrix.rows;
            call_data.num_columns = mw->matrix.columns;
            call_data.param = NULL;
            call_data.qparam = NULLQUARK;

            XtCallCallbackList((Widget)mw, mw->matrix.traverse_cell_callback,
                               (XtPointer) &call_data);

            row = call_data.next_row;
            column = call_data.next_column;
        }

        (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.edit_cell)
            (mw, row, column);

        XmProcessTraversal(TextChild(mw), XmTRAVERSE_CURRENT);
    }
}

static void
CopyCells(mw)
XbaeMatrixWidget mw;
{
    String **copy;
    int i, j;

    /*
     * Malloc an array of row pointers
     */
    copy = (String **) XtMalloc(mw->matrix.rows * sizeof(String *));

    /*
     * Malloc an array of Strings for each row pointer
     */
    for (i = 0; i < mw->matrix.rows; i++)
        copy[i] = (String *) XtMalloc(mw->matrix.columns * sizeof(String));

    /*
     * Create a bunch of "" cells if cells was NULL
     */
    if (!mw->matrix.cells) {
        for (i = 0; i < mw->matrix.rows; i++)
            for (j = 0; j < mw->matrix.columns; j++)
                copy[i][j] = XtNewString("");
    }

    /*
     * Otherwise copy the table passed in
     */
    else {
        for (i = 0; i < mw->matrix.rows; i++)
            for (j = 0; j < mw->matrix.columns; j++) {
                if (!mw->matrix.cells[i][j]) {
                    XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                                    "copyCells", "badValue", "XbaeMatrix",
                                  "XbaeMatrix: NULL entry found in cell table",
                                    NULL, 0);
                    copy[i][j] = XtNewString("");
                }
                else
                    copy[i][j] = XtNewString(mw->matrix.cells[i][j]);
            }
    }

    mw->matrix.cells = copy;
}

static void
CopyRowLabels(mw)
XbaeMatrixWidget mw;
{
    String *copy;
    int i;

    copy = (String *) XtMalloc(mw->matrix.rows * sizeof(String));

    for (i = 0; i < mw->matrix.rows; i++)
        if (!mw->matrix.row_labels[i]) {
            XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                            "copyRowLabels", "badValue", "XbaeMatrix",
                            "XbaeMatrix: NULL entry found in rowLabels array",
                            NULL, 0);
            copy[i] = XtNewString("");
        }
        else
            copy[i] = XtNewString(mw->matrix.row_labels[i]);

    mw->matrix.row_labels = copy;
}

static void
CopyColumnLabels(mw)
XbaeMatrixWidget mw;
{
    String *copy;
    int i;

    copy = (String *) XtMalloc(mw->matrix.columns * sizeof(String));

    mw->matrix.column_label_lines = (ColumnLabelLines)
        XtMalloc(mw->matrix.columns * sizeof(ColumnLabelLinesRec));

    for (i = 0; i < mw->matrix.columns; i++)
        if (!mw->matrix.column_labels[i]) {
            XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                            "copyColumnLabels", "badValue", "XbaeMatrix",
                          "XbaeMatrix: NULL entry found in columnLabels array",
                            NULL, 0);
            copy[i] = XtNewString("");
            ParseColumnLabel(copy[i], &mw->matrix.column_label_lines[i]);
        }
        else {
            copy[i] = XtNewString(mw->matrix.column_labels[i]);
            ParseColumnLabel(mw->matrix.column_labels[i],
                             &mw->matrix.column_label_lines[i]);
        }

    /*
     * Determine max number of lines in column labels
     */
    mw->matrix.column_label_maxlines = mw->matrix.column_label_lines[0].lines;
    for (i = 1; i < mw->matrix.columns; i++)
        if (mw->matrix.column_label_lines[i].lines >
            mw->matrix.column_label_maxlines)
            mw->matrix.column_label_maxlines =
                mw->matrix.column_label_lines[i].lines;

    mw->matrix.column_labels = copy;
}

static void
CopyColumnWidths(mw)
XbaeMatrixWidget mw;
{
    short *copy;
    int i;
    Boolean bad = False;

    copy = (short *) XtMalloc(mw->matrix.columns * sizeof(short));

    for (i = 0; i < mw->matrix.columns; i++) {
        if (!bad && mw->matrix.column_widths[i] == BAD_WIDTH) {
            bad = True;
            XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                            "copyColumnWidths", "tooShort", "XbaeMatrix",
                            "XbaeMatrix: Column widths array is too short",
                            NULL, 0);
            copy[i] = 1;
        }
        else if (bad)
            copy[i] = 1;
        else
            copy[i] = mw->matrix.column_widths[i];
    }

    mw->matrix.column_widths = copy;
}

static void
CopyColumnMaxLengths(mw)
XbaeMatrixWidget mw;
{
    int *copy;
    int i;
    Boolean bad = False;

    copy = (int *) XtMalloc(mw->matrix.columns * sizeof(int));

    for (i = 0; i < mw->matrix.columns; i++) {
        if (!bad && mw->matrix.column_max_lengths[i] == BAD_MAXLENGTH) {
            bad = True;
            XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                            "copyColumnMaxLengths", "tooShort", "XbaeMatrix",
                            "XbaeMatrix: Column max lengths array is too short",
                            NULL, 0);
            copy[i] = 1;
        }
        else if (bad)
            copy[i] = 1;
        else
            copy[i] = mw->matrix.column_max_lengths[i];
    }

    mw->matrix.column_max_lengths = copy;
}

static void
CopyColumnAlignments(mw)
XbaeMatrixWidget mw;
{
    unsigned char *copy;
    int i;
    Boolean bad = False;

    copy = (unsigned char *) XtMalloc(mw->matrix.columns *
                                      sizeof(unsigned char));

    for (i = 0; i < mw->matrix.columns; i++) {
        if (!bad && mw->matrix.column_alignments[i] == BAD_ALIGNMENT) {
            bad = True;
            XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                            "copyColumnAlignments", "tooShort", "XbaeMatrix",
                            "XbaeMatrix: Column alignments array is too short",
                            NULL, 0);
            copy[i] = XmALIGNMENT_BEGINNING;
        }
        else if (bad)
            copy[i] = XmALIGNMENT_BEGINNING;
        else
            copy[i] = mw->matrix.column_alignments[i];
    }

    mw->matrix.column_alignments = copy;
}

static void
CopyColumnLabelAlignments(mw)
XbaeMatrixWidget mw;
{
    unsigned char *copy;
    int i;
    Boolean bad = False;

    copy = (unsigned char *) XtMalloc(mw->matrix.columns *
                                      sizeof(unsigned char));

    for (i = 0; i < mw->matrix.columns; i++) {
        if (!bad &&
            mw->matrix.column_label_alignments[i] == BAD_ALIGNMENT) {
            bad = True;
            XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                            "copyColumnLabelAlignments", "tooShort",
                            "XbaeMatrix",
                     "XbaeMatrix: Column label alignments array is too short",
                            NULL, 0);
            copy[i] = XmALIGNMENT_BEGINNING;
        }
        else if (bad)
            copy[i] = XmALIGNMENT_BEGINNING;
        else
            copy[i] = mw->matrix.column_label_alignments[i];
    }

    mw->matrix.column_label_alignments = copy;
}

static void
CopyColors(mw)
XbaeMatrixWidget mw;
{
    Pixel **copy;
    int i, j;

    /*
     * Malloc an array of row pointers
     */
    copy = (Pixel **) XtMalloc(mw->matrix.rows * sizeof(Pixel *));

    /*
     * Malloc an array of Pixels for each row pointer
     */
    for (i = 0; i < mw->matrix.rows; i++)
        copy[i] = (Pixel *) XtMalloc(mw->matrix.columns * sizeof(Pixel));

    for (i = 0; i < mw->matrix.rows; i++)
        for (j = 0; j < mw->matrix.columns; j++)
            copy[i][j] = mw->matrix.colors[i][j];

    mw->matrix.colors = copy;
}

/*
 * Copy the selectedCells resource. Create a 2D array of Booleans to
 * represent selected cells if it is NULL.
 */
static void
CopySelectedCells(mw)
XbaeMatrixWidget mw;
{
    Boolean **copy;
    int i, j;

    /*
     * Malloc an array of row pointers
     */
     copy = (Boolean **) XtMalloc(mw->matrix.rows * sizeof(Boolean *));

    /*
     * Malloc an array of Booleans for each row pointer
     */
    for (i = 0; i < mw->matrix.rows; i++)
        copy[i] = (Boolean *) XtCalloc(mw->matrix.columns, sizeof(Boolean));

    /*
     * If selected_cells is not NULL, copy the table passed in
     */
    if (mw->matrix.selected_cells)
        for (i = 0; i < mw->matrix.rows; i++)
            for (j = 0; j < mw->matrix.columns; j++)
                copy[i][j] = mw->matrix.selected_cells[i][j];

    mw->matrix.selected_cells = copy;
}

static void
ParseColumnLabel(label, lines)
String label;
ColumnLabelLines lines;
{
    char *nl;

    /*
     * First count the number of lines in the label
     */
    lines->lines = 1;
    nl = label;
    while ((nl = strchr(nl, '\n')) != NULL) {
        nl++;
        lines->lines++;
    }

    /*
     * Now malloc a lengths array of the correct size.
     */
    lines->lengths = (int *) XtMalloc(lines->lines * sizeof(int));

    /*
     * An entry in the lengths array is the length of that line (substring).
     */

    /*
     * Handle the case of one line (no strchr() needed)
     */
    if (lines->lines == 1)
        lines->lengths[0] = strlen(label);
    else {
        int i;

        nl = label;
        i = 0;
        while ((nl = strchr(nl, '\n')) != NULL) {
            lines->lengths[i] = nl - label;
            nl++;
            label = nl;
            i++;
        }
        lines->lengths[i] = strlen(label);
    }
}

static void
FreeCells(mw)
XbaeMatrixWidget mw;
{
    int i, j;

    if (!mw->matrix.cells)
        return;

    /*
     * Free each cell in a row, then free the row and go to the next one
     */
    for (i = 0; i < mw->matrix.rows; i++) {
        for (j = 0; j < mw->matrix.columns; j++)
            XtFree((XtPointer) mw->matrix.cells[i][j]);
        XtFree((XtPointer) mw->matrix.cells[i]);
    }

    /*
     * Free the array of row pointers
     */
    XtFree((XtPointer) mw->matrix.cells);
}

static void
FreeRowLabels(mw)
XbaeMatrixWidget mw;
{
    int i;

    if (!mw->matrix.row_labels)
        return;

    for (i = 0; i < mw->matrix.rows; i++)
        XtFree((XtPointer)mw->matrix.row_labels[i]);

    XtFree((XtPointer)mw->matrix.row_labels);
}

static void
FreeColumnLabels(mw)
XbaeMatrixWidget mw;
{
    int i;

    if (!mw->matrix.column_labels)
        return;

    for (i = 0; i < mw->matrix.columns; i++) {
        XtFree((XtPointer)mw->matrix.column_labels[i]);
        XtFree((XtPointer)mw->matrix.column_label_lines[i].lengths);
    }

    XtFree((XtPointer)mw->matrix.column_label_lines);
    XtFree((XtPointer)mw->matrix.column_labels);
}


static void
FreeColors(mw)
XbaeMatrixWidget mw;
{
    int i;

    if (!mw->matrix.colors)
        return;

    /*
     * Free each row of Pixels
     */
    for (i = 0; i < mw->matrix.rows; i++)
        XtFree((XtPointer) mw->matrix.colors[i]);

    /*
     * Free the array of row pointers
     */
    XtFree((XtPointer) mw->matrix.colors);
}

static void
FreeSelectedCells(mw)
XbaeMatrixWidget mw;
{
    int i;

    /*
     * Free each row of Booleans
     */
    for (i = 0; i < mw->matrix.rows; i++)
        XtFree((XtPointer) mw->matrix.selected_cells[i]);

    /*
     * Free the array of row pointers
     */
    XtFree((XtPointer) mw->matrix.selected_cells);
}

/*
 * Create a matrix of Pixels
 */
static void
CreateColors(mw)
XbaeMatrixWidget mw;
{
    int i;

    /*
     * Malloc an array of row pointers
     */
    mw->matrix.colors = (Pixel **) XtMalloc(mw->matrix.rows * sizeof(Pixel *));

    /*
     * Malloc an array of Pixels for each row pointer
     */
    for (i = 0; i < mw->matrix.rows; i++)
        mw->matrix.colors[i] = (Pixel *) XtMalloc(mw->matrix.columns *
                                                      sizeof(Pixel));
}

/*
 * Add rows/columns of cells when set_values changes our rows/columns
 */
static void
ResizeCells(current, new)
XbaeMatrixWidget current;
XbaeMatrixWidget new;
{
    int i, j;
    int safe_rows;

    if (new->matrix.rows == current->matrix.rows)
        safe_rows = new->matrix.rows;

    /*
     * Adding rows
     */
    if (new->matrix.rows > current->matrix.rows) {
        /*
         * Realloc a larger array of row pointers
         */
        new->matrix.cells =
            (String **) XtRealloc((char *)new->matrix.cells,
                                   new->matrix.rows * sizeof(String *));

        /*
         * Malloc a new row array for each row. Initialize it with
         * NULL Strings. Use the new column size.
         */
        for (i = current->matrix.rows; i < new->matrix.rows; i++) {
            new->matrix.cells[i] =
                (String *) XtMalloc(new->matrix.columns * sizeof(String));
            for (j = 0; j < new->matrix.columns; j++)
                new->matrix.cells[i][j] = XtNewString("");
        }

        safe_rows = current->matrix.rows;
    }

    /*
     * Deleting rows
     */
    if (new->matrix.rows < current->matrix.rows) {
        /*
         * Free the cells in the rows being deleted and the rows themselves
         */
        for (i = new->matrix.rows; i < current->matrix.rows; i++) {
            for (j = 0; j < current->matrix.columns; j++)
                XtFree((XtPointer) new->matrix.cells[i][j]);
            XtFree((XtPointer) new->matrix.cells[i]);
        }

        safe_rows = new->matrix.rows;
    }

    /*
     * Adding columns
     */
    if (new->matrix.columns > current->matrix.columns) {
        /*
         * Realloc each row array. Do not touch any rows added/deleted above
         * (use safe_rows)
         */
        for (i = 0; i < safe_rows; i++) {
            new->matrix.cells[i] =
                (String *) XtRealloc((char *)new->matrix.cells[i],
                                     new->matrix.columns * sizeof(String));
            for (j = current->matrix.columns; j < new->matrix.columns; j++)
                new->matrix.cells[i][j] = XtNewString("");
        }
    }

    /*
     * Deleting columns
     */
    if (new->matrix.columns < current->matrix.columns) {
        /*
         * Free all the cells in the deleted columns. Do not touch any
         * rows added/deleted above (use safe_rows).
         * We don't bother to realloc each row, just leave some wasted space.
         * XXX is this a problem?
         */
        for (i = 0; i < safe_rows; i++)
            for (j = new->matrix.columns; j < current->matrix.columns; j++)
                XtFree((XtPointer) new->matrix.cells[i][j]);
    }
}

/*
 * Add rows/columns of selected flags when set_values changes our rows/columns
 */
static void
ResizeSelectedCells(current, new)
XbaeMatrixWidget current;
XbaeMatrixWidget new;
{
    int i;
    int safe_rows;

    if (new->matrix.rows == current->matrix.rows)
        safe_rows = new->matrix.rows;

    /*
     * Adding rows
     */
    if (new->matrix.rows > current->matrix.rows) {
        /*
         * Realloc a larger array of row pointers
         */
        new->matrix.selected_cells =
            (Boolean **) XtRealloc((char *)new->matrix.selected_cells,
                                   new->matrix.rows * sizeof(Boolean *));

        /*
         * Calloc a new row array for each row. Use the new column size.
         */
        for (i = current->matrix.rows; i < new->matrix.rows; i++)
            new->matrix.selected_cells[i] =
                (Boolean *) XtCalloc(new->matrix.columns, sizeof(Boolean));

        safe_rows = current->matrix.rows;
    }

    /*
     * Deleting rows
     */
    if (new->matrix.rows < current->matrix.rows) {
        for (i = new->matrix.rows; i < current->matrix.rows; i++)
            XtFree((XtPointer) new->matrix.selected_cells[i]);
        safe_rows = new->matrix.rows;
    }

    /*
     * Adding columns
     */
    if (new->matrix.columns > current->matrix.columns) {
        /*
         * Realloc each row array. Do not touch any rows added/deleted above
         * (use safe_rows)
         */
        for (i = 0; i < safe_rows; i++) {
            int j;

            new->matrix.selected_cells[i] =
                (Boolean *) XtRealloc((char *)new->matrix.selected_cells[i],
                                      new->matrix.columns * sizeof(Boolean));
            for (j = current->matrix.columns; j < new->matrix.columns; j++)
                new->matrix.selected_cells[i][j] = False;
        }
    }

    /*
     * Deleting columns
     *   if (new->matrix.columns < current->matrix.columns)
     * We don't bother to realloc, just leave some wasted space.
     * XXX is this a problem?
     */
}

/*
 * Add rows/columns of colors when set_values changes our rows/columns
 */
static void
ResizeColors(current, new)
XbaeMatrixWidget current;
XbaeMatrixWidget new;
{
    int i, j;
    int safe_rows;

    if (new->matrix.rows == current->matrix.rows)
        safe_rows = new->matrix.rows;

    /*
     * Adding rows
     */
    if (new->matrix.rows > current->matrix.rows) {
        /*
         * Realloc a larger array of row pointers
         */
        new->matrix.colors =
            (Pixel **) XtRealloc((char *)new->matrix.colors,
                                 new->matrix.rows * sizeof(Pixel *));

        /*
         * Malloc a new row array for each row. Initialize it with foreground.
         * Use the new column size.
         */
        for (i = current->matrix.rows; i < new->matrix.rows; i++) {
            new->matrix.colors[i] =
                (Pixel *) XtMalloc(new->matrix.columns * sizeof(Pixel));
            for (j = 0; j < new->matrix.columns; j++)
                new->matrix.colors[i][j] = new->manager.foreground;
        }

        safe_rows = current->matrix.rows;
    }

    /*
     * Deleting rows
     */
    if (new->matrix.rows < current->matrix.rows) {
        for (i = new->matrix.rows; i < current->matrix.rows; i++)
            XtFree((XtPointer) new->matrix.colors[i]);
        safe_rows = new->matrix.rows;
    }

    /*
     * Adding columns
     */
    if (new->matrix.columns > current->matrix.columns) {
        /*
         * Realloc each row array. Do not touch any rows added/deleted above
         * (use safe_rows)
         */
        for (i = 0; i < safe_rows; i++) {
            int j;

            new->matrix.colors[i] =
                (Pixel *) XtRealloc((char *)new->matrix.colors[i],
                                    new->matrix.columns * sizeof(Pixel));
            for (j = current->matrix.columns; j < new->matrix.columns; j++)
                new->matrix.colors[i][j] = new->manager.foreground;
        }
    }

    /*
     * Deleting columns
     *   if (new->matrix.columns < current->matrix.columns)
     * We don't bother to realloc, just leave some wasted space.
     * XXX is this a problem?
     */
}

/*
 * Add rows to the internal cells data structure.
 * If rows or labels is NULL, add empty rows.
 */
static void
AddRowsToTable(mw, position, rows, labels, colors, num_rows)
XbaeMatrixWidget mw;
int position;
String *rows;
String *labels;
Pixel *colors;
int num_rows;
{
    int i, j;

    /*
     * Realloc a larger array of row pointers and a larger label array
     */
    mw->matrix.cells = (String **) XtRealloc((char *)mw->matrix.cells,
                                             (mw->matrix.rows + num_rows) *
                                             sizeof(String *));
    if (mw->matrix.row_labels)
        mw->matrix.row_labels =
            (String *) XtRealloc((char *)mw->matrix.row_labels,
                                 (mw->matrix.rows + num_rows) *
                                 sizeof(String));
    if (mw->matrix.colors)
        mw->matrix.colors = (Pixel **) XtRealloc((char *)mw->matrix.colors,
                                                 (mw->matrix.rows + num_rows) *
                                                 sizeof(Pixel *));

    mw->matrix.selected_cells =
        (Boolean **) XtRealloc((char *)mw->matrix.selected_cells,
                               (mw->matrix.rows + num_rows) *
                               sizeof(Boolean *));

    /*
     * If we are inserting rows into the middle, we need to make room.
     * XXX we are assuming bcopy can handle overlapping moves.
     */
    if (position < mw->matrix.rows) {
        bcopy(&mw->matrix.cells[position],
              &mw->matrix.cells[position + num_rows],
              (mw->matrix.rows - position) * sizeof(String *));
        if (mw->matrix.row_labels)
            bcopy(&mw->matrix.row_labels[position],
                  &mw->matrix.row_labels[position + num_rows],
                  (mw->matrix.rows - position) * sizeof(String));
        if (mw->matrix.colors)
            bcopy(&mw->matrix.colors[position],
                  &mw->matrix.colors[position + num_rows],
                  (mw->matrix.rows - position) * sizeof(Pixel *));
        bcopy(&mw->matrix.selected_cells[position],
              &mw->matrix.selected_cells[position + num_rows],
              (mw->matrix.rows - position) * sizeof(Boolean *));
    }

    /*
     * Malloc a new row array for each new row. Copy the label for each row.
     * If no label was passed in, use a NULL String. Malloc a new Pixel
     * and Boolean row array for each new row.
     */
    for (i = 0; i < num_rows; i++) {
        mw->matrix.cells[i + position] =
            (String *) XtMalloc(mw->matrix.columns * sizeof(String));
        if (mw->matrix.row_labels)
            mw->matrix.row_labels[i + position] =
                labels ? XtNewString(labels[i]) : XtNewString("");
        if (mw->matrix.colors)
            mw->matrix.colors[i + position] =
                (Pixel *) XtMalloc(mw->matrix.columns * sizeof(Pixel));
        mw->matrix.selected_cells[i + position] =
            (Boolean *) XtMalloc(mw->matrix.columns * sizeof(Boolean));
    }

    /*
     * Copy the rows arrays passed in into each new row, or if NULL
     * was passed in initialize each row to NULL Strings. Copy the colors
     * arrays passed in into each new row, if NULL was passed use foreground.
     */
    for (i = 0; i < num_rows; i++)
        for (j = 0; j < mw->matrix.columns; j++) {
            mw->matrix.cells[i + position][j] =
                rows
                    ? XtNewString(rows[i * mw->matrix.columns + j])
                    : XtNewString("");
            if (mw->matrix.colors)
                mw->matrix.colors[i + position][j] =
                    colors
                        ? colors[i]
                        : mw->manager.foreground;
            mw->matrix.selected_cells[i + position][j] = False;
        }

    mw->matrix.rows += num_rows;
}

/*
 * Delete rows from the internal cells data structure.
 */
static void
DeleteRowsFromTable(mw, position, num_rows)
XbaeMatrixWidget mw;
int position;
int num_rows;
{
    int i, j;

    /*
     * We don't bother to realloc, we will just have some wasted space.
     * XXX is this a problem?
     */

    /*
     * Free all the cells in the rows being deleted and the rows themselves.
     * Also free the String row labels.  Free the color arrays for the rows
     * being deleted.
     */
    for (i = position; i < position + num_rows; i++) {
        for (j = 0; j < mw->matrix.columns; j++)
            XtFree((XtPointer) mw->matrix.cells[i][j]);
        XtFree((XtPointer) mw->matrix.cells[i]);
        if (mw->matrix.row_labels)
            XtFree((XtPointer) mw->matrix.row_labels[i]);
        if (mw->matrix.colors)
            XtFree((XtPointer) mw->matrix.colors[i]);
        XtFree((XtPointer) mw->matrix.selected_cells[i]);
    }

    /*
     * Copy those rows which are below the ones deleted, up.
     * (unless we deleted rows from the bottom).
     * XXX we assume bcopy can handle overlapping moves
     */
    if (position + num_rows < mw->matrix.rows) {
        bcopy(&mw->matrix.cells[position + num_rows],
              &mw->matrix.cells[position],
              (mw->matrix.rows - position - num_rows) * sizeof(String *));
        if (mw->matrix.row_labels)
            bcopy(&mw->matrix.row_labels[position + num_rows],
                  &mw->matrix.row_labels[position],
                  (mw->matrix.rows - position - num_rows) * sizeof(String));
        if (mw->matrix.colors)
            bcopy(&mw->matrix.colors[position + num_rows],
                  &mw->matrix.colors[position],
                  (mw->matrix.rows - position - num_rows) * sizeof(Pixel *));
        bcopy(&mw->matrix.selected_cells[position + num_rows],
              &mw->matrix.selected_cells[position],
              (mw->matrix.rows - position - num_rows) * sizeof(Boolean *));
    }

    mw->matrix.rows -= num_rows;
}

/*
 * Add columns to the internal cells data structure.
 * If columns or labels is NULL, add empty columns.
 * If max_lengths is NULL, widths will be used.
 * If alignments is NULL, use XmALIGNMENT_BEGINNING.
 * If label_alignments is NULL, use alignments, or if it is NULL
 *   XmALIGNMENT_BEGINNING.
 * widths must not be NULL.
 */
static void
AddColumnsToTable(mw, position, columns, labels, widths, max_lengths,
                  alignments, label_alignments, colors, num_columns)
XbaeMatrixWidget mw;
int position;
String *columns;
String *labels;
short *widths;
int *max_lengths;
unsigned char *alignments;
unsigned char *label_alignments;
Pixel *colors;
int num_columns;
{
    int i, j;

    /*
     * Realloc larger cells, widths, max_lengths, alignments,
     * colors, selected_cells, labels and label lines arrays.
     */

    for (i = 0; i < mw->matrix.rows; i++) {
        mw->matrix.cells[i] =
            (String *) XtRealloc((char *) mw->matrix.cells[i],
                                 (mw->matrix.columns + num_columns) *
                                 sizeof(String));
        if (mw->matrix.colors)
            mw->matrix.colors[i] =
                (Pixel *) XtRealloc((char *)mw->matrix.colors[i],
                                     (mw->matrix.columns + num_columns) *
                                     sizeof(Pixel));
        mw->matrix.selected_cells[i] =
            (Boolean *) XtRealloc((char *)mw->matrix.selected_cells[i],
                                   (mw->matrix.columns + num_columns) *
                                   sizeof(Boolean));
    }

    mw->matrix.column_widths =
        (short *) XtRealloc((char *) mw->matrix.column_widths,
                            (mw->matrix.columns + num_columns) *
                            sizeof(short));

    if (mw->matrix.column_max_lengths)
        mw->matrix.column_max_lengths =
            (int *) XtRealloc((char *) mw->matrix.column_max_lengths,
                              (mw->matrix.columns + num_columns) *
                              sizeof(int));

    if (mw->matrix.column_alignments)
        mw->matrix.column_alignments =
            (unsigned char *)
                XtRealloc((char *)mw->matrix.column_alignments,
                          (mw->matrix.columns + num_columns) *
                          sizeof(unsigned char));

    if (mw->matrix.column_label_alignments)
        mw->matrix.column_label_alignments =
            (unsigned char *) XtRealloc((char *)mw->matrix.
                                        column_label_alignments,
                                        (mw->matrix.columns + num_columns) *
                                        sizeof(unsigned char));

    if (mw->matrix.column_labels) {
        mw->matrix.column_labels =
            (String *) XtRealloc((char *)mw->matrix.column_labels,
                                 (mw->matrix.columns + num_columns) *
                                 sizeof(String));
        mw->matrix.column_label_lines =
            (ColumnLabelLines) XtRealloc((char *)mw->matrix.column_label_lines,
                                         (mw->matrix.columns + num_columns) *
                                         sizeof(ColumnLabelLinesRec));
    }

    /*
     * If we are inserting columns into the middle, we need to make room.
     * XXX we are assuming bcopy can handle overlapping moves.
     */
    if (position < mw->matrix.columns) {
        bcopy(&mw->matrix.column_widths[position],
              &mw->matrix.column_widths[position + num_columns],
              (mw->matrix.columns - position) * sizeof(short));

        if (mw->matrix.column_max_lengths)
            bcopy(&mw->matrix.column_max_lengths[position],
                  &mw->matrix.column_max_lengths[position + num_columns],
                  (mw->matrix.columns - position) * sizeof(int));

        if (mw->matrix.column_alignments)
            bcopy(&mw->matrix.column_alignments[position],
                  &mw->matrix.column_alignments[position + num_columns],
                  (mw->matrix.columns - position) * sizeof(unsigned char));

        if (mw->matrix.column_label_alignments)
            bcopy(&mw->matrix.column_label_alignments[position],
                  &mw->matrix.column_label_alignments[position + num_columns],
                  (mw->matrix.columns - position) * sizeof(unsigned char));

        if (mw->matrix.column_labels) {
            bcopy(&mw->matrix.column_labels[position],
                  &mw->matrix.column_labels[position + num_columns],
                  (mw->matrix.columns - position) * sizeof(String));
            bcopy(&mw->matrix.column_label_lines[position],
                  &mw->matrix.column_label_lines[position + num_columns],
                  (mw->matrix.columns - position) *
                  sizeof(ColumnLabelLinesRec));
        }

        /*
         * Shift the columns in each row.
         */
        for (i = 0; i < mw->matrix.rows; i++) {
            bcopy(&mw->matrix.cells[i][position],
                  &mw->matrix.cells[i][position + num_columns],
                  (mw->matrix.columns - position) * sizeof(String));
            if (mw->matrix.colors)
                bcopy(&mw->matrix.colors[i][position],
                      &mw->matrix.colors[i][position + num_columns],
                      (mw->matrix.columns - position) * sizeof(Pixel));
            bcopy(&mw->matrix.selected_cells[i][position],
                  &mw->matrix.selected_cells[i][position + num_columns],
                  (mw->matrix.columns - position) * sizeof(Boolean));
        }
    }

    /*
     * Copy all of the passed in info into each new column
     * (except column_positions which will be recalculated below).
     * If columns or labels is NULL, add empty columns.
     * If max_lengths is NULL, widths will be used.
     * If alignments is NULL, use XmALIGNMENT_BEGINNING.
     * If label_alignments is NULL, use XmALIGNMENT_BEGINNING
     * If labels is NULL, use NULL strings.
     * If colors is NULL, use foreground.
     */
    for (j = 0; j < num_columns; j++) {
        mw->matrix.column_widths[j + position] = widths[j];

        if (mw->matrix.column_max_lengths)
            mw->matrix.column_max_lengths[j + position] =
                max_lengths ? max_lengths[j] : (int)widths[j];

        if (mw->matrix.column_alignments)
            mw->matrix.column_alignments[j + position] =
                alignments ? alignments[j] : XmALIGNMENT_BEGINNING;

        if (mw->matrix.column_label_alignments)
            mw->matrix.column_label_alignments[j + position] =
                label_alignments ? label_alignments[j] : XmALIGNMENT_BEGINNING;

        if (mw->matrix.column_labels) {
            mw->matrix.column_labels[j + position] =
                labels ? XtNewString(labels[j]) : XtNewString("");
            ParseColumnLabel(mw->matrix.column_labels[j + position],
                             &mw->matrix.column_label_lines[j + position]);
        }

        /*
         * Add this new column to each row.
         */
        for (i = 0; i < mw->matrix.rows; i++) {
            mw->matrix.cells[i][j + position] =
                columns
                    ? XtNewString(columns[i * num_columns + j])
                    : XtNewString("");
            if (mw->matrix.colors)
                mw->matrix.colors[i][j + position] =
                    colors
                        ? colors[j]
                        : mw->manager.foreground;
            mw->matrix.selected_cells[i][j + position] = False;
        }
    }

    mw->matrix.columns += num_columns;
    GetCellTotalWidth(mw);

    /*
     * See if the max number of column label lines changed
     */
    if (mw->matrix.column_labels) {
        int end;
        end = position + num_columns;
        for (i = position; i < end; i++)
            if (mw->matrix.column_label_lines[i].lines >
                mw->matrix.column_label_maxlines)
                mw->matrix.column_label_maxlines =
                    mw->matrix.column_label_lines[i].lines;
    }

    /*
     * Recalculate the column positions
     */
    FreeColumnPositions(mw);
    mw->matrix.column_positions = CreateColumnPositions(mw);
    GetColumnPositions(mw);
}

/*
 * Delete columns from the internal cells data structure.
 */
static void
DeleteColumnsFromTable(mw, position, num_columns)
XbaeMatrixWidget mw;
int position;
int num_columns;
{
    int i, j;

    /*
     * Free all the cells in the columns being deleted.
     * Also free the String column labels and the associated ColumnLabelLines
     * lengths arrays.
     */
    for (j = position; j < position + num_columns; j++) {
        for (i = 0; i < mw->matrix.rows; i++)
            XtFree((XtPointer) mw->matrix.cells[i][j]);
        if (mw->matrix.column_labels) {
            XtFree((XtPointer) mw->matrix.column_labels[j]);
            XtFree((XtPointer) mw->matrix.column_label_lines[j].lengths);
        }
    }

    /*
     * Shift those columns after the ones being deleted, left.
     * (unless we deleted columns from the right).
     * XXX we assume bcopy can handle overlapping moves
     */
    if (position + num_columns < mw->matrix.columns) {
        bcopy(&mw->matrix.column_widths[position + num_columns],
              &mw->matrix.column_widths[position],
              (mw->matrix.columns - position - num_columns) * sizeof(short));

        if (mw->matrix.column_max_lengths)
            bcopy(&mw->matrix.column_max_lengths[position + num_columns],
                  &mw->matrix.column_max_lengths[position],
                  (mw->matrix.columns - position - num_columns) * sizeof(int));

        if (mw->matrix.column_alignments)
            bcopy(&mw->matrix.column_alignments[position + num_columns],
                  &mw->matrix.column_alignments[position],
                  (mw->matrix.columns - position - num_columns) *
                  sizeof(unsigned char));

        if (mw->matrix.column_label_alignments)
            bcopy(&mw->matrix.column_label_alignments[position + num_columns],
                  &mw->matrix.column_label_alignments[position],
                  (mw->matrix.columns - position - num_columns) *
                  sizeof(unsigned char));

        if (mw->matrix.column_labels) {
            bcopy(&mw->matrix.column_labels[position + num_columns],
                  &mw->matrix.column_labels[position],
                  (mw->matrix.columns - position - num_columns) *
                  sizeof(String));
            bcopy(&mw->matrix.column_label_lines[position + num_columns],
                  &mw->matrix.column_label_lines[position],
                  (mw->matrix.columns - position - num_columns) *
                  sizeof(ColumnLabelLinesRec));
        }

        /*
         * Shift the columns in each row.
         */
        for (i = 0; i < mw->matrix.rows; i++) {
            bcopy(&mw->matrix.cells[i][position + num_columns],
                  &mw->matrix.cells[i][position],
                  (mw->matrix.columns - position - num_columns) *
                  sizeof(String));
            if (mw->matrix.colors)
                bcopy(&mw->matrix.colors[i][position + num_columns],
                      &mw->matrix.colors[i][position],
                      (mw->matrix.columns - position - num_columns) *
                      sizeof(Pixel));
            bcopy(&mw->matrix.selected_cells[i][position + num_columns],
                  &mw->matrix.selected_cells[i][position],
                  (mw->matrix.columns - position - num_columns) *
                  sizeof(Boolean));
        }
    }

    mw->matrix.columns -= num_columns;
    GetCellTotalWidth(mw);

    /*
     * See if the max number of column label lines changed
     */
    if (mw->matrix.column_labels) {
        mw->matrix.column_label_maxlines =
            mw->matrix.column_label_lines[0].lines;
        for (i = 1; i < mw->matrix.columns; i++)
            if (mw->matrix.column_label_lines[i].lines >
                mw->matrix.column_label_maxlines)
                mw->matrix.column_label_maxlines =
                    mw->matrix.column_label_lines[i].lines;
    }

    /*
     * Recalculate the column positions
     */
    FreeColumnPositions(mw);
    mw->matrix.column_positions = CreateColumnPositions(mw);
    GetColumnPositions(mw);
}

/*
 * Try to make the row specified by the topRow resource (VERT_ORIGIN)
 * be the top row. The row is relative to fixed_rows - so 0 would
 * be the first non-fixed row.
 * If we can't make topRow the top row, make it as close as possible.
 */
static void
AdjustTopRow(mw)
XbaeMatrixWidget mw;
{
    int rows_visible = VISIBLE_HEIGHT(mw) / ROW_HEIGHT(mw);

    /*
     * If we have less than one full row visible, then count it as a full row
     */
    if (rows_visible == 0)
        rows_visible = 1;
    /*
     * rows_visible might be inaccurate since Clip may not have been resized
     */
    else if (rows_visible > mw->matrix.rows)
        rows_visible = mw->matrix.rows;

    if (VERT_ORIGIN(mw) > (mw->matrix.rows - (rows_visible +
                                              mw->matrix.fixed_rows)))
        VERT_ORIGIN(mw) = mw->matrix.rows - (rows_visible +
                                             mw->matrix.fixed_rows);
    else if (VERT_ORIGIN(mw) < 0)
        VERT_ORIGIN(mw) = 0;
}

/*
 * Return the top and bottom-most visible non-fixed row
 */
static void
GetVisibleRows(mw, top_row, bottom_row)
XbaeMatrixWidget mw;
int *top_row, *bottom_row;
{
    *top_row = VERT_ORIGIN(mw) + mw->matrix.fixed_rows;
    *bottom_row = *top_row + VISIBLE_HEIGHT(mw) / ROW_HEIGHT(mw);
}

/*
 * Return the left and right-most visible non-fixed column
 */
static void
GetVisibleColumns(mw, left_column, right_column)
XbaeMatrixWidget mw;
int *left_column, *right_column;
{
    *left_column = XtoCol(mw, FIXED_COLUMN_WIDTH(mw) +  HORIZ_ORIGIN(mw));
    *right_column = XtoCol(mw, FIXED_COLUMN_WIDTH(mw) +  HORIZ_ORIGIN(mw) +
                           VISIBLE_WIDTH(mw));
}

/*
 * Return the top and bottom row and left and right column of
 * the visible non-fixed cells
 */
static void
GetVisibleCells(mw, top_row, bottom_row, left_column, right_column)
XbaeMatrixWidget mw;
int *top_row, *bottom_row, *left_column, *right_column;
{
    GetVisibleRows(mw, top_row, bottom_row);
    GetVisibleColumns(mw, left_column, right_column);
}

/*
 * Utility function to clear a cell so we can draw something new in it.
 * Does not generate expose events on the cell.
 * Does not check if the cell is actually visible before clearing it.
 */
static void
ClearCell(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    int x, y;
    Window win = CELL_WINDOW(mw, row, column);

    if (!win)
        return;

    RowColToXY(mw, row, column, &x, &y);

    XClearArea(XtDisplay(mw), win,
               x, y,
               COLUMN_WIDTH(mw, column),
               ROW_HEIGHT(mw),
               False);
}

/*
 * Return True if a row is visible on the screen (not scrolled totally off)
 */
static Boolean
IsRowVisible(mw, row)
XbaeMatrixWidget mw;
int row;
{
    /*
     * If we are not in a fixed row, see if we are on the screen vertically
     * (fixed rows are always on the screen)
     */
    if (row >= mw->matrix.fixed_rows) {
        row -= mw->matrix.fixed_rows;
        if (row >= VERT_ORIGIN(mw) &&
            row <
                ClipChild(mw)->core.height / ROW_HEIGHT(mw) + VERT_ORIGIN(mw))
            return True;
    }
    else
        return True;

    return False;
}

/*
 * Return True if a column is visible on the screen (not scrolled totally off)
 */
static Boolean
IsColumnVisible(mw, column)
XbaeMatrixWidget mw;
int column;
{
    /*
     * If we are not in a fixed column, see if we are on the screen
     * horizontally (fixed columns are always on the screen)
     */
    if (column >= mw->matrix.fixed_columns) {
        int x;

        /*
         * Calculate the x endpoints of this column
         */
        x = mw->matrix.column_positions[column] -
            mw->matrix.column_positions[mw->matrix.fixed_columns];

        /*
         * Check if we are visible horizontally
         */
        if (x + COLUMN_WIDTH(mw, column) > HORIZ_ORIGIN(mw) &&
            x < ClipChild(mw)->core.width + HORIZ_ORIGIN(mw))
            return True;
    }
    else
        return True;

    return False;
}

/*
 * Return True if a cell is visible on the screen (not scrolled totally off)
 */
static Boolean
IsCellVisible(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    return IsRowVisible(mw, row) && IsColumnVisible(mw, column);
}

/*
 * Scroll a row so it is visible on the screen.
 */
static void
MakeRowVisible(mw, row)
XbaeMatrixWidget mw;
int row;
{
    int rows_visible;
    int value, slider_size, increment, page_increment, vert_value;

    /*
     * If we are in a fixed row, we are already visible.
     */
    if (row < mw->matrix.fixed_rows)
        return;

    /*
     * Take into account fixed_rows.
     * Calculate the number of rows visible. If less than one full
     * row is visible, use one full row.
     */
    row -= mw->matrix.fixed_rows;
    rows_visible = VISIBLE_HEIGHT(mw) / ROW_HEIGHT(mw);
    if (rows_visible == 0)
        rows_visible = 1;

    /*
     * Figure out the new value of the VSB to scroll this cell
     * onto the screen (the VSB uses row coordinates instead of pixels)
     */
    if (row < VERT_ORIGIN(mw))
        vert_value = row;
    else if (row >= rows_visible + VERT_ORIGIN(mw))
        vert_value = row - rows_visible + 1;
    else
        vert_value = VERT_ORIGIN(mw);

    /*
     * Give the VSB the new value and pass a flag to make it call
     * our scroll callbacks
     */
    if (vert_value != VERT_ORIGIN(mw)) {
        XmScrollBarGetValues(VertScrollChild(mw), &value,
                             &slider_size, &increment, &page_increment);
        XmScrollBarSetValues(VertScrollChild(mw), vert_value,
                             slider_size, increment, page_increment, True);
    }
}

/*
 * Scroll a column so it is visible on the screen.
 */
static void
MakeColumnVisible(mw, column)
XbaeMatrixWidget mw;
int column;
{
    int value, slider_size, increment, page_increment, x, horiz_value;

    /*
     * If we are in a fixed column, we are already visible.
     */
    if (column < mw->matrix.fixed_columns)
        return;

    /*
     * Calculate the x position of this column
     */
    x = mw->matrix.column_positions[column] -
        mw->matrix.column_positions[mw->matrix.fixed_columns];

    /*
     * Figure out the new value of the HSB to scroll this cell
     * onto the screen. If the whole cell won't fit, scroll so its
     * left edge is visible.
     */
    if (x < HORIZ_ORIGIN(mw))
        horiz_value = x;
    else if (x + COLUMN_WIDTH(mw, column) >
             VISIBLE_WIDTH(mw) + HORIZ_ORIGIN(mw)) {
        int off = (x + COLUMN_WIDTH(mw, column)) - (VISIBLE_WIDTH(mw) +
                                                    HORIZ_ORIGIN(mw));
        if (x - off < HORIZ_ORIGIN(mw))
            horiz_value = x;
        else
            horiz_value = HORIZ_ORIGIN(mw) + off;
    }
    else
        horiz_value = HORIZ_ORIGIN(mw);

    /*
     * Give the HSB the new value and pass a flag to make it
     * call our scroll callbacks
     */
    if (horiz_value != HORIZ_ORIGIN(mw)) {
        XmScrollBarGetValues(HorizScrollChild(mw), &value,
                             &slider_size, &increment, &page_increment);
        XmScrollBarSetValues(HorizScrollChild(mw), horiz_value,
                             slider_size, increment, page_increment, True);
    }
}

/*
 * Scrolls a fixed or non-fixed cell so it is visible on the screen.
 */
static void
MakeCellVisible(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    MakeRowVisible(mw, row);
    MakeColumnVisible(mw, column);
}

static Boolean
DoCommitEdit(mw)
XbaeMatrixWidget mw;
{
    String cell;

    if (!XtIsManaged(TextChild(mw)))
        return True;

    /*
     * Get the value the user entered in the textField (this is a copy)
     */
    cell = XmTextFieldGetString(TextChild(mw));

    /*
     * Call the leaveCellCallback to see if we can leave the current cell.
     */
    if (mw->matrix.leave_cell_callback) {
        XbaeMatrixLeaveCellCallbackStruct call_data;

        call_data.reason = XbaeLeaveCellReason;
        call_data.row = mw->matrix.current_row;
        call_data.column = mw->matrix.current_column;
        call_data.value = cell;
        call_data.doit = True;

        XtCallCallbackList((Widget)mw, mw->matrix.leave_cell_callback,
                           (XtPointer) &call_data);

        /*
         * Application doesn't want to leave this cell. Make the cell visible
         * and traverse to it so the user can see where they screwed up.
         */
        if (!call_data.doit) {
            MakeCellVisible(mw,
                            mw->matrix.current_row, mw->matrix.current_column);
            XmProcessTraversal(TextChild(mw), XmTRAVERSE_CURRENT);
            XtFree((XtPointer)cell);
            return False;
        }

        /*
         * Use the applications value if it is different.
         * If the application modified the string inplace, we will pick that
         * up automatically.
         */
        if (call_data.value != cell) {
            XtFree((XtPointer)cell);
            cell = call_data.value;
        }
    }

    /*
     * Call the set_cell method to store the new value in the cell and redraw.
     */
    (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.set_cell)
        (mw, mw->matrix.current_row, mw->matrix.current_column, cell);

    XtFree((XtPointer)cell);

    return True;
}

/*
 * Matrix set_cell method
 */
static void
SetCell(mw, row, column, value)
XbaeMatrixWidget mw;
int row, column;
String value;
{
    if (row >= mw->matrix.rows || row < 0 ||
        column >= mw->matrix.columns || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "setCell", "badIndex", "XbaeMatrix",
             "XbaeMatrix: Row or column parameter out of bounds for SetCell.",
                        NULL, 0);
        return;
    }

    /*
     * Store the new value in the cell.
     */
    XtFree((XtPointer)mw->matrix.cells[row][column]);
    mw->matrix.cells[row][column] = XtNewString(value);

    /*
     * Draw the cell.
     */
    if (IsCellVisible(mw, row, column)) {
        ClearCell(mw, row, column);
        DrawCell(mw, row, column);
    }

    /*
     * If we are editing this cell, load the textField too.
     * XXX there is a bug in XmTextFieldSetString in Motif 1.1.3
     *     it doesn't handle "" strings well.  Use SetValues instead.
     */
    if (XtIsManaged(TextChild(mw)) &&
        mw->matrix.current_row == row && mw->matrix.current_column == column) {
        if (value[0] == '\0')
            XtVaSetValues(TextChild(mw),
                          XmNvalue, value,
                          NULL);
        else
            XmTextFieldSetString(TextChild(mw), value);
    }
}

/*
 * Public interface to set_cell method
 */
void
XbaeMatrixSetCell(w, row, column, value)
Widget w;
int row, column;
String value;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the set_cell method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.set_cell)
        ((XbaeMatrixWidget)w, row, column, value);
}

/*
 * Matrix edit_cell method
 */
static void
EditCell(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    Boolean edit = True;
    int x, y;

    if (row >= mw->matrix.rows || row < 0 ||
        column >= mw->matrix.columns || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "editCell", "badIndex", "XbaeMatrix",
             "XbaeMatrix: Row or column parameter out of bounds for EditCell.",
                        NULL, 0);
        return;
    }

    /*
     * Attempt to commit the edit in the current cell. Return if we fail.
     */
    if (!DoCommitEdit(mw))
        return;

    /*
     * Scroll the cell onto the screen
     */
    MakeCellVisible(mw, row, column);

    /*
     * Fixed cells are not editable
     */
    if (IS_FIXED(mw, row, column))
        return;

    /*
     * If we have an enterCellCallback, call it to see if the cell is
     * editable.
     */
    if (mw->matrix.enter_cell_callback) {
        XbaeMatrixEnterCellCallbackStruct call_data;

        call_data.reason = XbaeEnterCellReason;
        call_data.row = row;
        call_data.column = column;
        call_data.doit = True;
        XtCallCallbackList((Widget)mw, mw->matrix.enter_cell_callback,
                           (XtPointer) &call_data);

        edit = call_data.doit;
    }

    mw->matrix.current_row = row;
    mw->matrix.current_column = column;

    /*
     * Unmap the textField to avoid flashing.
     */
    if (XtIsManaged(TextChild(mw)) && XtIsRealized(TextChild(mw)))
        XtUnmapWidget(TextChild(mw));

    /*
     * Convert the row/column to an xy position and move the textField
     * to this position. (the xy position will be relative to the Clip
     * widget because only non-fixed cells are editable)
     */
    RowColToXY(mw, row, column, &x, &y);
    XtMoveWidget(TextChild(mw), x, y);

#ifdef notdef
    /*
     * XXX Resetting the XmTextField foreground can cause a protocol error in
     * XmTextField.  I filed a bug report.  Put this code back in when it
     * is fixed.
     */
    /*
     * Figure out what color the textField needs to be
     */
    if (mw->matrix.colors)
        color = mw->matrix.colors[row][column];
    else
        color = mw->manager.foreground;

    /*
     * Include XmNforeground in the setvalues below
     */
#endif

    /*
     * Setup the textField for the new cell. If the modifyVerify CB
     * rejects the new value, then it is the applications fault for
     * loading the cell with a bad value to begin with.
     */
    XtVaSetValues(TextChild(mw),
                  XmNvalue,     mw->matrix.cells[row][column],
                  XmNwidth,     COLUMN_WIDTH(mw, column),
                  XmNheight,    ROW_HEIGHT(mw),
                  XmNmaxLength, mw->matrix.column_max_lengths
                                ? mw->matrix.column_max_lengths[column]
                                : (int)mw->matrix.column_widths[column],
                  XmNeditable,  edit,
                  XmNcursorPositionVisible, edit,
                  NULL);

    /*
     * We need this to work around an XmTextField problem with
     * the I-beam and caret
     */
    if (edit)
        XmTextFieldSetInsertionPosition(TextChild(mw),
                                        strlen(mw->matrix.cells[row][column]));

    /*
     * Manage and map the textField
     */
    XtManageChild(TextChild(mw));
    if (XtIsRealized(TextChild(mw)))
        XtMapWidget(TextChild(mw));
}

/*
 * Public interface to edit_cell method
 */
void
XbaeMatrixEditCell(w, row, column)
Widget w;
int row, column;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the edit_cell method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.edit_cell)
        ((XbaeMatrixWidget)w, row, column);

    XmProcessTraversal(TextChild(((XbaeMatrixWidget)w)), XmTRAVERSE_CURRENT);
}

/*
 * Matrix select_cell method
 */
static void
SelectCell(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    if (row >= mw->matrix.rows || row < 0 ||
        column >= mw->matrix.columns || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "selectCell", "badIndex", "XbaeMatrix",
           "XbaeMatrix: Row or column parameter out of bounds for SelectCell.",
                        NULL, 0);
        return;
    }

    /*
     * Scroll the cell onto the screen
     */
    MakeCellVisible(mw, row, column);

    /*
     * If the cell is not already selected, select it and redraw it
     */
    if (!mw->matrix.selected_cells[row][column]) {
        mw->matrix.selected_cells[row][column] = True;
        DrawCell(mw, row, column);
    }
}

/*
 * Public interface to select_cell method
 */
void
XbaeMatrixSelectCell(w, row, column)
Widget w;
int row, column;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the select_cell method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.select_cell)
        ((XbaeMatrixWidget)w, row, column);
}

/*
 * Matrix select_row method
 */
static void
SelectRow(mw, row)
XbaeMatrixWidget mw;
int row;
{
    int j, lc, rc;

    if (row >= mw->matrix.rows || row < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "selectRow", "badIndex", "XbaeMatrix",
                        "XbaeMatrix: Row out of bounds for SelectRow.",
                        NULL, 0);
        return;
    }

    /*
     * Scroll the row onto the screen
     */
    MakeRowVisible(mw, row);

    /*
     * For each cell in the row, if the cell is not already selected,
     * select it and redraw it
     */
    GetVisibleColumns(mw, &lc, &rc);
    for (j = 0; j < mw->matrix.columns; j++) {
        if (!mw->matrix.selected_cells[row][j]) {
            mw->matrix.selected_cells[row][j] = True;
            if ((j >= lc && j <= rc) || j < mw->matrix.fixed_columns) {
                ClearCell(mw, row, j);
                DrawCell(mw, row, j);
            }
        }
    }
}

/*
 * Public interface to select_row method
 */
void
XbaeMatrixSelectRow(w, row)
Widget w;
int row;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the select_row method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.select_row)
        ((XbaeMatrixWidget)w, row);
}

/*
 * Matrix select_column method
 */
static void
SelectColumn(mw, column)
XbaeMatrixWidget mw;
int column;
{
    int i, tr, br;

    if (column >= mw->matrix.columns || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "selectColumn", "badIndex", "XbaeMatrix",
                        "XbaeMatrix: Column out of bounds for SelectColumn.",
                        NULL, 0);
        return;
    }

    /*
     * Scroll the column onto the screen
     */
    MakeColumnVisible(mw, column);

    /*
     * For each cell in the column, if the cell is not already selected,
     * select it and redraw it
     */
    GetVisibleRows(mw, &tr, &br);
    for (i = 0; i < mw->matrix.rows; i++) {
        if (!mw->matrix.selected_cells[i][column]) {
            mw->matrix.selected_cells[i][column] = True;
            if ((i >= tr && i <= br) || i < mw->matrix.fixed_rows) {
                ClearCell(mw, i, column);
                DrawCell(mw, i, column);
            }
        }
    }
}

/*
 * Public interface to select_column method
 */
void
XbaeMatrixSelectColumn(w, column)
Widget w;
int column;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the select_column method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.select_column)
        ((XbaeMatrixWidget)w, column);
}

/*
 * Matrix deselect_all method
 */
static void
DeselectAll(mw)
XbaeMatrixWidget mw;
{
    int i, j;
    int tr, br, lc, rc;

    GetVisibleCells(mw, &tr, &br, &lc, &rc);
    for (i = 0; i < mw->matrix.rows; i++)
        for (j = 0; j < mw->matrix.columns; j++) {
            if (mw->matrix.selected_cells[i][j]) {
                mw->matrix.selected_cells[i][j] = False;
                if ((i >= tr && i <= br && j >= lc && j <= rc) ||
                    (i < mw->matrix.fixed_rows && j >= lc && j <= rc) ||
                    (j < mw->matrix.fixed_columns && i >= tr && i <= br)) {
                    ClearCell(mw, i, j);
                    DrawCell(mw, i, j);
                }
            }
        }
}

/*
 * Public interface to deselect_all method
 */
void
XbaeMatrixDeselectAll(w)
Widget w;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the deselect_all method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.deselect_all)
        ((XbaeMatrixWidget)w);
}

/*
 * Matrix deselect_cell method
 */
static void
DeselectCell(mw, row, column)
XbaeMatrixWidget mw;
int row;
int column;
{
    if (row >= mw->matrix.rows || row < 0 ||
        column > mw->matrix.columns - 1 || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deselectCell", "badIndex", "XbaeMatrix",
         "XbaeMatrix: Row or column parameter out of bounds for DeselectCell.",
                        NULL, 0);
        return;
    }

    if (mw->matrix.selected_cells[row][column]) {
        mw->matrix.selected_cells[row][column] = False;
        if (IsCellVisible(mw, row, column)) {
            ClearCell(mw, row, column);
            DrawCell(mw, row, column);
        }
    }
}

/*
 * Public interface to deselect_cell method
 */
void
XbaeMatrixDeselectCell(w, row, column)
Widget w;
int row;
int column;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the deselect_cell method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.deselect_cell)
        ((XbaeMatrixWidget)w, row, column);
}

/*
 * Matrix deselect_row method
 */
static void
DeselectRow(mw, row)
XbaeMatrixWidget mw;
int row;
{
    int j, lc, rc;

    if (row >= mw->matrix.rows || row < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deselectRow", "badIndex", "XbaeMatrix",
                "XbaeMatrix: Row parameter out of bounds for DeselectRow.",
                        NULL, 0);
        return;
    }

    /*
     * For each cell in the row, if the cell is selected,
     * deselect it and redraw it
     */
    GetVisibleColumns(mw, &lc, &rc);
    for (j = 0; j < mw->matrix.columns; j++) {
        if (mw->matrix.selected_cells[row][j]) {
            mw->matrix.selected_cells[row][j] = False;
            if ((j >= lc && j <= rc) || j < mw->matrix.fixed_columns) {
                ClearCell(mw, row, j);
                DrawCell(mw, row, j);
            }
        }
    }
}

/*
 * Public interface to deselect_row method
 */
void
XbaeMatrixDeselectRow(w, row)
Widget w;
int row;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the deselect_row method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.deselect_row)
        ((XbaeMatrixWidget)w, row);
}

/*
 * Matrix deselect_column method
 */
static void
DeselectColumn(mw, column)
XbaeMatrixWidget mw;
int column;
{
    int i, tr, br;

    if (column >= mw->matrix.columns || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deselectColumn", "badIndex", "XbaeMatrix",
        "XbaeMatrix: Column parameter out of bounds for DeselectColumn.",
                        NULL, 0);
        return;
    }

    /*
     * For each cell in the column, if the cell is selected,
     * deselect it and redraw it
     */
    GetVisibleRows(mw, &tr, &br);
    for (i = 0; i < mw->matrix.rows; i++) {
        if (mw->matrix.selected_cells[i][column]) {
            mw->matrix.selected_cells[i][column] = False;
            if ((i >= tr && i <= br) || i < mw->matrix.fixed_rows) {
                ClearCell(mw, i, column);
                DrawCell(mw, i, column);
            }
        }
    }
}

/*
 * Public interface to deselect_column method
 */
void
XbaeMatrixDeselectColumn(w, column)
Widget w;
int column;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the deselect_column method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.deselect_column)
        ((XbaeMatrixWidget)w, column);
}

/*
 * Matrix get_cell method
 */
static String
GetCell(mw, row, column)
XbaeMatrixWidget mw;
int row, column;
{
    if (row >= mw->matrix.rows || row < 0 ||
        column > mw->matrix.columns - 1 || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "getCell", "badIndex", "XbaeMatrix",
        "XbaeMatrix: Row or column parameter out of bounds for GetCell.",
                        NULL, 0);
        return(NULL);
    }

    return mw->matrix.cells[row][column];
}

/*
 * Public interface to get_cell method
 */
String
XbaeMatrixGetCell(w, row, column)
Widget w;
int row, column;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the get_cell method
     */
    return (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.get_cell)
        ((XbaeMatrixWidget)w, row, column);
}

/*
 * Matrix commit_edit method
 */
static Boolean
#if NeedFunctionPrototypes
CommitEdit(XbaeMatrixWidget mw, Boolean unmap)
#else
CommitEdit(mw, unmap)
XbaeMatrixWidget mw;
Boolean unmap;
#endif
{
    Boolean commit;

    if (!XtIsManaged(TextChild(mw)))
        return True;

    /*
     * Attempt to commit the edit
     */
    commit = DoCommitEdit(mw);

    /*
     * If the commit succeeded and we are supposed to unmap the textField,
     * then hide the textField and traverse out
     */
    if (commit && unmap) {
        XtUnmanageChild(TextChild(mw));
        XmProcessTraversal(TextChild(mw), XmTRAVERSE_RIGHT);
    }

    return commit;
}

/*
 * Public interface to commit_edit method
 */
Boolean
#if NeedFunctionPrototypes
XbaeMatrixCommitEdit(Widget w, Boolean unmap)
#else
XbaeMatrixCommitEdit(w, unmap)
Widget w;
Boolean unmap;
#endif
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the commit_edit method
     */
    return (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.commit_edit)
        ((XbaeMatrixWidget)w, unmap);
}

/*
 * Matrix cancel_edit method
 */
static void
#if NeedFunctionPrototypes
CancelEdit(XbaeMatrixWidget mw, Boolean unmap)
#else
CancelEdit(mw, unmap)
XbaeMatrixWidget mw;
Boolean unmap;
#endif
{
    if (!XtIsManaged(TextChild(mw)))
        return;

    /*
     * If unmap is set, hide the textField and traverse out.
     */
    if (unmap) {
        XtUnmanageChild(TextChild(mw));
        XmProcessTraversal(TextChild(mw), XmTRAVERSE_RIGHT);
    }

    /*
     * Don't unmap, just restore original contents
     */
    else {
        XtVaSetValues(TextChild(mw),
                      XmNvalue, mw->matrix.cells[mw->matrix.current_row]
                                                [mw->matrix.current_column],
                      NULL);
    }
}

/*
 * Public interface to cancel_edit method
 */
void
#if NeedFunctionPrototypes
XbaeMatrixCancelEdit(Widget w, Boolean unmap)
#else
XbaeMatrixCancelEdit(w, unmap)
Widget w;
Boolean unmap;
#endif
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the cancel_edit method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.cancel_edit)
        ((XbaeMatrixWidget)w, unmap);
}

/*
 * Matrix add_rows method
 */
static void
AddRows(mw, position, rows, labels, colors, num_rows)
XbaeMatrixWidget mw;
int position;
String *rows;
String *labels;
Pixel *colors;
int num_rows;
{
    /*
     * Do some error checking.
     */
    if (num_rows <= 0)
        return;
    if (position < 0 || position > mw->matrix.rows) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "addRows", "badPosition", "XbaeMatrix",
                        "XbaeMatrix: Position out of bounds in AddRows.",
                        NULL, 0);
        return;
    }

    /*
     * Add the new rows into the internal cells/labels data structure.
     */
    AddRowsToTable(mw, position, rows, labels, colors, num_rows);

    /*
     * Reconfig the VSB maximum.
     */
    XtVaSetValues(VertScrollChild(mw),
                  XmNmaximum,   (mw->matrix.rows -
                                 (int) mw->matrix.fixed_rows),
                  NULL);

    /*
     * Relayout.
     */
    Resize(mw);

    /*
     * Call our cancel_edit method since the rows shifted underneath us
     */
    (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.cancel_edit)
        (mw, True);

    if (XtIsRealized(mw)) {
        /*
         * Generate expose events on Matrix and Clip to force the
         * new rows to be drawn.
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   True);
        XbaeClipRedraw(ClipChild(mw));
    }
}

/*
 * Public interface to add_rows method
 */
void
XbaeMatrixAddRows(w, position, rows, labels, colors, num_rows)
Widget w;
int position;
String *rows;
String *labels;
Pixel *colors;
int num_rows;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the add_rows method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.add_rows)
        ((XbaeMatrixWidget)w, position, rows, labels, colors, num_rows);
}

/*
 * Matrix delete_rows method
 */
static void
DeleteRows(mw, position, num_rows)
XbaeMatrixWidget mw;
int position;
int num_rows;
{
    /*
     * Do some error checking.
     */
    if (num_rows <= 0)
        return;
    if (position < 0 || position + num_rows > mw->matrix.rows) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deleteRows", "badPosition", "XbaeMatrix",
                        "XbaeMatrix: Position out of bounds in DeleteRows.",
                        NULL, 0);
        return;
    }
    if (num_rows >= mw->matrix.rows - mw->matrix.fixed_rows) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deleteRows", "tooMany", "XbaeMatrix",
                "XbaeMatrix: Attempting to delete too many rows in DeleteRows.",
                        NULL, 0);
        return;
    }

    /*
     * Delete the new rows from the internal cells/labels data structure.
     */
    DeleteRowsFromTable(mw, position, num_rows);

    /*
     * Reconfig the VSB maximum. Reset the sliderSize to avoid warnings.
     */
    XtVaSetValues(VertScrollChild(mw),
                  XmNmaximum,   (mw->matrix.rows -
                                 (int) mw->matrix.fixed_rows),
                  XmNsliderSize,1,
                  NULL);

    /*
     * Relayout.
     */
    Resize(mw);

    /*
     * Call our cancel_edit method since the rows shifted underneath us
     */
    (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.cancel_edit)
        (mw, True);

    if (XtIsRealized(mw)) {
        /*
         * Generate expose events on Matrix and Clip to force the
         * rows to be redrawn.
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   True);
        XbaeClipRedraw(ClipChild(mw));
    }
}

/*
 * Public interface to delete_rows method
 */
void
XbaeMatrixDeleteRows(w, position, num_rows)
Widget w;
int position;
int num_rows;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the delete_rows method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.delete_rows)
        ((XbaeMatrixWidget)w, position, num_rows);
}

/*
 * Matrix add_columns method.
 */
static void
AddColumns(mw, position, columns, labels, widths, max_lengths,
           alignments, label_alignments, colors, num_columns)
XbaeMatrixWidget mw;
int position;
String *columns;
String *labels;
short *widths;
int *max_lengths;
unsigned char *alignments;
unsigned char *label_alignments;
Pixel *colors;
int num_columns;
{
    /*
     * Do some error checking.
     */
    if (num_columns <= 0)
        return;
    if (position < 0 || position > mw->matrix.columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "addColumns", "badPosition", "XbaeMatrix",
                        "XbaeMatrix: Position out of bounds in AddColumns.",
                        NULL, 0);
        return;
    }
    if (!widths) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "addColumns", "noWidths", "XbaeMatrix",
                        "XbaeMatrix: Must specify column widths in AddColumns.",
                        NULL, 0);
        return;
    }

    /*
     * Add the new rows into the internal cells/labels data structure.
     */
    AddColumnsToTable(mw, position, columns, labels, widths, max_lengths,
                      alignments, label_alignments, colors, num_columns);

    /*
     * Reconfig the HSB maximum.
     */
    XtVaSetValues(HorizScrollChild(mw),
                  XmNmaximum,   CELL_TOTAL_WIDTH(mw),
                  NULL);

    /*
     * Relayout.
     */
    Resize(mw);

    /*
     * Call our cancel_edit method since the columns shifted underneath us
     */
    (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.cancel_edit)
        (mw, True);

    if (XtIsRealized(mw)) {
        /*
         * Generate expose events on Matrix and Clip to force the
         * new columns to be drawn.
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   True);
        XbaeClipRedraw(ClipChild(mw));
    }
}

/*
 * Public interface to add_columns method
 */
void
XbaeMatrixAddColumns(w, position, columns, labels, widths, max_lengths,
                          alignments, label_alignments, colors, num_columns)
Widget w;
int position;
String *columns;
String *labels;
short *widths;
int *max_lengths;
unsigned char *alignments;
unsigned char *label_alignments;
Pixel *colors;
int num_columns;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the add_columns method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.add_columns)
        ((XbaeMatrixWidget)w, position, columns, labels, widths,
         max_lengths, alignments, label_alignments, colors, num_columns);
}

/*
 * Matrix delete_columns method
 */
static void
DeleteColumns(mw, position, num_columns)
XbaeMatrixWidget mw;
int position;
int num_columns;
{
    /*
     * Do some error checking.
     */
    if (num_columns <= 0)
        return;
    if (position < 0 || position + num_columns > mw->matrix.columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deleteColumns", "badPosition", "XbaeMatrix",
                        "XbaeMatrix: Position out of bounds in DeleteColumns.",
                        NULL, 0);
        return;
    }
    if (num_columns >= mw->matrix.columns - mw->matrix.fixed_columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "deleteColumns", "tooMany", "XbaeMatrix",
        "XbaeMatrix: Attempting to delete too many columns in DeleteColumns.",
                        NULL, 0);
        return;
    }

    /*
     * Delete the new columns from the internal cells/labels data structure.
     */
    DeleteColumnsFromTable(mw, position, num_columns);

    /*
     * Reconfig the HSB maximum. Reset the sliderSize to avoid warnings.
     */
    XtVaSetValues(HorizScrollChild(mw),
                  XmNmaximum,   CELL_TOTAL_WIDTH(mw),
                  XmNsliderSize,1,
                  NULL);

    /*
     * Relayout.
     */
    Resize(mw);

    /*
     * Call our cancel_edit method since the columns shifted underneath us
     */
    (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.cancel_edit)
        (mw, True);

    if (XtIsRealized(mw)) {
        /*
         * Generate expose events on Matrix and Clip to force the
         * columns to be redrawn.
         */
        XClearArea(XtDisplay(mw), XtWindow(mw),
                   0, 0,
                   0 /*Full Width*/, 0 /*Full Height*/,
                   True);
        XbaeClipRedraw(ClipChild(mw));
    }
}

/*
 * Public interface to delete_columns method
 */
void
XbaeMatrixDeleteColumns(w, position, num_columns)
Widget w;
int position;
int num_columns;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the delete_columns method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.delete_columns)
        ((XbaeMatrixWidget)w, position, num_columns);
}

/*
 * Matrix set_row_colors method
 */
static void
SetRowColors(mw, position, colors, num_colors)
XbaeMatrixWidget mw;
int position;
Pixel *colors;
int num_colors;
{
    Rectangle rect;
    int i, j;

    /*
     * Do some error checking.
     */
    if (num_colors <= 0)
        return;
    if (position < 0 || position + num_colors > mw->matrix.rows) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "setRowColors", "badPosition", "XbaeMatrix",
      "XbaeMatrix: Position out of bounds or too many colors in SetRowColors.",
                        NULL, 0);
        return;
    }

    /*
     * If we don't have any colors yet, malloc them, and initialize
     * unused entries to foreground
     */
    if (mw->matrix.colors == NULL) {
        CreateColors(mw);
        for (i = 0; i < position; i++)
            for (j = 0; j < mw->matrix.columns; j++)
                mw->matrix.colors[i][j] = mw->manager.foreground;
        for (i = position + num_colors; i < mw->matrix.rows; i++)
            for (j = 0; j < mw->matrix.columns; j++)
                mw->matrix.colors[i][j] = mw->manager.foreground;
    }

    /*
     * Set each row to the appropriate color
     */
    for (i = 0; i < num_colors; i++)
        for (j = 0; j < mw->matrix.columns; j++)
            mw->matrix.colors[i + position][j] = colors[i];

    if (XtIsRealized(mw)) {
        /*
         * Redraw all the visible non-fixed cells. We don't need to clear first
         * since only the color changed.
         */
        SETRECT(rect,
                0, 0,
                ClipChild(mw)->core.width - 1, ClipChild(mw)->core.height - 1);
        RedrawCells(mw, &rect);

        /*
         * Redraw all the visible fixed cells (but not the labels).
         * We don't need to clear first since only the color changed.
         */
        SETRECT(rect,
                ROW_LABEL_WIDTH(mw), COLUMN_LABEL_HEIGHT(mw),
                mw->core.width - 1, mw->core.height - 1);
        RedrawLabelsAndFixed(mw, &rect);
    }
}

/*
 * Public interface to set_row_colors method
 */
void
XbaeMatrixSetRowColors(w, position, colors, num_colors)
Widget w;
int position;
Pixel *colors;
int num_colors;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the set_row_colors method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.set_row_colors)
        ((XbaeMatrixWidget)w, position, colors, num_colors);
}

/*
 * Matrix set_column_colors method
 */
static void
SetColumnColors(mw, position, colors, num_colors)
XbaeMatrixWidget mw;
int position;
Pixel *colors;
int num_colors;
{
    Rectangle rect;
    int i, j;

    /*
     * Do some error checking.
     */
    if (num_colors <= 0)
        return;
    if (position < 0 || position + num_colors > mw->matrix.columns) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "setColumnColors", "badPosition", "XbaeMatrix",
"XbaeMatrix: Position out of bounds or too many colors in SetColumnColors.",
                        NULL, 0);
        return;
    }

    /*
     * If we don't have any colors yet, malloc them, and initialize
     * unused entries to foreground
     */
    if (mw->matrix.colors == NULL) {
        CreateColors(mw);
        for (i = 0; i < mw->matrix.rows; i++)
            for (j = 0; j < position; j++)
                mw->matrix.colors[i][j] = mw->manager.foreground;
        for (i = 0; i < mw->matrix.rows; i++)
            for (j = position + num_colors; j < mw->matrix.columns; j++)
                mw->matrix.colors[i][j] = mw->manager.foreground;
    }

    /*
     * Set each column to the appropriate color
     */
    for (i = 0; i < mw->matrix.rows; i++)
        for (j = 0; j < num_colors; j++)
            mw->matrix.colors[i][j + position] = colors[j];

    if (XtIsRealized(mw)) {
        /*
         * Redraw all the visible non-fixed cells. We don't need to clear first
         * since only the color changed.
         */
        SETRECT(rect,
                0, 0,
                ClipChild(mw)->core.width - 1, ClipChild(mw)->core.height - 1);
        RedrawCells(mw, &rect);

        /*
         * Redraw all the visible fixed cells (but not the labels).
         * We don't need to clear first since only the color changed.
         */
        SETRECT(rect,
                ROW_LABEL_WIDTH(mw), COLUMN_LABEL_HEIGHT(mw),
                mw->core.width - 1, mw->core.height - 1);
        RedrawLabelsAndFixed(mw, &rect);
    }
}

/*
 * Public interface to set_column_colors method
 */
void
XbaeMatrixSetColumnColors(w, position, colors, num_colors)
Widget w;
int position;
Pixel *colors;
int num_colors;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the set_column_colors method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.set_column_colors)
        ((XbaeMatrixWidget)w, position, colors, num_colors);
}

/*
 * Matrix set_cell_color method
 */
static void
SetCellColor(mw, row, column, color)
XbaeMatrixWidget mw;
int row;
int column;
Pixel color;
{
    int i, j;

    /*
     * Do some error checking.
     */
    if (row >= mw->matrix.rows || row < 0 ||
        column >= mw->matrix.columns || column < 0) {
        XtAppWarningMsg(XtWidgetToApplicationContext((Widget)mw),
                        "setCellColor", "badIndex", "XbaeMatrix",
        "XbaeMatrix: Row or column parameter out of bounds for SetCellColor.",
                        NULL, 0);
        return;
    }

    /*
     * If we don't have any colors yet, malloc them and initialize them
     */
    if (mw->matrix.colors == NULL) {
        CreateColors(mw);
        for (i = 0; i < mw->matrix.rows; i++)
            for (j = 0; j < mw->matrix.columns; j++)
                mw->matrix.colors[i][j] = mw->manager.foreground;
    }

    /*
     * Set our cells color
     */
    mw->matrix.colors[row][column] = color;

    if (XtIsRealized(mw)) {
        /*
         * Redraw the cell if it is visible
         */
        if (IsCellVisible(mw, row, column))
            DrawCell(mw, row, column);
    }
}

/*
 * Public interface to set_cell_color method
 */
void
XbaeMatrixSetCellColor(w, row, column, color)
Widget w;
int row;
int column;
Pixel color;
{
    /*
     * Make sure w is a Matrix or a subclass
     */
    XtCheckSubclass(w, xbaeMatrixWidgetClass, NULL);

    /*
     * Call the set_cell_color method
     */
    (*((XbaeMatrixWidgetClass) XtClass(w))->matrix_class.set_cell_color)
        ((XbaeMatrixWidget)w, row, column, color);
}


/*
 * Action to edit a non-fixed cell.
 */
/* ARGSUSED */
static void
EditCellACT(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;
Cardinal *nparams;
{
    XbaeMatrixWidget mw;
    int row, column;
    XrmQuark q;
    static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
    static Boolean haveQuarks = False;

    /*
     * Get static quarks for the parms we understand
     */
    if (!haveQuarks) {
        QPointer = XrmPermStringToQuark("Pointer");
        QLeft = XrmPermStringToQuark("Left");
        QRight = XrmPermStringToQuark("Right");
        QUp = XrmPermStringToQuark("Up");
        QDown = XrmPermStringToQuark("Down");
        haveQuarks = True;
    }

    /*
     * Get Matrix widget and make sure it is a Matrix subclass.
     * w could be Matrix, or the Clip or textField children of Matrix
     */
    if (XtIsSubclass(w, xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) w;
    else if (XtIsSubclass(XtParent(w), xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) XtParent(w);
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "editCellACT", "badWidget", "XbaeMatrix",
                        "XbaeMatrix: Bad widget passed to EditCell action",
                        NULL, 0);
        return;
    }

    /*
     * Make sure we have a single parm
     */
    if (*nparams != 1) {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "editCellACT", "badParms", "XbaeMatrix",
  "XbaeMatrix: Wrong number of parameters passed to EditCell action, needs 1",
                        NULL, 0);
        return;
    }

    /*
     * Initialize row/column to the current position
     */
    row = mw->matrix.current_row;
    column = mw->matrix.current_column;

    /*
     * Quarkify the string param
     */
    q = XrmStringToQuark(params[0]);

    /*
     * If we aren't currently editing, then the only kind of traversal that
     * makes sense is pointer.
     */
    if (!XtIsManaged(TextChild(mw)) && q != QPointer)
        return;

    if (q == QPointer) {
        int x, y;

        /*
         * Get the x,y point coordinate relative to the Clip window.
         * Return if this event did not occur in the Clip subwindow
         * (since we can only edit non-fixed cells).
         */
        switch (event->type) {
        case ButtonPress:
        case ButtonRelease:
            x = event->xbutton.x;
            y = event->xbutton.y;
            break;
        case KeyPress:
        case KeyRelease:
            x = event->xkey.x;
            y = event->xkey.y;
            break;
        case MotionNotify:
            x = event->xmotion.x;
            y = event->xmotion.y;
            break;
        default:
            return;
        }

        if (event->xbutton.subwindow == XtWindow(ClipChild(mw))) {
            x -= FIXED_COLUMN_LABEL_OFFSET(mw);
            y -=  FIXED_ROW_LABEL_OFFSET(mw);
        }
        else if (event->xbutton.window != XtWindow(ClipChild(mw)))
            return;

        /*
         * Convert the point to a row,column. If it does not pick a valid
         * cell, then return.
         */
        if (!XYToRowCol(mw, &x, &y, &row, &column, NonFixedCell))
            return;
    }
    else if (q == QRight) {
        /*
         * If we are in the lower right corner, stay there.
         * Otherwise move over a column, if we move off the right edge,
         * then move down a row and back to the first non-fixed column.
         */
        if (mw->matrix.current_row != mw->matrix.rows - 1 ||
            mw->matrix.current_column != mw->matrix.columns -1) {

            column++;

            if (column >= (int)mw->matrix.columns) {
                column = mw->matrix.fixed_columns;
                row++;
            }
        }
    }
    else if (q == QLeft) {
        /*
         * If we are in the upper left corner, stay there.
         * Otherwise move back a column, if we move before the fixed columns,
         * then move up a row and over to the last column.
         */
        if (mw->matrix.current_row != mw->matrix.fixed_rows ||
            mw->matrix.current_column !=
            mw->matrix.fixed_columns) {

            column--;

            if (column < (int)mw->matrix.fixed_columns) {
                column = mw->matrix.columns - 1;
                row--;
            }
        }
    }
    else if (q == QDown) {
        row++;

        if (row >= (int)mw->matrix.rows)
            row = mw->matrix.fixed_rows;
    }
    else if (q == QUp) {
        row--;

        if (row < (int)mw->matrix.fixed_rows)
            row = mw->matrix.rows - 1;
    }

    /*
     * Call the traverseCellCallback to allow the application to
     * perform custom traversal.
     */
    if (mw->matrix.traverse_cell_callback) {
        XbaeMatrixTraverseCellCallbackStruct call_data;

        call_data.reason = XbaeTraverseCellReason;
        call_data.row = mw->matrix.current_row;
        call_data.column = mw->matrix.current_column;
        call_data.next_row = row;
        call_data.next_column = column;
        call_data.fixed_rows = mw->matrix.fixed_rows;
        call_data.fixed_columns = mw->matrix.fixed_columns;
        call_data.num_rows = mw->matrix.rows;
        call_data.num_columns = mw->matrix.columns;
        call_data.param = params[0];
        call_data.qparam = q;

        XtCallCallbackList((Widget)mw, mw->matrix.traverse_cell_callback,
                           (XtPointer) &call_data);

        row = call_data.next_row;
        column = call_data.next_column;
    }

    /*
     * Attempt to edit the new cell using the edit_cell method.
     * If we are editing a cell based on pointer position, we always
     * call edit_cell.  Otherwise, we must be editing a new cell to
     * call edit_cell.
     */
    if (q == QPointer || (row != mw->matrix.current_row ||
                          column != mw->matrix.current_column))
        (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.edit_cell)
            (mw, row, column);

    /*
     * Traverse to the textField
     */
    XmProcessTraversal(TextChild(mw), XmTRAVERSE_CURRENT);
}

/*
 * Action to unmap the textField and discard any edits made
 */
/* ARGSUSED */
static void
CancelEditACT(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;
Cardinal *nparams;
{
    XbaeMatrixWidget mw;
    Boolean unmap;

    /*
     * Get Matrix widget and make sure it is a Matrix subclass.
     * w could be Matrix, or the Clip or textField children of Matrix
     */
    if (XtIsSubclass(w, xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) w;
    else if (XtIsSubclass(XtParent(w), xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) XtParent(w);
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "cancelEditACT", "badWidget", "XbaeMatrix",
                        "XbaeMatrix: Bad widget passed to CancelEdit action",
                        NULL, 0);
        return;
    }

    /*
     * Make sure we have a single param
     */
    if (*nparams != 1) {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "cancelEditACT", "badParms", "XbaeMatrix",
"XbaeMatrix: Wrong number of parameters passed to CancelEdit action, needs 1",
                        NULL, 0);
        return;
    }

    /*
     * Validate our param
     */
    if (!strcmp(params[0], "True"))
        unmap = True;
    else if (!strcmp(params[0], "False"))
        unmap = False;
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "cancelEditACT", "badParm", "XbaeMatrix",
"XbaeMatrix: Invalid parameter passed to CancelEdit action, must be T or F",
                        NULL, 0);
        return;
    }
    /*
     * Call the cancel_edit method
     */
    (*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.cancel_edit)
        (mw, unmap);
}

/*
 * Action save any edits made and unmap the textField if params[0] is True
 */
/* ARGSUSED */
static void
CommitEditACT(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;
Cardinal *nparams;
{
    XbaeMatrixWidget mw;
    Boolean unmap;

    /*
     * Get Matrix widget and make sure it is a Matrix subclass.
     * w could be Matrix, or the Clip or textField children of Matrix
     */
    if (XtIsSubclass(w, xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) w;
    else if (XtIsSubclass(XtParent(w), xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) XtParent(w);
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "commitEditACT", "badWidget", "XbaeMatrix",
                        "XbaeMatrix: Bad widget passed to CommitEdit action",
                        NULL, 0);
        return;
    }

    /*
     * Make sure we have a single param
     */
    if (*nparams != 1) {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "commitEditACT", "badParms", "XbaeMatrix",
"XbaeMatrix: Wrong number of parameters passed to CommitEdit action, needs 1",
                        NULL, 0);
        return;
    }

    /*
     * Validate our param
     */
    if (!strcmp(params[0], "True"))
        unmap = True;
    else if (!strcmp(params[0], "False"))
        unmap = False;
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "commitEditACT", "badParm", "XbaeMatrix",
"XbaeMatrix: Invalid parameter passed to CommitEdit action, must be T or F",
                        NULL, 0);
        return;
    }

    (void)(*((XbaeMatrixWidgetClass) XtClass(mw))->matrix_class.commit_edit)
        (mw, unmap);
}

/*
 * Convert the coordinates in an event to be relative to the Clip
 * window or the Matrix window.  Set the cell to indicate which one.
 * Used by some actions.
 */
/* ARGSUSED */
static Boolean
EventToXY(mw, event, x, y, cell)
XbaeMatrixWidget mw;
XEvent *event;
int *x, *y;
CellType *cell;
{
    switch (event->type) {
    case ButtonPress:
    case ButtonRelease:
        *x = event->xbutton.x;
        *y = event->xbutton.y;
        break;
    case KeyPress:
    case KeyRelease:
        *x = event->xkey.x;
        *y = event->xkey.y;
        break;
    case MotionNotify:
        *x = event->xmotion.x;
        *y = event->xmotion.y;
        break;
    default:
        return False;
    }

    if (event->xbutton.subwindow == XtWindow(ClipChild(mw))) {
        *cell = NonFixedCell;
        *x -= FIXED_COLUMN_LABEL_OFFSET(mw);
        *y -= FIXED_ROW_LABEL_OFFSET(mw);
    }
    else if (event->xbutton.window == XtWindow(mw))
        *cell = FixedCell;
    else if (event->xbutton.window == XtWindow(ClipChild(mw)))
        *cell = NonFixedCell;
    else if (event->xbutton.window == XtWindow(TextChild(mw))) {
        Position tx, ty;
        *cell = NonFixedCell;
        XtVaGetValues(TextChild(mw),
                      XmNx, &tx,
                      XmNy, &ty,
                      NULL);
        *x += tx;
        *y += ty;
    }
    else
        return False;

    return True;
}

/* ARGSUSED */
static void
SelectCellACT(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;
Cardinal *nparams;
{
    XbaeMatrixWidget mw;
    int x, y;
    int row, column;
    CellType cell;
    XbaeMatrixSelectCellCallbackStruct call_data;

    /*
     * Get Matrix widget and make sure it is a Matrix subclass.
     * w could be Matrix, or the Clip or textField children of Matrix
     */
    if (XtIsSubclass(w, xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) w;
    else if (XtIsSubclass(XtParent(w), xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) XtParent(w);
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "selectCellACT", "badWidget", "XbaeMatrix",
                        "XbaeMatrix: Bad widget passed to SelectCell action",
                        NULL, 0);
        return;
    }

    /*
     * If we don't have a selectCellCallback, then return now
     */
    if (!mw->matrix.select_cell_callback)
        return;

    if (!EventToXY(mw, event, &x, &y, &cell))
        return;

    /*
     * Convert the point to a row,column. If it does not pick a valid
     * cell, then return.
     */
    if (!XYToRowCol(mw, &x, &y, &row, &column, cell))
        return;

    /*
     * Call our select_cell callbacks
     */
    call_data.reason = XbaeSelectCellReason;
    call_data.row = row;
    call_data.column = column;
    call_data.selected_cells = mw->matrix.selected_cells;
    call_data.cells = mw->matrix.cells;
    call_data.num_params = *nparams;
    call_data.params = params;
    call_data.event = event;

    XtCallCallbackList((Widget)mw, mw->matrix.select_cell_callback,
                       (XtPointer) &call_data);
}


/* ARGSUSED */
static void
TraverseNextACT(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;
Cardinal *nparams;
{
    XbaeMatrixWidget mw;

    /*
     * Get Matrix widget and make sure it is a Matrix subclass.
     * w should be the textField widget.
     */
    if (XtIsSubclass(XtParent(w), xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) XtParent(w);
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "traverseNextACT", "badWidget", "XbaeMatrix",
                        "XbaeMatrix: Bad widget passed to TraverseNext action",
                        NULL, 0);
        return;
    }

    /*
     * Set the traversing direction flag.  XmProcessTraversal may traverse
     * to the Clip widget. If it does, then we will see this flag in
     * the Clip focusCallback, TraverseInCB, and we will continue to traverse
     * on out of the mw.  yuck!
     */
    mw->matrix.traversing = XmTRAVERSE_NEXT_TAB_GROUP;
    XmProcessTraversal(TextChild(mw), XmTRAVERSE_NEXT_TAB_GROUP);
    mw->matrix.traversing = NOT_TRAVERSING;
}

/* ARGSUSED */
static void
TraversePrevACT(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;
Cardinal *nparams;
{
    XbaeMatrixWidget mw;

    /*
     * Get Matrix widget and make sure it is a Matrix subclass.
     * w should be the textField widget.
     */
    if (XtIsSubclass(XtParent(w), xbaeMatrixWidgetClass))
        mw = (XbaeMatrixWidget) XtParent(w);
    else {
        XtAppWarningMsg(XtWidgetToApplicationContext(w),
                        "traversePrevACT", "badWidget", "XbaeMatrix",
                        "XbaeMatrix: Bad widget passed to TraversePrev action",
                        NULL, 0);
        return;
    }

    /*
     * Set the traversing direction flag.  XmProcessTraversal may traverse
     * to the Clip widget. If it does, then we will see this flag in
     * the Clip focusCallback, TraverseInCB, and we will continue to traverse
     * on out of the mw.  yuck!
     */
    mw->matrix.traversing = XmTRAVERSE_PREV_TAB_GROUP;
    XmProcessTraversal(TextChild(mw), XmTRAVERSE_PREV_TAB_GROUP);
    mw->matrix.traversing = NOT_TRAVERSING;
}

/*
 * Convert a comma separated list of strings to a NULL terminated array
 * of substrings.  Handles escaped commas (\,) and escaped escaped commas (\\,)
 * - A comma (,) terminates a string
 * - A backslash-comma (\,) does not terminate a string and is copied as
 *   a comma (,)
 * - A backslash-backslash-comma (\\,) does not terminate a string and is
 *   copied as a backslash-comma (\,)
 */
/* ARGSUSED */
Boolean
CvtStringToStringArray(dpy, args, num_args, from, to, data)
Display *dpy;
XrmValuePtr args;
Cardinal *num_args;
XrmValuePtr from, to;
XtPointer *data;
{
    static String *array;
    String start = from->addr;

    if (*num_args != 0)
        XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
                        "cvtStringToStringArray", "wrongParameters",
                        "XbaeMatrix",
                "String to StringArray conversion needs no extra arguments",
                        NULL, NULL);

    /*
     * User didn't provide enough space
     */
    if (to->addr != NULL && to->size < sizeof(String *)) {
        to->size = sizeof(String *);
        return False;
    }

    if (start == NULL || *start == '\0')
        array = NULL;
    else {
        char *ch, *next, *a;
        int i, sub, len, count;

        /*
         * Count the substrings
         */
        for (ch = start, count = 1; *ch != '\0'; ch++) {

            /*
             * We hit a backslash
             */
            if (*ch == '\\') {
                /*
                 * Take care of backslash-backslash-comma
                 */
                if (*(ch+1) == '\\' && *(ch+2) == ',')
                    ch += 2;

                /*
                 * Take care of backslash-comma
                 */
                else if (*(ch+1) == ',')
                    ch++;
            }
            /*
             * We hit an unescaped comma
             */
            else if (*ch == ',')
                count++;
        }

        /*
         * Malloc the array, make it one bigger for a terminating NULL entry
         */
        array = (String *) XtMalloc((count + 1) * sizeof(String));
        array[count] = NULL;

        for (sub = 0; sub < count; sub++) {

            /*
             * Skip leading white space
             */
            while (isspace((unsigned char)*start))
                start++;

            /*
             * Count the number of chars in this substring.
             * backslash-comma counts as one and does not terminate.
             * backslash-backslash-comma counts as two and does not terminate.
             */
            for (ch = start, len = 0; *ch != '\0' && *ch != ','; ch++) {
                /*
                 * We hit a backslash
                 */
                if (*ch == '\\') {
                    /*
                     * Take care of backslash-backslash-comma
                     */
                    if (*(ch+1) == '\\' && *(ch+2) == ',') {
                        len += 2;
                        ch += 2;
                    }
                    /*
                     * Take care of backslash-comma
                     */
                    else if (*(ch+1) == ',') {
                        len++;
                        ch++;
                    }
                    else
                        len++;
                }
                else
                    len++;
            }

            /*
             * Save the beginning of the next substring
             */
            next = ch + 1;

            /*
             * Back up over trailing white space if we moved at all
             */
            if (ch != start)
                while (isspace((unsigned char)*(--ch)))
                    len--;

            /*
             * Malloc a String of the correct size
             */
            array[sub] = (String) XtMalloc(len + 1);

            /*
             * Copy the substring into our new string.
             * backslash-comma gets copied as comma.
             * backslash-backslash-comma gets copied as backslash-comma.
             */
            for (i = 0, ch = start, a = array[sub];
                 i < len;
                 i++, ch++) {

                /*
                 * We hit a backslash
                 */
                if (*ch == '\\') {
                    /*
                     * Take care of backslash-backslash-comma
                     */
                    if (*(ch+1) == '\\' && *(ch+2) == ',') {
                        *(a++) = '\\';
                        *(a++) = ',';
                        i++;
                        ch += 2;
                    }
                    /*
                     * Take care of backslash-comma
                     */
                    else if (*(ch+1) == ',') {
                        *(a++) = ',';
                        ch++;
                    }
                    else
                        *(a++) = *ch;
                }
                else
                    *(a++) = *ch;
            }
            *a = '\0';

            /*
             * Point to the beginning of the next string.
             */
            start = next;
        }
    }

    if (to->addr == NULL)
        to->addr = (caddr_t) &array;
    else
        *(String **) to->addr = array;
    to->size = sizeof(String *);

    return True;
}

/*
 * Free the string array allocated by the String to StringArray converter
 */
/* ARGSUSED */
void
StringArrayDestructor(app, to, converter_data, args, num_args)
XtAppContext app;
XrmValuePtr to;
XtPointer converter_data;
XrmValuePtr args;
Cardinal *num_args;
{
    String *array = *(String **) to->addr;
    String *entry;

    if (array == NULL)
        return;

    for (entry = array; *entry != NULL; entry++)
        XtFree((XtPointer) *entry);

    XtFree((XtPointer) array);
}

/*
 * Convert a comma separated list of short ints to array of widths.
 * The array is terminated with BAD_WIDTH.
 */
/* ARGSUSED */
Boolean
CvtStringToWidthArray(dpy, args, num_args, from, to, data)
Display *dpy;
XrmValuePtr args;
Cardinal *num_args;
XrmValuePtr from, to;
XtPointer *data;
{
    static short *array;
    String start = from->addr;
    char *ch;
    int i, count;

    if (*num_args != 0)
        XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
                        "cvtStringToWidthArray", "wrongParameters",
                        "XbaeMatrix",
                "String to WidthArray conversion needs no extra arguments",
                        NULL, NULL);

    /*
     * User didn't provide enough space
     */
    if (to->addr != NULL && to->size < sizeof(short *)) {
        to->size = sizeof(short *);
        return False;
    }

    if (start == NULL || *start == '\0')
        array = NULL;

    else {

        /*
         * Count the comma separated shorts
         */
        for (ch = start, count = 1; *ch != '\0'; ch++)
            if (*ch == ',')
                count++;

        /*
         * Malloc the array
         */
        array = (short *) XtMalloc((count + 1) * sizeof(short));
        array[count] = BAD_WIDTH;

        for (i = 0; i < count; i++) {

            array[i] = (short) atoi(start);

            /*
             * Find the comma at the end of this short
             */
            /* EMPTY */
            for (; *start != '\0' && *start != ','; start++);
            start++;
        }
    }

    if (to->addr == NULL)
        to->addr = (caddr_t) &array;
    else
        *(short **) to->addr = array;
    to->size = sizeof(short *);

    return True;
}

/*
 * Free the width array allocated by the String to WidthArray converter
 */
/* ARGSUSED */
void
WidthArrayDestructor(app, to, converter_data, args, num_args)
XtAppContext app;
XrmValuePtr to;
XtPointer converter_data;
XrmValuePtr args;
Cardinal *num_args;
{
    short *array = *(short **) to->addr;

    XtFree((XtPointer) array);
}

/*
 * Convert a comma separated list of ints to array of max lengths.
 * The array is terminated with BAD_MAXLENGTH.
 */
/* ARGSUSED */
Boolean
CvtStringToMaxLengthArray(dpy, args, num_args, from, to, data)
Display *dpy;
XrmValuePtr args;
Cardinal *num_args;
XrmValuePtr from, to;
XtPointer *data;
{
    static int *array;
    String start = from->addr;
    char *ch;
    int i, count;

    if (*num_args != 0)
        XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
                        "cvtStringToMaxLengthArray", "wrongParameters",
                        "XbaeMatrix",
                "String to MaxLengthArray conversion needs no extra arguments",
                        NULL, NULL);

    /*
     * User didn't provide enough space
     */
    if (to->addr != NULL && to->size < sizeof(int *)) {
        to->size = sizeof(int *);
        return False;
    }

    if (start == NULL || *start == '\0')
        array = NULL;

    else {

        /*
         * Count the comma separated ints
         */
        for (ch = start, count = 1; *ch != '\0'; ch++)
            if (*ch == ',')
                count++;

        /*
         * Malloc the array
         */
        array = (int *) XtMalloc((count + 1) * sizeof(int));
        array[count] = BAD_MAXLENGTH;

        for (i = 0; i < count; i++) {

            array[i] = (int) atoi(start);

            /*
             * Find the comma at the end of this int
             */
            /* EMPTY */
            for (; *start != '\0' && *start != ','; start++);
            start++;
        }
    }

    if (to->addr == NULL)
        to->addr = (caddr_t) &array;
    else
        *(int **) to->addr = array;
    to->size = sizeof(int *);

    return True;
}

/*
 * Free the max length array allocated by the String to
 * MaxLengthArray converter
 */
/* ARGSUSED */
void
MaxLengthArrayDestructor(app, to, converter_data, args, num_args)
XtAppContext app;
XrmValuePtr to;
XtPointer converter_data;
XrmValuePtr args;
Cardinal *num_args;
{
    int *array = *(int **) to->addr;

    XtFree((XtPointer) array);
}

/*
 * Compare two strings up to length chars, and return True if they are equal.
 * Handles Xm prefix too. The string test must be lower case.
 * Used by StringToAlignmentArray converter.
 */
static Boolean
StringsAreEqual(in, test, length)
String in;
String test;
int length;
{
    int i;

    if ((in[0] == 'X' || in[0] == 'x') &&
        (in[1] == 'M' || in[1] == 'm'))
        in +=2;

    for (i = 0; i < length; i++) {
        char c = *in;

        if (isupper(c))
            c = tolower(c);

        if (c != test[i])
            return False;

        in++;
    }

    /*
     * String in may have trailing garbage, but as long as the first
     * length chars matched, we return True
     */
    return True;
}

/*
 * Convert a comma separated list of alignments to array of alignments
 * (unsigned chars).  The array is terminated by BAD_ALIGNMENT.
 */
/* ARGSUSED */
Boolean
CvtStringToAlignmentArray(dpy, args, num_args, from, to, data)
Display *dpy;
XrmValuePtr args;
Cardinal *num_args;
XrmValuePtr from, to;
XtPointer *data;
{
    static unsigned char *array;
    String start = from->addr;
    char *ch;
    int i, count;

    if (*num_args != 0)
        XtAppWarningMsg(XtDisplayToApplicationContext(dpy),
                        "cvtStringToAlignmentArray", "wrongParameters",
                        "XbaeMatrix",
                "String to AlignmentArray conversion needs no extra arguments",
                        NULL, NULL);

    /*
     * User didn't provide enough space
     */
    if (to->addr != NULL && to->size < sizeof(unsigned char *)) {
        to->size = sizeof(unsigned char *);
        return False;
    }

    if (start == NULL || *start == '\0')
        array = NULL;

    else {

        /*
         * Count the comma separated alignments
         */
        for (ch = start, count = 1; *ch != '\0'; ch++)
            if (*ch == ',')
                count++;

        /*
         * Malloc the array
         */
        array = (unsigned char *)XtMalloc((count + 1) * sizeof(unsigned char));
        array[count] = BAD_ALIGNMENT;

        /*
         * Compare each substring to the alignment strings.
         * If we find a bad one, display a warning and fail.
         * We should be able to use XtCallConverter on _XmCvtStringToAlignment,
         * but that function is static so we have to duplicate its
         * functionality.
         */
        for (i = 0; i < count; i++) {

            /*
             * Skip leading white space
             */
            while (isspace(*start))
                start++;

            if (StringsAreEqual(start, "alignment_beginning", 19))
                array[i] = XmALIGNMENT_BEGINNING;
            else if (StringsAreEqual(start, "alignment_center", 16))
                array[i] = XmALIGNMENT_CENTER;
            else if (StringsAreEqual(start, "alignment_end", 13))
                array[i] = XmALIGNMENT_END;
            else {
                XtDisplayStringConversionWarning(dpy, from->addr,
                                                 XmRAlignmentArray);
                XtFree((char *)array);
                return False;
            }

            /*
             * Find the comma at the end of this alignment
             */
            /* EMPTY */
            for (; *start != '\0' && *start != ','; start++);
            start++;
        }
    }

    if (to->addr == NULL)
        to->addr = (caddr_t) &array;
    else
        *(unsigned char **) to->addr = array;
    to->size = sizeof(unsigned char *);

    return True;
}

/*
 * Free the alignment array allocated by the String to AlignmentArray converter
 */
/* ARGSUSED */
void
AlignmentArrayDestructor(app, to, converter_data, args, num_args)
XtAppContext app;
XrmValuePtr to;
XtPointer converter_data;
XrmValuePtr args;
Cardinal *num_args;
{
    unsigned char *array = *(unsigned char **) to->addr;

    XtFree((XtPointer) array);
}


/*
 * ScrollMgr implementation.
 * When we scroll using XCopyArea, occluding windows will cause GraphicsExpose
 * events to be generated, if there are no occluding windows then NoExpose
 * events will be generated. The removal of occluding windows will cause Expose
 * events.  If a number of scrolls (XCopyAreas) occur in quick succession,
 * the events will contain obsolete x/y information since our internal
 * coordinates have been scrolled to a new location.  The ScrollMgr
 * keeps track of scrolls and offsets required to relocate the events to the
 * current coordinate system.
 *
 * The Matrix widget has two ScrollMgrs, one for the Matrixs window
 * and one for the Clip widgets window.
 *
 * Each widgets compress_exposures field should be XtExposeCompressSeries
 * or XtExposeNoCompress.
 *
 * The idea behind this code is based on the PanHandler posted by Chuck Ocheret
 * (chuck@fid.morgan.com)
 */

/*
 * Create and initialize a ScrollMgr
 */
static SmScrollMgr
SmCreateScrollMgr()
{
    SmScrollMgr scrollMgr = XtNew(SmScrollMgrRec);

    scrollMgr->offset_x = 0;
    scrollMgr->offset_y = 0;
    scrollMgr->scroll_count = 0;
    scrollMgr->scroll_queue = NULL;
    scrollMgr->scrolling = False;

    return scrollMgr;
}

/*
 * Destroy a ScrollMgr, including any queued scrolls
 */
static void
SmDestroyScrollMgr(scrollMgr)
SmScrollMgr scrollMgr;
{
    if (scrollMgr->scroll_queue) {
        SmScrollNode node = scrollMgr->scroll_queue->next;

        while (node != scrollMgr->scroll_queue) {
            SmScrollNode d = node;
            node = node->next;
            XtFree((XtPointer)d);
        }
        XtFree((XtPointer) node);
    }

    XtFree((XtPointer)scrollMgr);
}

/*
 * Record a new scroll request in the ScrollMgr
 */
static void
SmAddScroll(scrollMgr, delta_x, delta_y)
SmScrollMgr scrollMgr;
int delta_x;
int delta_y;
{
    SmScrollNode node = XtNew(SmScrollNodeRec);

    node->x = delta_x;
    node->y = delta_y;

    scrollMgr->offset_x += delta_x;
    scrollMgr->offset_y += delta_y;
    scrollMgr->scroll_count++;

    /*
     * Insert the node at the end of the queue
     */
    if (!scrollMgr->scroll_queue) {
        scrollMgr->scroll_queue = node;
        node->next = node;
        node->prev = node;
    }
    else {
        SmScrollNode last = scrollMgr->scroll_queue->prev;

        last->next = node;
        node->next = scrollMgr->scroll_queue;
        node->prev = last;
        scrollMgr->scroll_queue->prev = node;
    }
}

/*
 * Remove a scroll from the ScrollMgr queue
 */
static void
SmRemoveScroll(scrollMgr)
SmScrollMgr scrollMgr;
{
    if (scrollMgr->scroll_count) {
        SmScrollNode node = scrollMgr->scroll_queue;

        scrollMgr->offset_x -= node->x;
        scrollMgr->offset_y -= node->y;

        /*
         * Remove node from head of queue
         */
        if (node->next == node)
            scrollMgr->scroll_queue = NULL;
        else {
            scrollMgr->scroll_queue = node->next;
            node->next->prev = node->prev;
            node->prev->next = node->next;
        }
        XtFree((XtPointer)node);

        scrollMgr->scroll_count--;
    }
}

/*
 * Handle an expose event
 */
static void
SmScrollEvent(scrollMgr, event)
SmScrollMgr scrollMgr;
XEvent *event;
{
    switch (event->type) {

    case Expose:

        /*
         * Normal Expose event, translate it into our scrolled
         * coordinate system.
         */
        event->xexpose.x += scrollMgr->offset_x;
        event->xexpose.y += scrollMgr->offset_y;
        break;

    case GraphicsExpose:

        /*
         * If we are not scrolling, then this must be the first
         * GraphicsExpose event.  Remove the corresponding scroll from the
         * queue, and if we have more GraphicsExposes to come, set scrolling
         * to True.
         */
        if (scrollMgr->scrolling == False) {
            SmRemoveScroll(scrollMgr);
            if (event->xgraphicsexpose.count != 0)
                scrollMgr->scrolling = True;
        }

        /*
         * This is the last GraphicsExpose so set scrolling to False.
         */
        else if (event->xgraphicsexpose.count == 0)
            scrollMgr->scrolling = False;

        /*
         * Translate the event into our scrolled coordinate system.
         */
        event->xgraphicsexpose.x += scrollMgr->offset_x;
        event->xgraphicsexpose.y += scrollMgr->offset_y;
        break;

    case NoExpose:

        /*
         * A NoExpose event means we won't be getting any GraphicsExpose
         * events, so remove the scroll from the queue and set scrolling
         * to False.
         */
        SmRemoveScroll(scrollMgr);
        scrollMgr->scrolling = False;
        break;

    default:
        break;
    }
}

