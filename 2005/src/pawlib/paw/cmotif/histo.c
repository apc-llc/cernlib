/*
 * $Id: histo.c,v 1.1.1.1 1996/03/01 11:38:56 mclareni Exp $
 *
 * $Log: histo.c,v $
 * Revision 1.1.1.1  1996/03/01 11:38:56  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.06/03 24/11/94  15.37.28  by  O.Couet*/
/*-- Author :*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>

#include "hmotif/pawm.h"
#include "hmotif/axis.h"
#include "hmotif/style.h"

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

#define MAX_X_ZONE        5
#define MAX_Y_ZONE        5

/* histo style panel icon */

#define histo_width 50
#define histo_height 50
static char histo_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00, 0x30, 0x00, 0x00,
   0x00, 0x00, 0xfc, 0x00, 0x30, 0x00, 0x00, 0x00, 0x00, 0x86, 0x01, 0x30,
   0x00, 0x00, 0x00, 0x00, 0x86, 0x01, 0x30, 0x00, 0x00, 0x00, 0x00, 0x86,
   0x01, 0x30, 0x00, 0x00, 0x00, 0x10, 0x86, 0x01, 0x30, 0x00, 0x00, 0x80,
   0x19, 0xc6, 0x01, 0x30, 0x00, 0x00, 0xc0, 0x0b, 0xcc, 0x01, 0x30, 0x00,
   0x00, 0x40, 0x0f, 0xfc, 0x01, 0x30, 0x00, 0x00, 0x00, 0x07, 0xf8, 0x00,
   0x30, 0x00, 0x00, 0x00, 0x06, 0x60, 0x00, 0x30, 0x00, 0x02, 0x00, 0x0e,
   0x00, 0x00, 0x30, 0x00, 0x02, 0x1e, 0x1c, 0x00, 0x00, 0x30, 0x00, 0x00,
   0x2e, 0x1c, 0x00, 0x00, 0xf0, 0x03, 0x07, 0x06, 0x38, 0x00, 0x00, 0xf0,
   0x07, 0x06, 0x06, 0x38, 0x02, 0x00, 0x30, 0x06, 0x06, 0x0e, 0x78, 0x04,
   0x00, 0x30, 0x06, 0x06, 0xfc, 0x70, 0x06, 0x00, 0x30, 0x06, 0x0e, 0xc0,
   0xf1, 0x06, 0x00, 0x30, 0x06, 0x0e, 0x80, 0xe1, 0x07, 0x00, 0x30, 0x06,
   0x0e, 0x84, 0x81, 0x03, 0x00, 0x30, 0x06, 0x1e, 0xfe, 0x00, 0x00, 0x00,
   0x18, 0x06, 0x3c, 0xf8, 0x00, 0x00, 0x01, 0x18, 0x0e, 0x00, 0x00, 0x00,
   0x80, 0x01, 0x00, 0x3c, 0x00, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x70, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00,
   0xf8, 0x80, 0x01, 0x00, 0x78, 0x00, 0x00, 0xff, 0xe7, 0xf1, 0x00, 0x38,
   0x00, 0x00, 0x07, 0xe7, 0xe3, 0x01, 0x38, 0x00, 0x00, 0x03, 0x67, 0x83,
   0x01, 0x38, 0x00, 0x00, 0x03, 0x67, 0x83, 0x01, 0x38, 0x00, 0x00, 0x83,
   0x67, 0x03, 0x01, 0x38, 0x00, 0x00, 0x83, 0x77, 0x07, 0x03, 0x38, 0x00,
   0x00, 0x87, 0x71, 0x06, 0x03, 0x38, 0x00, 0x00, 0xef, 0x70, 0x06, 0x73,
   0x38, 0x00, 0x00, 0x7f, 0x38, 0x06, 0x77, 0x38, 0x00, 0x00, 0x03, 0x38,
   0x0c, 0xf6, 0x38, 0x00, 0x00, 0x03, 0x3c, 0x0e, 0xf6, 0x1c, 0x00, 0x00,
   0x03, 0xfc, 0x1f, 0xbe, 0x0f, 0x00, 0x00, 0x03, 0xfe, 0x1d, 0xbc, 0x07,
   0x00, 0x30, 0x07, 0x18, 0x38, 0x9c, 0x07, 0x00, 0xf0, 0x07, 0x18, 0x78,
   0x1c, 0x03, 0x00, 0xe0, 0x07, 0x1c, 0x70, 0x18, 0x03, 0x00, 0xc0, 0x0f,
   0x1e, 0xf0, 0x18, 0x02, 0x00, 0x00, 0x1e, 0x1e, 0xf0, 0x11, 0x00, 0x00,
   0x00, 0x1c, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};

/* extern functions */
extern Widget create_histoStyle(void);
extern Widget create_statFormDialog(void);
extern Widget create_fitFormDialog(void);
extern Widget create_axisFormDialog(void);
extern Widget create_rangeFormDialog(void);
extern Widget create_angleFormDialog(void);
extern Widget create_zoneFormDialog(void);
extern Widget create_fileFormDialog(void);
extern Widget create_dateFormDialog(void);
extern void save_style(void);
extern void getang(float *, float *);
extern void cut_apply_sensitivity(Boolean);
extern void unselect_list(void);
extern void unmanage_generalAttributes(void);
extern void unmanage_style_dialogs(void);
extern void unmanage_attributes_dialog(void);
extern void unmanage_fontSettings(void);
extern void unmanage_geometry_dialog(void);
extern void unmanage_axis_dialog(void);

/* extern widgets */
extern Widget CloseHistoPanel;
extern Widget PlotHistoPanel;
extern Widget ResetHistoPanel;
extern Widget ZoneButton;
extern Widget StatToggleButtonGadget;
extern Widget FitToggleButtonGadget;
extern Widget FileToggleButtonGadget;
extern Widget DateToggleButtonGadget;

extern Widget widget_saveAsStyle;
extern Widget widget_openStyle;

extern Widget ClosePushButton;
extern Widget OpenstylePushButton;
extern Widget SavestylePushButton;
extern Widget SavestyleasPushButton;

extern Widget IdToggleButtonGadget;
extern Widget EntriesToggleButtonGadget;
extern Widget MeanToggleButtonGadget;
extern Widget RmsToggleButtonGadget;
extern Widget UnderToggleButtonGadget;
extern Widget OverToggleButtonGadget;
extern Widget AllchanToggleButtonGadget;

extern Widget ChiToggleButtonGadget;
extern Widget ErrorsToggleButtonGadget;
extern Widget ParametersToggleButtonGadget;

extern Widget HoriToggleButtonGadget;
extern Widget VertToggleButtonGadget;
extern Widget BothToggleButtonGadget;

extern Widget widget_file;
extern Widget TLToggleButtonGadget;
extern Widget BLToggleButtonGadget;
extern Widget TRToggleButtonGadget;
extern Widget BRToggleButtonGadget;
extern Widget FileDatePushButtonGadget;

extern Widget widget_date;
extern Widget TLToggleButtonGadget1;
extern Widget BLToggleButtonGadget1;
extern Widget TRToggleButtonGadget1;
extern Widget BRToggleButtonGadget1;
extern Widget FileDatePushButtonGadget1;

extern Widget widget_axis;
extern Widget widget_label;
extern Widget widget_orien;

extern Widget AxisSettings;
extern Widget AxisApplyButton;
extern Widget AxisResetButton;
extern Widget ZAxisToggleButtonGadget;
extern Widget XAxisToggleButtonGadget;
extern Widget YAxisToggleButtonGadget;
extern Widget DivAxisform;
extern Widget AxisPrimaryText;
extern Widget AxisPrimaryScale;
extern Widget AxisSecondaryText;
extern Widget AxisSecondaryScale;
extern Widget AxisTertiaryText;
extern Widget AxisTertiaryScale;
extern Widget labelPushButtonGadget;
extern Widget labelCloseButton;
extern Widget logtoggleButtonGadget;
extern Widget labeltoggleButtonGadget0;
extern Widget gridtoggleButtonGadget;
extern Widget orienPushButtonGadget;
extern Widget closedirPushButton;
extern Widget resetdirPushButton;

extern Widget widget_geometry;
extern Widget widget_fontSettings;
extern Widget widget_color;
extern Widget widget_attributes;
extern Widget widget_generalAttributes;
extern Widget widget_hatchStyle;
extern Widget widget_defineColor;

extern Widget ApplyColorBut;
extern Widget ApplyAttColorBut;
extern Widget ApplyGeometry;
extern Widget ApplyFontSettingsBut;
extern Widget ApplyAttributesBut;
extern Widget ApplyGeneralBut;
extern Widget ApplyHatchBut;
extern Widget ResetGeometry;
extern Widget ResetAttributesBut;
extern Widget ResetGeneralBut;
extern Widget ResetColorButton;
extern Widget ResetHatchBut;
extern Widget ResetAttColorButton;
extern Widget ResetFontSettingsBut;

extern Widget AxisRanges;
extern Widget Angles;
extern Widget GeometryButton;
extern Widget GeneralAttributesBut;
extern Widget FontButton;
extern Widget AttrButton;
extern Widget X1Scale;
extern Widget X2Scale;
extern Widget XBinNumber;
extern Widget LockXToggleButtonGadget;
extern Widget Y1Scale;
extern Widget Y2Scale;
extern Widget YBinNumber;
extern Widget YRangeFrame;
extern Widget LockYToggleButtonGadget;
extern Widget CloseRangeButtonGadget;
extern Widget ResetRangeButtonGadget;
extern Widget ApplyRangeButtonGadget;
extern Widget XScaleRebin;
extern Widget minimumText;
extern Widget maximumText;

extern Widget RefreshToggleButtonGadget;
extern Widget SameToggleButtonGadget;

extern Widget OptionPlot;
extern Widget OptionPlotD;
extern Widget OptionPlotL;
extern Widget OptionPlotC;
extern Widget OptionPlotB;
extern Widget OptionPlotP;
extern Widget OptionPlotSTAR;
extern Widget OptionPlotE;
extern Widget OptionPlotE1;
extern Widget OptionPlotE2;
extern Widget OptionPlotE3;
extern Widget OptionPlotE4;
extern Widget OptionPlotBOX;
extern Widget OptionPlotCOL;
extern Widget OptionPlotSURF;
extern Widget OptionPlotSURF1;
extern Widget OptionPlotSURF2;
extern Widget OptionPlotSURF3;
extern Widget OptionPlotSURF4;
extern Widget OptionPlotLEGO;
extern Widget OptionPlotLEGO1;
extern Widget OptionPlotLEGO2;
extern Widget OptionPlotCONT;
extern Widget OptionPlotCONT3;
extern Widget OptionPlotTEXT;

extern Widget CoordinateSystem;
extern Widget CoordinateSystemC;
extern Widget CoordinateSystemP;
extern Widget CoordinateSystemR;
extern Widget CoordinateSystemS;
extern Widget CoordinateSystemD;

extern Widget HistoName;

extern Widget AngleDrawingArea;
extern Widget AngleThetaText;
extern Widget AnglePhiText;
extern Widget AngleApplyButton;
extern Widget AngleResetButton;
extern Widget AngleCloseButton;

extern Widget zoneCloseButton;
extern Widget zoneResetButton;
extern Widget zoneForm;

/* global data */
Widget histoPanel   = (Widget)NULL;
Widget widget_stat  = (Widget)NULL;
Widget widget_fit   = (Widget)NULL;
Widget widget_range = (Widget)NULL;
Widget widget_angle = (Widget)NULL;
Widget widget_zone  = (Widget)NULL;
Widget widget_file  = (Widget)NULL;
Widget widget_date  = (Widget)NULL;
Boolean grid_active;
Boolean zdisac_scale;
int old_x_zone   = 1;
int old_y_zone   = 1;
int file_or;
int date_or;
Widget zone_buttons[MAX_X_ZONE][MAX_Y_ZONE];
struct button_idx_struct {
   int x;
   int y;
} button_idx[MAX_X_ZONE][MAX_Y_ZONE];

/* global functions */
void set_histo_id(Boolean);
void set_environment(void);
void unselect_buttons(int,int);
void global_reset_cb(Widget, XtPointer, XmAnyCallbackStruct *);

/* local functions */
static void popdown_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void show_option(Widget, int *, XmToggleButtonCallbackStruct *);
static void show_set(Widget, int *, XmToggleButtonCallbackStruct *);
static void coordinate_set(Widget, int *, XmAnyCallbackStruct *);
static void show_range(Widget, int *, XmScaleCallbackStruct *);
static void change_angle(Widget, XtPointer, XEvent *);
static void move_cube(Widget, int *, XmDrawingAreaCallbackStruct *);
static void set_zone_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void zone_reset_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void zone_close_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void angle_apply_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void angle_reset_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void angle_close_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void file_close_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void date_close_cb(Widget, XtPointer, XmAnyCallbackStruct *);
static void read_fit_attributes(void);
static void read_stat_attributes(void);
static void read_zone_attributes(void);
static void read_angle_attributes(void);
static void read_file_attributes(void);
static void read_date_attributes(void);
static void unmanage_dialogs(void);

/* local data */
static int x_range_lock = 0;
static int y_range_lock = 0;
static int moving_cube  = 0;
static float theta_cube = 0;
static float phi_cube   = 0;
static int x_cube       = 60;
static int y_cube       = 60;
static int upd_nbin     = 0;
char   comstr[MAX_CMD_LENGTH]; /* string pass to execute_kuip_cmd */


/***********************************************************************
 *                                                                     *
 *   Show (manage) the histogram style panel.                          *
 *                                                                     *
 ***********************************************************************/
void show_histoStyle(Widget w, XtPointer client_data, XmAnyCallbackStruct *cbs)
{
   Pixmap        pixmap;
   Display      *display;

   if (!histoPanel) {
      histoPanel    = create_histoStyle();
      style_root    = (Style*)NULL;
      current_style = (Style*)NULL;
      /*
       *   Set icon for file browser popup
       */
      display = XtDisplay(histoPanel);
      pixmap  = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                      histo_bits, histo_width, histo_height);
      XtVaSetValues(XtParent(histoPanel), XmNtitle, "Histogram Style Panel",
                    XmNiconName, "Histogram Style Panel",
                    XmNiconPixmap, pixmap,
                    XmNdeleteResponse, XmUNMAP,
                    NULL);

      /* make style panel known to KUIP's Exec window */
      km_store_appShell(histoPanel, "Histogram Style Panel");

      /*
       *   Make sure that if the window is popped down, all the
       *   local popups are unmanaged.
       */
      XtAddCallback(XtParent(histoPanel), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, NULL);

      /*
       *   Reset button callback
       */
      XtAddCallback(ResetHistoPanel, XmNactivateCallback,
                    (XtCallbackProc)global_reset_cb, histoPanel);
     /*
      *   Install close callback
      */
      XtAddCallback(CloseHistoPanel, XmNactivateCallback,
                    (XtCallbackProc)close_cb, histoPanel);
      XtAddCallback(ClosePushButton, XmNactivateCallback,
                    (XtCallbackProc)close_cb, histoPanel);
     /*
      *   Install open and save style callbacks
      */
      XtAddCallback(OpenstylePushButton,XmNactivateCallback,
                    (XtCallbackProc)show_set,(int*)7);
      XtAddCallback(SavestylePushButton,XmNactivateCallback,
                    (XtCallbackProc)show_set,(int*)8);
      XtAddCallback(SavestyleasPushButton,XmNactivateCallback,
                    (XtCallbackProc)show_set,(int*)9);
     /*
      *   Install callbacks on options buttons (STAT, FIT, FILE, DATE, GRID)
      */
      XtAddCallback(StatToggleButtonGadget, XmNvalueChangedCallback,
                    (XtCallbackProc)show_option, (int*)1);
      XtAddCallback(FitToggleButtonGadget, XmNvalueChangedCallback,
                    (XtCallbackProc)show_option, (int*)2);
      XtAddCallback(FileToggleButtonGadget, XmNvalueChangedCallback,
                    (XtCallbackProc)show_option, (int*)3);
      XtAddCallback(DateToggleButtonGadget, XmNvalueChangedCallback,
                    (XtCallbackProc)show_option, (int*)4);
      XtAddCallback(RefreshToggleButtonGadget, XmNvalueChangedCallback,
                    (XtCallbackProc)show_option, (int*)6);
      XtAddCallback(SameToggleButtonGadget, XmNvalueChangedCallback,
                    (XtCallbackProc)show_option, (int*)8);
     /*
      *   Set toggle gadgets depending on values set in resource file
      */
      XmToggleButtonGadgetSetState(RefreshToggleButtonGadget,
                                   appres.auto_refresh, False);
      XmToggleButtonGadgetSetState(SameToggleButtonGadget,
                                   appres.same_zone, False);
     /*
      *   Install callbacks on setting buttons (Axis parameters, Option plot)
      */
      XtAddCallback(GeneralAttributesBut, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)40);
      XtAddCallback(AttrButton, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)3);
      XtAddCallback(GeometryButton, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)4);
      XtAddCallback(FontButton, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)5);
      XtAddCallback(AxisSettings, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)6);
      XtAddCallback(OptionPlotD, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotL, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotC, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotB, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotP, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotSTAR, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotE, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotE1, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotE2, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotE3, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotE4, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotBOX, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotCOL, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotSURF, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotSURF1, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotSURF2, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotSURF3, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotSURF4, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotLEGO, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotLEGO1, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotLEGO2, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotCONT, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotCONT3, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(OptionPlotTEXT, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)14);
      XtAddCallback(CoordinateSystemC, XmNactivateCallback,
                    (XtCallbackProc)coordinate_set, (int*)1);
      XtAddCallback(CoordinateSystemP, XmNactivateCallback,
                    (XtCallbackProc)coordinate_set, (int*)2);
      XtAddCallback(CoordinateSystemR, XmNactivateCallback,
                    (XtCallbackProc)coordinate_set, (int*)3);
      XtAddCallback(CoordinateSystemS, XmNactivateCallback,
                    (XtCallbackProc)coordinate_set, (int*)4);
      XtAddCallback(CoordinateSystemD, XmNactivateCallback,
                    (XtCallbackProc)coordinate_set, (int*)5);
      XtAddCallback(PlotHistoPanel, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)33);
      XtAddCallback(AxisRanges, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)50);
      XtAddCallback(Angles, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)51);
      XtAddCallback(ZoneButton, XmNactivateCallback,
                    (XtCallbackProc)show_set, (int*)52);

      set_histo_id(True);
   } else
      set_histo_id(False);

   UxPopupInterface(histoPanel, XtGrabNone);
   /*
    * If the dialog was already open, UxPopupInterface does nothing.  In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XMapRaised(XtDisplay(histoPanel), XtWindow(XtParent(histoPanel)));
}

/***********************************************************************
 *                                                                     *
 *   Action routine which will be called when KUIP receives the        *
 *   command "style" from kxterm. This action routine has been         *
 *   registered in routine init_top_level_window.                      *
 *                                                                     *
 ***********************************************************************/
void kxterm_show_style(char **argv, int argc)
{
   show_histoStyle((Widget)NULL, (XtPointer)NULL,
                   (XmAnyCallbackStruct *)NULL);
}

/***********************************************************************
 *                                                                     *
 *   Popdown callback. This callback is called when the histoPanel     *
 *   widget is popped down.                                            *
 *                                                                     *
 ***********************************************************************/
static void popdown_cb(Widget w, XtPointer call_data,
                       XmAnyCallbackStruct *cbs)
{
   unmanage_style_dialogs();
   unmanage_attributes_dialog();
   unmanage_generalAttributes();
   unmanage_fontSettings();
   unmanage_geometry_dialog();
   unmanage_axis_dialog();
   unmanage_dialogs();
}

/***********************************************************************
 *                                                                     *
 *   Call back on option Toggle Button                                 *
 *                                                                     *
 ***********************************************************************/
static void show_option(Widget w, int *type, XmToggleButtonCallbackStruct *cbs)
{
   if (cbs->set) {
      switch( (int)type ) {
         case 1:
            if ( !widget_stat ) {
               widget_stat = create_statFormDialog();
               XtAddCallback(IdToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               XtAddCallback(EntriesToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               XtAddCallback(MeanToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               XtAddCallback(RmsToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               XtAddCallback(UnderToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               XtAddCallback(OverToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               XtAddCallback(AllchanToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)1);
               read_stat_attributes();
            }
            XtManageChild(widget_stat);
            XmUpdateDisplay(widget_stat);
            execute_kuip_cmd("Option STAT");
            break;

         case 2:
            if ( !widget_fit ) {
               widget_fit = create_fitFormDialog();
               XtAddCallback(ChiToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)2);
               XtAddCallback(ErrorsToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)2);
               XtAddCallback(ParametersToggleButtonGadget,
                    XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)2);
               read_fit_attributes();
            }
            XtManageChild(widget_fit);
            XmUpdateDisplay(widget_fit);
            execute_kuip_cmd("Option FIT");
            break;

         case 3:
          if ( !widget_file ) {
               widget_file = create_fileFormDialog();
               XtAddCallback(TLToggleButtonGadget,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)10);
               XtAddCallback(TRToggleButtonGadget,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)10);
               XtAddCallback(BLToggleButtonGadget,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)10);
               XtAddCallback(BRToggleButtonGadget,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)10);
               XtAddCallback(FileDatePushButtonGadget, XmNactivateCallback,
                 (XtCallbackProc)file_close_cb,NULL);

               read_file_attributes();
            }

            XtManageChild(widget_file);
            XmUpdateDisplay(widget_file);
            execute_kuip_cmd("Option FILE");
            break;

         case 4:
           if ( !widget_date ) {
               widget_date = create_dateFormDialog();
               XtAddCallback(TLToggleButtonGadget1,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)12);
               XtAddCallback(TRToggleButtonGadget1,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)12);
               XtAddCallback(BLToggleButtonGadget1,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)12);
               XtAddCallback(BRToggleButtonGadget1,
                   XmNvalueChangedCallback, (XtCallbackProc)show_set, (int*)12);
               XtAddCallback(FileDatePushButtonGadget1, XmNactivateCallback,
                 (XtCallbackProc)date_close_cb,NULL);
               read_date_attributes();
            }

            XtManageChild(widget_date);
            XmUpdateDisplay(widget_date);
            execute_kuip_cmd("Option DATE");
            break;

         case 5:
            break;

         case 6:
            appres.auto_refresh = True;
            if(widget_range ) XtSetSensitive(ApplyRangeButtonGadget, False);
            if(widget_axis ) XtSetSensitive(AxisApplyButton, False);
            if(widget_angle) XtSetSensitive(AngleApplyButton, False);
            if(widget_color) XtSetSensitive(ApplyAttColorBut, False);
            if(widget_defineColor) XtSetSensitive(ApplyColorBut, False);
            if(widget_fontSettings) XtSetSensitive(ApplyFontSettingsBut, False);
            if(widget_attributes) XtSetSensitive(ApplyAttributesBut, False);
            if(widget_geometry) XtSetSensitive(ApplyGeometry, False);
            if(widget_generalAttributes) XtSetSensitive(ApplyGeneralBut, False);
            if (widget_hatchStyle) XtSetSensitive(ApplyHatchBut, False);
            cut_apply_sensitivity(False);
            plot_current_histo();
            return;

         case 7:
            return;

         case 8:
            appres.same_zone = True;
            return;

         case 9:
            x_range_lock = histogram.x2-histogram.x1+1;
            return;

         case 10:
            y_range_lock = histogram.y2-histogram.y1+1;
            return;

         case 11:
            return;
      }
   } else {
      switch( (int)type ) {
         case 1:
          if (widget_stat) {
             XtUnmanageChild(widget_stat);
             execute_kuip_cmd("Option NSTA");}
            break;

         case 2:
          if (widget_fit) {
             XtUnmanageChild(widget_fit);
             execute_kuip_cmd("Option NFIT");
          }
            break;

         case 3:
          if (widget_file) {
             XtUnmanageChild(widget_file);
             execute_kuip_cmd("Option NFIL");
          }
            break;

         case 4:
          if (widget_date) {
             XtUnmanageChild(widget_date);
             execute_kuip_cmd("Option NDAT");
          }
            break;

         case 5:
            break;

         case 6:
            appres.auto_refresh = False;
            if( widget_range ) XtSetSensitive(ApplyRangeButtonGadget, True);
            if( widget_axis ) XtSetSensitive(AxisApplyButton, True);
            if( widget_angle) XtSetSensitive(AngleApplyButton,True);
            if( widget_color) XtSetSensitive(ApplyAttColorBut, True);
            if( widget_defineColor) XtSetSensitive(ApplyColorBut, True);
            if( widget_fontSettings) XtSetSensitive(ApplyFontSettingsBut, True);
            if( widget_attributes) XtSetSensitive(ApplyAttributesBut, True);
            if( widget_geometry) XtSetSensitive(ApplyGeometry, True);
            if( widget_generalAttributes) XtSetSensitive(ApplyGeneralBut, True);
            if( widget_hatchStyle) XtSetSensitive(ApplyHatchBut, True);
            cut_apply_sensitivity(True);
            return;

         case 7:
            return;

         case 8:
            appres.same_zone = False;
            return;

         case 9:
            x_range_lock = 0;
            return;

         case 10:
            y_range_lock = 0;
            return;

         case 11:
            return;
      }
   }
   XmUpdateDisplay(histoPanel);
   km_all_cursor(KM_WAIT_CURSOR);
   if ( appres.auto_refresh ) plot_current_histo();
   km_all_cursor(KM_MAIN_CURSOR);
}

/***********************************************************************
 *                                                                     *
 *   Unmanage dialog. This routine should be called by the             *
 *   popdown_cb routine of the parent.                                 *
 *                                                                     *
 ***********************************************************************/
static void unmanage_dialogs(void)
{
   MenuCbStruct  *item;

   if (widget_range) {
      XtVaGetValues(widget_range, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
   if (widget_angle) {
      XtVaGetValues(widget_angle, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
   if (widget_zone) {
      XtVaGetValues(widget_zone, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/***********************************************************************
 *                                                                     *
 *   Call back on set Toggle Button                                    *
 *                                                                     *
 ***********************************************************************/
static void show_set(Widget w, int *type, XmToggleButtonCallbackStruct *cbs)
{
   Display       *display;
   MenuCbStruct  *menu_button;
   int            x,y;


   switch( (int)type ) {
      case 1:
         sprintf (comstr,"Set STAT %d%d%d%d%d%d%d"
                  ,XmToggleButtonGadgetGetState(AllchanToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(OverToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(UnderToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(RmsToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(MeanToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(EntriesToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(IdToggleButtonGadget));
         break;

      case 2:
         sprintf (comstr,"Set FIT %d%d%d"
                  ,XmToggleButtonGadgetGetState(ChiToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(ErrorsToggleButtonGadget)
                  ,XmToggleButtonGadgetGetState(ParametersToggleButtonGadget));
         break;

      case 3:
         show_attributes(w);
         return;

      case 4:
         show_geometry(w);
         return;

      case 5:
         show_fontSettings(w);
         return;

      case 6:
         axis_settings_dialog(w);
         set_environment();
         return;

      case 7:
         open_style_dialog(w);
         return;

      case 8:
         save_style();
         return;

      case 9:
         save_style_as_dialog(w);
         return;

      case 10:
         if (cbs->set) {
           if (XmToggleButtonGadgetGetState(TLToggleButtonGadget))
              sprintf( comstr,"Set FILE %d",1);
           if (XmToggleButtonGadgetGetState(TRToggleButtonGadget))
              sprintf( comstr,"Set FILE %d",2);
           if (XmToggleButtonGadgetGetState(BLToggleButtonGadget))
              sprintf( comstr,"Set FILE %d",3);
           if (XmToggleButtonGadgetGetState(BRToggleButtonGadget))
              sprintf( comstr,"Set FILE %d",4);
           }
         break;

      case 11:
         set_histo_id(True);
         break;

      case 12:
         if (cbs->set) {
           if (XmToggleButtonGadgetGetState(TLToggleButtonGadget1))
              sprintf( comstr,"Set DATE %d",1);
           if (XmToggleButtonGadgetGetState(TRToggleButtonGadget1))
              sprintf( comstr,"Set DATE %d",2);
           if (XmToggleButtonGadgetGetState(BLToggleButtonGadget1))
              sprintf( comstr,"Set DATE %d",3);
           if (XmToggleButtonGadgetGetState(BRToggleButtonGadget1))
              sprintf( comstr,"Set DATE %d",4);
         }
         break;

      case 14:
         set_environment();
         break;

      case 33:
         plot_current_histo();
         return;

      case 40 :
         show_generalAttributes(w);
         return;

      case 50:
         if ( !widget_range ) {
           widget_range = create_rangeFormDialog();
           menu_button = make_menuStruct(w, widget_range);
           InstallMwmCloseCallback(XtParent(widget_range),
                   (XtCallbackProc)cancel_cb, (XtPointer)menu_button);
           XtVaSetValues(widget_range, XmNuserData, menu_button, NULL);
           XtAddCallback(X1Scale, XmNvalueChangedCallback,
                          (XtCallbackProc)show_range, (int*)1);
           XtAddCallback(X2Scale, XmNvalueChangedCallback,
                          (XtCallbackProc)show_range, (int*)2);
           XtAddCallback(X1Scale, XmNdragCallback,
                          (XtCallbackProc)show_range, (int*)1);
           XtAddCallback(X2Scale, XmNdragCallback,
                          (XtCallbackProc)show_range, (int*)2);
           XtAddCallback(LockXToggleButtonGadget, XmNvalueChangedCallback,
                          (XtCallbackProc)show_option, (int*)9);
           XtAddCallback(Y1Scale, XmNvalueChangedCallback,
                          (XtCallbackProc)show_range, (int*)3);
           XtAddCallback(Y2Scale, XmNvalueChangedCallback,
                          (XtCallbackProc)show_range, (int*)4);
           XtAddCallback(Y1Scale, XmNdragCallback,
                          (XtCallbackProc)show_range, (int*)3);
           XtAddCallback(Y2Scale, XmNdragCallback,
                          (XtCallbackProc)show_range, (int*)4);
           XtAddCallback(LockYToggleButtonGadget, XmNvalueChangedCallback,
                          (XtCallbackProc)show_option, (int*)10);
           XtAddCallback(CloseRangeButtonGadget, XmNactivateCallback,
                          (XtCallbackProc)cancel_cb, menu_button);
           XtAddCallback(ResetRangeButtonGadget, XmNactivateCallback,
                          (XtCallbackProc)show_set, (int*)11);
           XtAddCallback(ApplyRangeButtonGadget, XmNactivateCallback,
                          (XtCallbackProc)show_set, (int*)33);
           if (!appres.auto_refresh)
              XtSetSensitive(ApplyRangeButtonGadget, True);
           else
              XtSetSensitive(ApplyRangeButtonGadget, False);
           XtAddCallback(XScaleRebin, XmNvalueChangedCallback,
                          (XtCallbackProc)show_range, (int*)5);
           XtAddCallback(XScaleRebin, XmNdragCallback,
                          (XtCallbackProc)show_range, (int*)5);
           XtAddCallback(minimumText,XmNactivateCallback,
                          (XtCallbackProc)show_range, (int *)6);
           XtAddCallback(maximumText,XmNactivateCallback,
                          (XtCallbackProc)show_range, (int *)7);
           set_histo_id(True);
         }
         XtSetSensitive(w, False);
         XtManageChild(widget_range);
         return;

      case 51:
         if ( !widget_angle ) {
           widget_angle = create_angleFormDialog();
           XtManageChild(widget_angle);
           menu_button = make_menuStruct(w, widget_angle);
           InstallMwmCloseCallback(XtParent(widget_angle),
                  (XtCallbackProc)cancel_cb, (XtPointer)menu_button);
           XtVaSetValues(widget_angle, XmNuserData, menu_button, NULL);
           XmUpdateDisplay(widget_angle);
           XtAddEventHandler(AngleDrawingArea, PointerMotionMask, False,
                              (XtEventHandler)change_angle, NULL);
           XtAddCallback(AngleDrawingArea, XmNinputCallback,
              (XtCallbackProc)move_cube, (int*)1);
           display = XtDisplay(histoPanel);
           ixsdswi(display, XtWindow(AngleDrawingArea));
           read_angle_attributes();
           XtAddCallback(AngleApplyButton,XmNactivateCallback,
              (XtCallbackProc)angle_apply_cb,NULL);
           XtAddCallback(AngleResetButton,XmNactivateCallback,
              (XtCallbackProc)angle_reset_cb,NULL);
           XtAddCallback(AngleCloseButton,XmNactivateCallback,
              (XtCallbackProc)angle_close_cb,NULL);
           if (!appres.auto_refresh)
              XtSetSensitive(AngleApplyButton,True);
           else
              XtSetSensitive(AngleApplyButton,False);
         }
         else {
           XtManageChild(widget_angle);
           XmUpdateDisplay(widget_angle);
         }
         XtSetSensitive(w, False);
         return;

      case 52:
         if ( !widget_zone ) {
           widget_zone = create_zoneFormDialog();
           menu_button = make_menuStruct(w, widget_zone);
           InstallMwmCloseCallback(XtParent(widget_zone),
                   (XtCallbackProc)cancel_cb, (XtPointer)menu_button);
           XtVaSetValues(widget_zone, XmNuserData, menu_button, NULL);
           XtVaSetValues(zoneForm, XmNfractionBase, MAX_X_ZONE, NULL);
           for (x=0; x<MAX_X_ZONE; x++) {
             for (y=0; y<MAX_Y_ZONE; y++) {
               button_idx[x][y].x = x;
               button_idx[x][y].y = y;
               zone_buttons[x][y]=XtVaCreateManagedWidget("",
                   xmPushButtonWidgetClass,
                   zoneForm,
                   XmNtopAttachment, XmATTACH_POSITION,
                   XmNtopPosition,   y,
                   XmNleftAttachment, XmATTACH_POSITION,
                   XmNleftPosition,   x,
                   XmNrightAttachment, XmATTACH_POSITION,
                   XmNrightPosition,   x+1,
                   XmNbottomAttachment, XmATTACH_POSITION,
                   XmNbottomPosition,   y+1,
                   XmNuserData, &button_idx[x][y],
                   NULL);
               XtAddCallback(zone_buttons[x][y], XmNactivateCallback,
               (XtCallbackProc)set_zone_cb, NULL);
             }
           }
           XtAddCallback(zoneResetButton, XmNactivateCallback,
              (XtCallbackProc)zone_reset_cb,NULL);
           XtAddCallback(zoneCloseButton, XmNactivateCallback,
              (XtCallbackProc)zone_close_cb,NULL);
           read_zone_attributes();
         }
         XtSetSensitive(w, False);
         XtManageChild(widget_zone);
         XmUpdateDisplay(widget_zone);
         return;

      case 53:
         break;

   }
   XmUpdateDisplay(histoPanel);
   km_all_cursor(KM_WAIT_CURSOR);

   if (!strempty(comstr)) execute_kuip_cmd(comstr);

   /* cbs->event = NULL when called from set_histo_id */
   if (appres.auto_refresh && cbs->event) plot_current_histo();

   km_all_cursor(KM_MAIN_CURSOR);
}

/***********************************************************************
 *                                                                     *
 *   Call back on the coordinate system                                *
 *                                                                     *
 ***********************************************************************/
static void coordinate_set(Widget w, int *type, XmAnyCallbackStruct *cbs)
{

   switch( (int)type ) {
      case 1:
         sprintf (histogram.coordinate,"   ");
         break;

      case 2:
         sprintf (histogram.coordinate,"POL");
         break;

      case 3:
         sprintf (histogram.coordinate,"CYL");
         break;

      case 4:
         sprintf (histogram.coordinate,"SPH");
         break;

      case 5:
         sprintf (histogram.coordinate,"PSD");
         break;
   }
   /* cbs->event = NULL when called from set_histo_id */
   if (appres.auto_refresh && cbs->event) plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *   Call back on the range scales                                     *
 *                                                                     *
 ***********************************************************************/
static void show_range(Widget w, int *type, XmScaleCallbackStruct *cbs)
{
   char     *minmax;
   char     *nbins;
   int      N;
   int      ilen1, ilen2;
   XmString xmstr;

   switch( (int)type ) {
      case 1:
         histogram.x1 = cbs->value;
         if( x_range_lock != 0 ) {
            histogram.x2 = histogram.x1 + x_range_lock - 1;
            if( histogram.x2 > histogram.nx ) {
               histogram.x2 = histogram.nx;
               histogram.x1 = histogram.nx - x_range_lock + 1;
            }
         }
         if( (histogram.x2 - histogram.x1) < 2 )
            histogram.x2 = min(histogram.x1+2,histogram.nx);
         break;

      case 2:
         histogram.x2 = cbs->value;
         if( x_range_lock != 0 ) {
            histogram.x1 = histogram.x2 - x_range_lock + 1;
            if( histogram.x1 < 1 ) {
               histogram.x1 = 1;
               histogram.x2 = x_range_lock;
            }
         }
         if( (histogram.x2 - histogram.x1) < 2 )
            histogram.x1 = max(histogram.x2-2,1);
         break;

      case 3:
         histogram.y1 = cbs->value;
         if( y_range_lock != 0 ) {
            histogram.y2 = histogram.y1 + y_range_lock - 1;
            if( histogram.y2 > histogram.ny ) {
               histogram.y2 = histogram.ny;
               histogram.y1 = histogram.ny - y_range_lock + 1;
            }
         }
         if( (histogram.y2 - histogram.y1) < 2 )
            histogram.y2 = min(histogram.y1+2,histogram.ny);
         break;

      case 4:
         histogram.y2 = cbs->value;
         if( y_range_lock != 0 ) {
            histogram.y1 = histogram.y2 - y_range_lock + 1;
            if( histogram.y1 < 1 ) {
               histogram.y1 = 1;
               histogram.y2 = y_range_lock;
            }
         }
         if( (histogram.y2 - histogram.y1) < 2 )
            histogram.y1 = max(histogram.y2-2,1);
         break;

      case 5:
         N = cbs->value;
         ilen1 = strlen(histogram.dir);
         ilen2 = strlen(histogram.option_1d);
         Rebin(&histogram.id, &histogram.x1, &histogram.x2, &N,
               histogram.dir, ilen1, histogram.option_1d, ilen2);
         nbins = XtCalloc(8, 1);
         sprintf (nbins,"%d",N);
         xmstr = XmStringCreate(nbins, XmSTRING_DEFAULT_CHARSET);
         XtVaSetValues(XBinNumber, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XtFree(nbins);
         upd_nbin = 1;
         return;

      case 6:
         minmax = XmTextGetString(minimumText);
         sprintf (comstr,"Minimum %d %s",histogram.id,minmax);
         strcpy(histogram.dir, "//PAWC");
         set_histo_id(False);
         execute_kuip_cmd(comstr);
         if ( appres.auto_refresh ) plot_current_histo();
         XtFree(minmax);
         return;

      case 7:
         minmax = XmTextGetString(maximumText);
         sprintf (comstr,"Maximum %d %s",histogram.id,minmax);
         strcpy(histogram.dir, "//PAWC");
         set_histo_id(False);
         execute_kuip_cmd(comstr);
         if ( appres.auto_refresh ) plot_current_histo();
         XtFree(minmax);
         return;

      default:
         return;
   }

      if( histogram.x1 <= 0 ) histogram.x1 = 1;
      if( histogram.x2 <= 0 ) histogram.x2 = 1;
      if( histogram.y1 <= 0 ) histogram.y1 = 1;
      if( histogram.y2 <= 0 ) histogram.y2 = 1;

   if ( (int)type < 3 ) {
      XmScaleSetValue(X2Scale,histogram.x2);
      XmScaleSetValue(X1Scale,histogram.x1);
      if( x_range_lock == 0 || upd_nbin) {
         nbins = XtCalloc(8, 1);
         sprintf (nbins,"%d",histogram.x2-histogram.x1+1);
         xmstr = XmStringCreate(nbins, XmSTRING_DEFAULT_CHARSET);
         XtVaSetValues(XBinNumber, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XtFree(nbins);
         upd_nbin = 0;
      }
   } else {
      XmScaleSetValue(Y2Scale,histogram.y2);
      XmScaleSetValue(Y1Scale,histogram.y1);
      if( y_range_lock == 0 ) {
         nbins = XtCalloc(8, 1);
         sprintf (nbins,"%d",histogram.y2-histogram.y1+1);
         xmstr = XmStringCreate(nbins, XmSTRING_DEFAULT_CHARSET);
         XtVaSetValues(YBinNumber, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XtFree(nbins);
      }
   }
   XmUpdateDisplay(AxisRanges);

   if ( appres.auto_refresh ) plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *   Set the current histogram ID label                                *
 *                                                                     *
 * Note that in this procedure, HISTO_3D is (for the time being)       *
 * consider as an Ntuple in which 3 variables have to be plotted.      *
 *                                                                     *
 ***********************************************************************/
void set_histo_id(Boolean force)
{
   char          *cid;
   char          title[MAX_STRING];
   XmString      xmstr;
   int           nx, ny;
   int           bits;
   int           ntup = 0;
   float         xmin, xmax, ymin, ymax;
   static int    old_id   = 0;
   static char   old_dir[MAX_STRING] = "\0";
   char          tid[3];

   if ( histogram.type != HISTO_1D &&
        histogram.type != HISTO_2D &&
        histogram.type != HISTO_3D ) return;

   if ( histogram.type != HISTO_3D ) {
      Mhlattr(histogram.dir, strlen(histogram.dir), &histogram.id,
              title, (sizeof title),
              &nx, &xmin, &xmax, &ny, &ymin, &ymax, &bits, &ntup);

      if (histogram.id   == old_id && !strcmp(histogram.dir, old_dir) &&
          histogram.nx   == nx     && histogram.xmin == xmin          &&
          histogram.xmax == xmax   && histogram.ny   == ny            &&
          histogram.ymin == ymin   && histogram.ymax == ymax          &&
          !force)
         return;
   }

   if ( histogram.type != HISTO_3D ) {
      histogram.bits = bits;
      histogram.nx   = nx;
      histogram.xmin = xmin;
      histogram.xmax = xmax;
      histogram.ny   = ny;
      histogram.ymin = ymin;
      histogram.ymax = ymax;
      histogram.x1   = 1;
      histogram.y1   = 1;
      histogram.x2   = nx;
      histogram.y2   = ny;

      old_id = histogram.id;
      strcpy(old_dir, histogram.dir);
   }

   if ( !histoPanel ) return;

   set_environment();

   switch (histogram.type) {
      case HISTO_1D:
         sprintf (tid,"1d");
         break;
      case HISTO_2D:
         sprintf (tid,"2d");
         break;
      case HISTO_3D:
         sprintf (tid,"3d");
         break;
   }

   cid = XtCalloc(MAX_CMD_LENGTH, 1);
   if (histogram.type == HISTO_3D) {
      sprintf (cid,"%s/%d  (%s)",ntuple.dir, ntuple.id, tid);
   } else {
      sprintf (cid,"%s/%d  (%s)",histogram.dir, histogram.id, tid);
   }
   xmstr = XmStringCreate(cid, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(HistoName, XmNlabelString, xmstr, NULL);
   XmStringFree(xmstr);

   if ( widget_range && histogram.type != HISTO_3D) {
      int mult = max (1, (int) ((histogram.nx - 1)/10));
      XtVaSetValues(X1Scale, XmNscaleMultiple, mult, NULL);
      XtVaSetValues(X1Scale,
                    XmNminimum, 1,
                    XmNmaximum, histogram.nx,
                    XmNvalue,   1,
                    NULL);
      XtVaSetValues(X2Scale, XmNscaleMultiple, mult, NULL);
      XtVaSetValues(X2Scale,
                    XmNminimum, 1,
                    XmNmaximum, histogram.nx,
                    XmNvalue,   histogram.nx,
                    NULL);
      sprintf (cid,"%d",histogram.nx);
      xmstr = XmStringCreate(cid, XmSTRING_DEFAULT_CHARSET);
      XtVaSetValues(XBinNumber, XmNlabelString, xmstr, NULL);
      XmStringFree(xmstr);

      if ( histogram.type == HISTO_2D ) {
         int mult = max (1, (int) ((histogram.ny - 1)/10));
         XtSetSensitive(YRangeFrame, True);
         XtVaSetValues(Y1Scale, XmNscaleMultiple, mult, NULL);
         XtVaSetValues(Y1Scale,
                       XmNminimum, 1,
                       XmNmaximum, histogram.ny,
                       XmNvalue,   1,
                       NULL);
         XtVaSetValues(Y2Scale, XmNscaleMultiple, mult, NULL);
         XtVaSetValues(Y2Scale,
                       XmNminimum, 1,
                       XmNmaximum, histogram.ny,
                       XmNvalue,   histogram.ny,
                       NULL);
         sprintf (cid,"%d",histogram.ny);
         xmstr = XmStringCreate(cid, XmSTRING_DEFAULT_CHARSET);
         XtVaSetValues(YBinNumber, XmNlabelString, xmstr, NULL);
         XmStringFree(xmstr);
         XtSetSensitive(XScaleRebin, False);
      } else {
         XtSetSensitive(YRangeFrame, False);
         XtSetSensitive(XScaleRebin, True);
         XtVaSetValues(XScaleRebin,
                       XmNminimum, 1,
                       XmNmaximum, histogram.nx,
                       XmNvalue,   1,
                       NULL);
      }
      XtSetSensitive(X1Scale, True);
      XtSetSensitive(X2Scale, True);
      XmToggleButtonGadgetSetState(LockXToggleButtonGadget, False, False);
      XmToggleButtonGadgetSetState(LockYToggleButtonGadget, False, False);
      x_range_lock = 0;
      y_range_lock = 0;
      XmUpdateDisplay(widget_range);
   }

   XmUpdateDisplay(histoPanel);
   XtFree(cid);
}

/***********************************************************************
 *                                                                     *
 *           Reset all the histostyle Panel widgets                    *
 *                                                                     *
 ***********************************************************************/
void global_reset_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   XmAnyCallbackStruct cbs1;

   cbs1.reason = 0;
   cbs1.event = (XEvent *) NULL;

   execute_kuip_cmd("Igset * ; Set * ; Option * ; Option ZFL1");
   execute_kuip_cmd("Angle 30 30");

   if (widget_axis)
      XtCallCallbacks(AxisResetButton,XmNactivateCallback, &cbs1);
   if (widget_angle)
      XtCallCallbacks(AngleResetButton,XmNactivateCallback, &cbs1);
   if (widget_zone)
      XtCallCallbacks(zoneResetButton,XmNactivateCallback, &cbs1);
   if (widget_stat) {
      XmToggleButtonGadgetSetState(IdToggleButtonGadget,True,False);
      XmToggleButtonGadgetSetState(EntriesToggleButtonGadget,True,False);
      XmToggleButtonGadgetSetState(MeanToggleButtonGadget,True,False);
      XmToggleButtonGadgetSetState(RmsToggleButtonGadget,True,False);
      XmToggleButtonGadgetSetState(UnderToggleButtonGadget,False,False);
      XmToggleButtonGadgetSetState(OverToggleButtonGadget,False,False);
      XmToggleButtonGadgetSetState(AllchanToggleButtonGadget,False,False);
   }
   if (widget_fit) {
      XmToggleButtonGadgetSetState(ChiToggleButtonGadget,True,False);
      XmToggleButtonGadgetSetState(ParametersToggleButtonGadget,True,False);
      XmToggleButtonGadgetSetState(ErrorsToggleButtonGadget,False,False);
   }
   if (widget_file)
      XmToggleButtonGadgetSetState(TLToggleButtonGadget,True,True);
   if (widget_date)
      XmToggleButtonGadgetSetState(TRToggleButtonGadget1,True,True);
   cbs1.reason = -999;
   if (widget_geometry)
      XtCallCallbacks(ResetGeometry,XmNactivateCallback, &cbs1);
   if (widget_generalAttributes)
      XtCallCallbacks(ResetGeneralBut,XmNactivateCallback, &cbs1);
   if (widget_attributes)
      XtCallCallbacks(ResetAttributesBut,XmNactivateCallback, &cbs1);
   if (widget_fontSettings)
      XtCallCallbacks(ResetFontSettingsBut,XmNactivateCallback, &cbs1);
   if (cbs->event  && appres.auto_refresh) plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *   Handle the angle window motion event.                             *
 *                                                                     *
 ***********************************************************************/
static void change_angle(Widget widget, XtPointer client_data, XEvent *ev)
{
   char     angle[8];

   if( moving_cube ) {
      x_cube      = ev->xbutton.x;
      y_cube      = ev->xbutton.y;
      x_cube      = min(x_cube,120);
      x_cube      = max(x_cube,0);
      y_cube      = min(y_cube,120);
      y_cube      = max(y_cube,0);
      phi_cube    = (float)x_cube;
      theta_cube  = (float)y_cube;
      getang(&theta_cube,&phi_cube);
      sprintf (angle,"%d",(int)theta_cube);
      XmTextSetString(AngleThetaText,angle);
      sprintf (angle,"%d",(int)phi_cube);
      XmTextSetString(AnglePhiText,(char *)angle);
   }
}

/***********************************************************************
 *                                                                     *
 *   Start and finish with the angle changing                          *
 *                                                                     *
 ***********************************************************************/
static void move_cube(Widget w, int *type,
                      XmDrawingAreaCallbackStruct *call_data)
{
   XEvent *ev = call_data->event;

   switch ( ev->type ) {

   case ButtonPress :
        moving_cube = 1;
        XWarpPointer(XtDisplay(histoPanel),XtWindow(AngleDrawingArea),
                     XtWindow(AngleDrawingArea), 0, 0, 120, 120,
                     x_cube, y_cube);
        break;

   case ButtonRelease :
        moving_cube = 0;
        phi_cube    = ev->xbutton.x;
        theta_cube  = ev->xbutton.y;
        getang(&theta_cube,&phi_cube);
        sprintf (comstr,"Angle %f %f",theta_cube,phi_cube);
        execute_kuip_cmd(comstr);
        if ( appres.auto_refresh ) plot_current_histo();
        break;

   default : break;
   }
}

/***********************************************************************
 *                                                                     *
 *          Apply angle changing                                       *
 *                                                                     *
 ***********************************************************************/
static void angle_apply_cb(Widget w,  XtPointer cd, XmAnyCallbackStruct *cbs)
{
   char       *theta,*phi;
   theta      = XmTextGetString(AngleThetaText);
   phi        = XmTextGetString(AnglePhiText);
   theta_cube = (float)atoi(theta);
   phi_cube   = (float)atoi(phi);
   theta_cube = 90 -theta_cube;
   phi_cube   = -90 - phi_cube;
   theta_cube =  60*(((theta_cube-60)/180) + 1);
   phi_cube   =  60*(((phi_cube+120)/(-360)) +1);
   getang(&theta_cube,&phi_cube);
   sprintf (comstr,"Angle %f %f",theta_cube,phi_cube);
   execute_kuip_cmd(comstr);
   plot_current_histo();
   XtFree(theta);
   XtFree(phi);
}

/***********************************************************************
 *                                                                     *
 *           Reset default angles                                      *
 *                                                                     *
 ***********************************************************************/
static void angle_reset_cb(Widget w,  XtPointer cd, XmAnyCallbackStruct *cbs)
{
   theta_cube = phi_cube = 30;
   theta_cube = 90 -theta_cube;
   phi_cube   = -90 - phi_cube;
   theta_cube = 60*(((theta_cube-60)/180) + 1);
   phi_cube   = 60*(((phi_cube+120)/(-360)) +1);
   getang(&theta_cube,&phi_cube);
   sprintf(comstr,"Angle %f %f",theta_cube,phi_cube);
   XmTextSetString(AngleThetaText,"30");
   XmTextSetString(AnglePhiText,"30");
   execute_kuip_cmd(comstr);
   if (cbs->event)
      plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *          Close ANGLE widget                                         *
 *                                                                     *
 ***********************************************************************/
static void angle_close_cb(Widget w,  XtPointer cd, XmAnyCallbackStruct *cbs)
{
  XtSetSensitive(Angles,True);
  XtUnmanageChild(widget_angle);
}

/***********************************************************************
 *                                                                     *
 *           Show currently zone                                       *
 *                                                                     *
 ***********************************************************************/
static void set_zone_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
     int    x, y;
     struct button_idx_struct *idx;

     XtVaGetValues(w, XmNuserData, &idx, NULL);

     sprintf(comstr,"Zone %d %d", (idx->x+1),(idx->y+1));
     unselect_buttons(old_x_zone, old_y_zone);
     for (x=0; x<=idx->x; x++) {
      for (y=0; y<=idx->y; y++)
         TogglePushButton(zone_buttons[x][y], True);
     }
     old_x_zone = x;
     old_y_zone = y;

     execute_kuip_cmd(comstr);
     plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *           Reset the ZONE widget                                     *
 *                                                                     *
 ***********************************************************************/
static void zone_reset_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   sprintf(comstr,"Zone 1 1");
   unselect_buttons(old_x_zone, old_y_zone);
   TogglePushButton(zone_buttons[0][0], True);
   old_x_zone = 1;
   old_y_zone = 1;
   execute_kuip_cmd(comstr);
   if (cbs->event) plot_current_histo();
}

/***********************************************************************
 *                                                                     *
 *           Unselect the buttons of ZONE widget                       *
 *                                                                     *
 ***********************************************************************/
void unselect_buttons(int x_old, int y_old)
{
   int x, y;

   for (x=0; x<x_old; x++) {
      for (y=0; y<y_old; y++)
       TogglePushButton(zone_buttons[x][y], False);
   }
}

/***********************************************************************
 *                                                                     *
 *           Close the ZONE widget                                     *
 *                                                                     *
 ***********************************************************************/
static void zone_close_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
   XtSetSensitive(ZoneButton,True);
   XtUnmanageChild(widget_zone);
}

/***********************************************************************
 *                                                                     *
 *           Retrieve the FIT attributes                               *
 *                                                                     *
 ***********************************************************************/
static void read_fit_attributes()
{
   char str[4];
   int temp;
   float r;

   Hplset("?FIT",&r);
   sprintf(str,"%3d",(int)r);
   sscanf( &str[0], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(ChiToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(ChiToggleButtonGadget,False,False);
   sscanf( &str[1], "%1d", &temp);

   if(temp)
      XmToggleButtonGadgetSetState(ErrorsToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(ErrorsToggleButtonGadget,False,False);
   sscanf( &str[2], "%1d", &temp);

   if(temp)
      XmToggleButtonGadgetSetState(ParametersToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(ParametersToggleButtonGadget,False,False);
}

/***********************************************************************
 *                                                                     *
 *           Retrieve the STAT attributes                              *
 *                                                                     *
 ***********************************************************************/
static void read_stat_attributes()
{
   char str[8];
   int temp;
   float r;

   Hplset("?STAT",&r);
   if (r == 1111)
      sprintf(str,"%d%d%d%4d",0,0,0,(int)r);
   else
      sprintf(str,"%7d",(int)r);
   sscanf( &str[0], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(AllchanToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(AllchanToggleButtonGadget,False,False);
   sscanf( &str[1], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(OverToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(OverToggleButtonGadget,False,False);
   sscanf( &str[2], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(UnderToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(UnderToggleButtonGadget,False,False);
   sscanf( &str[3], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(RmsToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(RmsToggleButtonGadget,False,False);
   sscanf( &str[4], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(MeanToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(MeanToggleButtonGadget,False,False);
   sscanf( &str[5], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(EntriesToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(EntriesToggleButtonGadget,False,False);
   sscanf( &str[6], "%1d", &temp);

   if (temp)
      XmToggleButtonGadgetSetState(IdToggleButtonGadget,True,False);
   else
      XmToggleButtonGadgetSetState(IdToggleButtonGadget,False,False);
}

/***********************************************************************
 *                                                                     *
 *           Retrieve the ZONE attributes                              *
 *                                                                     *
 ***********************************************************************/
static void read_zone_attributes()
{
   int ix,iy;
   int x,y;

   hplgzo(&ix,&iy);
   for (x=0; x<=ix-1; x++) {
      for (y=0; y<=iy-1; y++)
       TogglePushButton(zone_buttons[x][y], True);
   }
   old_x_zone = x;
   old_y_zone = y;
}

/***********************************************************************
 *                                                                     *
 *           Retrieve the ANGLE attributes                             *
 *                                                                     *
 ***********************************************************************/
static void read_angle_attributes()
{
   char *theta,*phi;

   gettp(&theta_cube,&phi_cube);
   sprintf(comstr,"%d",(int)theta_cube);
   XmTextSetString(AngleThetaText,(char *)comstr);
   sprintf(comstr,"%d",(int)phi_cube);
   XmTextSetString(AnglePhiText,(char *)comstr);
   theta      = XmTextGetString(AngleThetaText);
   phi        = XmTextGetString(AnglePhiText);
   theta_cube = (float)atoi(theta);
   phi_cube   = (float)atoi(phi);
   theta_cube = 90 -theta_cube;
   phi_cube   = -90 - phi_cube;
   theta_cube =  60*(((theta_cube-60)/180) + 1);
   phi_cube   =  60*(((phi_cube+120)/(-360)) +1);
   x_cube     = (int)phi_cube;
   y_cube     = (int)theta_cube;
   getang(&theta_cube,&phi_cube);
   XtFree(theta);
   XtFree(phi);
}

/***********************************************************************
 *                                                                     *
 *           Retrieve the FILE NAME attributes                         *
 *                                                                     *
 ***********************************************************************/
static void read_file_attributes()
{
   int n;
   float r;

   Hplset("?FILE",&r);
   n = (int) r;
   switch (n) {
       case 1:
       XmToggleButtonGadgetSetState(TLToggleButtonGadget,True,True);
       break;
      case 2:
       XmToggleButtonGadgetSetState(TRToggleButtonGadget,True,True);
       break;
      case 3:
       XmToggleButtonGadgetSetState(BLToggleButtonGadget,True,True);
       break;
      case 4:
       XmToggleButtonGadgetSetState(BRToggleButtonGadget,True,True);
       break;
   }
}

/***********************************************************************
 *                                                                     *
 *           Retrieve the DATE attributes                              *
 *                                                                     *
 ***********************************************************************/
static void read_date_attributes()
{
   int n;
   float r;

   Hplset("?DATE",&r);
   n = (int) r;
   switch (n) {
       case 1:
       XmToggleButtonGadgetSetState(TLToggleButtonGadget1,True,True);
       break;
      case 2:
       XmToggleButtonGadgetSetState(TRToggleButtonGadget1,True,True);
       break;
      case 3:
       XmToggleButtonGadgetSetState(BLToggleButtonGadget1,True,True);
       break;
      case 4:
       XmToggleButtonGadgetSetState(BRToggleButtonGadget1,True,True);
       break;
   }
}

/***********************************************************************
 *                                                                     *
 *           Close the FILE NAME widget                                *
 *                                                                     *
 ***********************************************************************/
static void file_close_cb(Widget w,  XtPointer cd, XmAnyCallbackStruct *cbs)
{
    XtUnmanageChild(widget_file);
}

/***********************************************************************
 *                                                                     *
 *           Close the DATE widget                                     *
 *                                                                     *
 ***********************************************************************/
static void date_close_cb(Widget w,  XtPointer cd, XmAnyCallbackStruct *cbs)
{
    XtUnmanageChild(widget_date);
}

/***********************************************************************
 *                                                                     *
 *   Set the valid Motif environment according to the histogram type   *
 *   and the plotting options selected.                                *
 *                                                                     *
 ***********************************************************************/
void set_environment()
{
   Boolean plot_3d;
   static Widget option_plot_1d_def = (Widget)NULL;
   static Widget option_plot_2d_def = (Widget)NULL;
   static Widget option_plot_3d_def = (Widget)NULL;
   static int    old_type = 0;
   char          option[20];

   if ( !histoPanel ) return;

   if (!option_plot_1d_def) {
      option_plot_1d_def = OptionPlotD;
      option_plot_2d_def = OptionPlotD;
      option_plot_3d_def = OptionPlotD;
   }
   if (old_type == HISTO_1D) {
      XtVaGetValues(OptionPlot, XmNmenuHistory, &option_plot_1d_def, NULL);
   } else if (old_type == HISTO_2D) {
      XtVaGetValues(OptionPlot, XmNmenuHistory, &option_plot_2d_def, NULL);
   } else if (old_type == HISTO_3D) {
      XtVaGetValues(OptionPlot, XmNmenuHistory, &option_plot_3d_def, NULL);
   }
   if (histogram.type == HISTO_1D) {
      strcpy(option, XtName(option_plot_1d_def));
      strcpy(histogram.option_1d, &option[10]);
      if (strstr(histogram.option_1d,"STAR")) sprintf (histogram.option_1d,"*");
   } else if (histogram.type == HISTO_2D) {
      strcpy(option, XtName(option_plot_2d_def));
      strcpy(histogram.option_2d, &option[10]);
   } else if (histogram.type == HISTO_3D) {
      strcpy(option, XtName(option_plot_3d_def));
      strcpy(histogram.option_3d, &option[10]);
      if (strstr(histogram.option_3d,"SURF4"))
         sprintf (histogram.option_3d,"G");
   } else {
      strcpy(option, " ");
   }

   if(old_type != histogram.type) {
      old_type = histogram.type;
      switch (histogram.type) {
         case HISTO_1D:
            XtManageChild   (OptionPlotL);
            XtManageChild   (OptionPlotC);
            XtManageChild   (OptionPlotB);
            XtManageChild   (OptionPlotP);
            XtManageChild   (OptionPlotSTAR);
            XtManageChild   (OptionPlotE);
            XtManageChild   (OptionPlotE1);
            XtManageChild   (OptionPlotE2);
            XtManageChild   (OptionPlotE3);
            XtManageChild   (OptionPlotE4);
            XtManageChild   (OptionPlotSURF);
            XtManageChild   (OptionPlotSURF1);
            XtManageChild   (OptionPlotSURF2);
            XtManageChild   (OptionPlotLEGO);
            XtManageChild   (OptionPlotLEGO1);
            XtManageChild   (OptionPlotLEGO2);
            XtUnmanageChild (OptionPlotBOX);
            XtUnmanageChild (OptionPlotCOL);
            XtUnmanageChild (OptionPlotSURF3);
            XtUnmanageChild (OptionPlotSURF4);
            XtUnmanageChild (OptionPlotCONT);
            XtUnmanageChild (OptionPlotCONT3);
            XtUnmanageChild (OptionPlotTEXT);
            XtVaSetValues(OptionPlot, XmNmenuHistory, option_plot_1d_def, NULL);
            break;

         case HISTO_2D:
            XtUnmanageChild (OptionPlotL);
            XtUnmanageChild (OptionPlotC);
            XtUnmanageChild (OptionPlotB);
            XtUnmanageChild (OptionPlotP);
            XtUnmanageChild (OptionPlotSTAR);
            XtUnmanageChild (OptionPlotE);
            XtUnmanageChild (OptionPlotE1);
            XtUnmanageChild (OptionPlotE2);
            XtUnmanageChild (OptionPlotE3);
            XtUnmanageChild (OptionPlotE4);
            XtManageChild   (OptionPlotSURF);
            XtManageChild   (OptionPlotSURF1);
            XtManageChild   (OptionPlotSURF2);
            XtManageChild   (OptionPlotLEGO);
            XtManageChild   (OptionPlotLEGO1);
            XtManageChild   (OptionPlotLEGO2);
            XtManageChild   (OptionPlotBOX);
            XtManageChild   (OptionPlotCOL);
            XtManageChild   (OptionPlotSURF3);
            XtManageChild   (OptionPlotSURF4);
            XtManageChild   (OptionPlotCONT);
            XtManageChild   (OptionPlotCONT3);
            XtManageChild   (OptionPlotTEXT);
            XtVaSetValues(OptionPlot, XmNmenuHistory, option_plot_2d_def, NULL);
            break;

         case HISTO_3D:
            XtUnmanageChild (OptionPlotL);
            XtUnmanageChild (OptionPlotC);
            XtUnmanageChild (OptionPlotB);
            XtUnmanageChild (OptionPlotP);
            XtUnmanageChild (OptionPlotSTAR);
            XtUnmanageChild (OptionPlotE);
            XtUnmanageChild (OptionPlotE1);
            XtUnmanageChild (OptionPlotE2);
            XtUnmanageChild (OptionPlotE3);
            XtUnmanageChild (OptionPlotE4);
            XtUnmanageChild (OptionPlotBOX);
            XtUnmanageChild (OptionPlotCOL);
            XtUnmanageChild (OptionPlotSURF3);
            XtUnmanageChild (OptionPlotCONT);
            XtUnmanageChild (OptionPlotCONT3);
            XtUnmanageChild (OptionPlotTEXT);
            XtUnmanageChild (OptionPlotSURF);
            XtUnmanageChild (OptionPlotSURF1);
            XtUnmanageChild (OptionPlotSURF2);
            XtUnmanageChild (OptionPlotLEGO);
            XtUnmanageChild (OptionPlotLEGO1);
            XtUnmanageChild (OptionPlotLEGO2);
            XtManageChild   (OptionPlotSURF4);
            XtVaSetValues(OptionPlot, XmNmenuHistory, option_plot_3d_def, NULL);
            break;

         default:
            break;
      }
   }

/*
 *      Determine if the current option is 3D or not (plot_3d)
 */
   if (strstr(option,"SURF") || strstr(option,"LEGO")) {
      plot_3d = True;
      grid_active = False;
      XtSetSensitive (CoordinateSystem,True);
   } else {
      plot_3d = False;
      grid_active = True;
      XtVaSetValues (CoordinateSystem, XmNmenuHistory, CoordinateSystemC, NULL);
      XtSetSensitive (CoordinateSystem,False);
      sprintf (histogram.coordinate," ");
   }

/*
 *      Update the Axis widget (Axis Settings ...)
 */
   if (widget_axis) {
      if (plot_3d) {
         if (histogram.type == HISTO_1D) {
            if (XmToggleButtonGadgetGetState(YAxisToggleButtonGadget)) {
                XmToggleButtonGadgetSetState
                (XAxisToggleButtonGadget,True,True);
                XmToggleButtonGadgetSetState
                (YAxisToggleButtonGadget,False,False);
            }
            XtSetSensitive (YAxisToggleButtonGadget,False);
         } else {
            XtSetSensitive (YAxisToggleButtonGadget,True);
         }
         XtSetSensitive (ZAxisToggleButtonGadget,True);
         XtSetSensitive (DivAxisform,True);
         XtSetSensitive (gridtoggleButtonGadget,False);
         XtSetSensitive (orienPushButtonGadget,False);
         XtSetSensitive (labelPushButtonGadget,False);
         if (widget_orien) XtUnmanageChild(widget_orien);
         if (widget_label) XtUnmanageChild(widget_label);
      } else {
         if (histogram.type == HISTO_1D) {
            if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget)) {
                XmToggleButtonGadgetSetState
                (XAxisToggleButtonGadget,True,True);
                XmToggleButtonGadgetSetState
                (ZAxisToggleButtonGadget,False,False);
            }
            XtSetSensitive (ZAxisToggleButtonGadget,False);
            XtSetSensitive (DivAxisform,True);
         } else {
            XtSetSensitive (ZAxisToggleButtonGadget,True);
            if (XmToggleButtonGadgetGetState(ZAxisToggleButtonGadget)) {
               XtSetSensitive (DivAxisform,False);
            } else {
               XtSetSensitive (DivAxisform,True);
            }
         }
         XtSetSensitive (YAxisToggleButtonGadget,True);
         XtSetSensitive (gridtoggleButtonGadget,True);
         XtSetSensitive (orienPushButtonGadget,True);
         XtSetSensitive (labelPushButtonGadget,True);
      }
      XmUpdateDisplay(widget_axis);
   }

/*
 *      Update the Range widget (Axis Scaling ...)
 */
   if (widget_range) {
      if (histogram.type == HISTO_1D) {
         XtSetSensitive (YRangeFrame,False);
         XtSetSensitive (XScaleRebin,True);
      } else {
         XtSetSensitive (YRangeFrame,True);
         XtSetSensitive (XScaleRebin,False);
      }
      XmUpdateDisplay(widget_range);
   }

/*
 *      Update the Angle widget (Viewing Angles ...)
 */
   if (histogram.type == HISTO_3D || plot_3d) {
      XtSetSensitive(Angles,True);
   } else {
      XtSetSensitive(Angles,False);
      if (widget_angle) XtUnmanageChild (widget_angle);
   }
   XmUpdateDisplay(histoPanel);
}

/***********************************************************************
 *                                                                     *
 *   Update the histo style panel. This routine is called by the       *
 *   fortran routine PMHIST.                                           *
 *                                                                     *
 ***********************************************************************/
#ifdef vms
void fset_histo(int *id, int *type, struct dsc$descriptor_s *path_dsc,
                int *lp)
{
   char *path = path_dsc->dsc$a_pointer;
#else
void fset_histo(int *id, int *type, char *path, int *lp)
{
#endif

   histogram.id = *id;
   strncpy(histogram.dir, path, *lp); histogram.dir[*lp] = '\0';

   switch (*type) {
      case 1:
         histogram.type = HISTO_1D;
         break;
      case 2:
         histogram.type = HISTO_2D;
         break;
      case 3:
         histogram.type = HISTO_3D;
         break;
      default:
         printf("fset_histo: Illegal histogram type.\n");
   }

   set_histo_id(False);
}
